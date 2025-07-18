// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include <optional>
#include <tuple>

#include "toolchain/base/kind_switch.h"
#include "toolchain/check/class.h"
#include "toolchain/check/context.h"
#include "toolchain/check/convert.h"
#include "toolchain/check/decl_name_stack.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/check/eval.h"
#include "toolchain/check/generic.h"
#include "toolchain/check/handle.h"
#include "toolchain/check/import.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/merge.h"
#include "toolchain/check/modifiers.h"
#include "toolchain/check/name_component.h"
#include "toolchain/check/name_lookup.h"
#include "toolchain/check/type.h"
#include "toolchain/check/type_completion.h"
#include "toolchain/parse/node_ids.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

auto HandleParseNode(Context& context, Parse::ClassIntroducerId node_id)
    -> bool {
  // This class is potentially generic.
  StartGenericDecl(context);
  // Create an instruction block to hold the instructions created as part of the
  // class signature, such as generic parameters.
  context.inst_block_stack().Push();
  // Push the bracketing node.
  context.node_stack().Push(node_id);
  // Optional modifiers and the name follow.
  context.decl_introducer_state_stack().Push<Lex::TokenKind::Class>();
  context.decl_name_stack().PushScopeAndStartName();
  return true;
}

// Tries to merge new_class into prev_class_id. Since new_class won't have a
// definition even if one is upcoming, set is_definition to indicate the planned
// result.
//
// If merging is successful, returns true and may update the previous class.
// Otherwise, returns false. Prints a diagnostic when appropriate.
static auto MergeClassRedecl(Context& context, Parse::AnyClassDeclId node_id,
                             SemIR::Class& new_class, bool new_is_definition,
                             SemIR::ClassId prev_class_id,
                             SemIR::ImportIRId prev_import_ir_id) -> bool {
  auto& prev_class = context.classes().Get(prev_class_id);
  SemIR::LocId prev_loc_id(prev_class.latest_decl_id());

  // Check the generic parameters match, if they were specified.
  if (!CheckRedeclParamsMatch(context, DeclParams(new_class),
                              DeclParams(prev_class))) {
    return false;
  }

  DiagnoseIfInvalidRedecl(
      context, Lex::TokenKind::Class, prev_class.name_id,
      RedeclInfo(new_class, node_id, new_is_definition),
      RedeclInfo(prev_class, prev_loc_id, prev_class.has_definition_started()),
      prev_import_ir_id);

  if (new_is_definition && prev_class.has_definition_started()) {
    // Don't attempt to merge multiple definitions.
    return false;
  }

  if (new_is_definition) {
    prev_class.MergeDefinition(new_class);
    prev_class.scope_id = new_class.scope_id;
    prev_class.body_block_id = new_class.body_block_id;
    prev_class.adapt_id = new_class.adapt_id;
    prev_class.base_id = new_class.base_id;
    prev_class.complete_type_witness_id = new_class.complete_type_witness_id;
  }

  if (prev_import_ir_id.has_value() ||
      (prev_class.is_extern && !new_class.is_extern)) {
    prev_class.first_owning_decl_id = new_class.first_owning_decl_id;
    ReplacePrevInstForMerge(context, new_class.parent_scope_id,
                            prev_class.name_id, new_class.first_owning_decl_id);
  }
  return true;
}

// Adds the name to name lookup. If there's a conflict, tries to merge. May
// update class_decl and class_info when merging.
static auto MergeOrAddName(Context& context, Parse::AnyClassDeclId node_id,
                           const DeclNameStack::NameContext& name_context,
                           SemIR::InstId class_decl_id,
                           SemIR::ClassDecl& class_decl,
                           SemIR::Class& class_info, bool is_definition,
                           SemIR::AccessKind access_kind) -> void {
  SemIR::ScopeLookupResult lookup_result =
      context.decl_name_stack().LookupOrAddName(name_context, class_decl_id,
                                                access_kind);
  if (lookup_result.is_poisoned()) {
    // This is a declaration of a poisoned name.
    DiagnosePoisonedName(context, name_context.name_id_for_new_inst(),
                         lookup_result.poisoning_loc_id(), name_context.loc_id);
    return;
  }

  if (!lookup_result.is_found()) {
    return;
  }

  SemIR::InstId prev_id = lookup_result.target_inst_id();

  auto prev_class_id = SemIR::ClassId::None;
  auto prev_import_ir_id = SemIR::ImportIRId::None;
  auto prev = context.insts().Get(prev_id);
  CARBON_KIND_SWITCH(prev) {
    case CARBON_KIND(SemIR::ClassDecl class_decl): {
      prev_class_id = class_decl.class_id;
      break;
    }
    case CARBON_KIND(SemIR::ImportRefLoaded import_ref): {
      auto import_ir_inst =
          context.import_ir_insts().Get(import_ref.import_ir_inst_id);

      // Verify the decl so that things like aliases are name conflicts.
      const auto* import_ir =
          context.import_irs().Get(import_ir_inst.ir_id()).sem_ir;
      if (!import_ir->insts().Is<SemIR::ClassDecl>(import_ir_inst.inst_id())) {
        break;
      }

      // Use the constant value to get the ID.
      auto decl_value = context.insts().Get(
          context.constant_values().GetConstantInstId(prev_id));
      if (auto class_type = decl_value.TryAs<SemIR::ClassType>()) {
        prev_class_id = class_type->class_id;
        prev_import_ir_id = import_ir_inst.ir_id();
      } else if (auto generic_class_type =
                     context.types().TryGetAs<SemIR::GenericClassType>(
                         decl_value.type_id())) {
        prev_class_id = generic_class_type->class_id;
        prev_import_ir_id = import_ir_inst.ir_id();
      }
      break;
    }
    default:
      break;
  }

  if (!prev_class_id.has_value()) {
    // This is a redeclaration of something other than a class.
    DiagnoseDuplicateName(context, name_context.name_id, name_context.loc_id,
                          SemIR::LocId(prev_id));
    return;
  }

  // TODO: Fix `extern` logic. It doesn't work correctly, but doesn't seem worth
  // ripping out because existing code may incrementally help.
  if (MergeClassRedecl(context, node_id, class_info, is_definition,
                       prev_class_id, prev_import_ir_id)) {
    // When merging, use the existing entity rather than adding a new one.
    class_decl.class_id = prev_class_id;
    class_decl.type_id = prev.type_id();
    // TODO: Validate that the redeclaration doesn't set an access modifier.
  }
}

static auto BuildClassDecl(Context& context, Parse::AnyClassDeclId node_id,
                           bool is_definition)
    -> std::tuple<SemIR::ClassId, SemIR::InstId> {
  auto name = PopNameComponent(context);
  auto name_context = context.decl_name_stack().FinishName(name);
  context.node_stack()
      .PopAndDiscardSoloNodeId<Parse::NodeKind::ClassIntroducer>();

  // Process modifiers.
  auto [_, parent_scope_inst] =
      context.name_scopes().GetInstIfValid(name_context.parent_scope_id);
  auto introducer =
      context.decl_introducer_state_stack().Pop<Lex::TokenKind::Class>();
  CheckAccessModifiersOnDecl(context, introducer, parent_scope_inst);
  auto always_acceptable_modifiers =
      KeywordModifierSet::Access | KeywordModifierSet::Extern;
  LimitModifiersOnDecl(context, introducer,
                       always_acceptable_modifiers | KeywordModifierSet::Class);
  if (!is_definition) {
    LimitModifiersOnNotDefinition(context, introducer,
                                  always_acceptable_modifiers);
  }
  RestrictExternModifierOnDecl(context, introducer, parent_scope_inst,
                               is_definition);

  bool is_extern = introducer.modifier_set.HasAnyOf(KeywordModifierSet::Extern);
  if (introducer.extern_library.has_value()) {
    context.TODO(node_id, "extern library");
  }
  auto inheritance_kind =
      introducer.modifier_set.ToEnum<SemIR::Class::InheritanceKind>()
          .Case(KeywordModifierSet::Abstract, SemIR::Class::Abstract)
          .Case(KeywordModifierSet::Base, SemIR::Class::Base)
          .Default(SemIR::Class::Final);

  auto decl_block_id = context.inst_block_stack().Pop();

  // Add the class declaration.
  auto class_decl = SemIR::ClassDecl{.type_id = SemIR::TypeType::TypeId,
                                     .class_id = SemIR::ClassId::None,
                                     .decl_block_id = decl_block_id};
  auto class_decl_id = AddPlaceholderInst(context, node_id, class_decl);

  // TODO: Store state regarding is_extern.
  SemIR::Class class_info = {
      name_context.MakeEntityWithParamsBase(name, class_decl_id, is_extern,
                                            SemIR::LibraryNameId::None),
      {// `.self_type_id` depends on the ClassType, so is set below.
       .self_type_id = SemIR::TypeId::None,
       .inheritance_kind = inheritance_kind}};

  DiagnoseIfGenericMissingExplicitParameters(context, class_info);

  MergeOrAddName(context, node_id, name_context, class_decl_id, class_decl,
                 class_info, is_definition,
                 introducer.modifier_set.GetAccessKind());

  // Create a new class if this isn't a valid redeclaration.
  bool is_new_class = !class_decl.class_id.has_value();
  if (is_new_class) {
    // TODO: If this is an invalid redeclaration of a non-class entity or there
    // was an error in the qualifier, we will have lost track of the class name
    // here. We should keep track of it even if the name is invalid.
    class_info.generic_id = BuildGenericDecl(context, class_decl_id);
    class_decl.class_id = context.classes().Add(class_info);
    if (class_info.has_parameters()) {
      class_decl.type_id = GetGenericClassType(
          context, class_decl.class_id, context.scope_stack().PeekSpecificId());
    }
  } else {
    auto prev_decl_generic_id =
        context.classes().Get(class_decl.class_id).generic_id;
    FinishGenericRedecl(context, prev_decl_generic_id);
  }

  // Write the class ID into the ClassDecl.
  ReplaceInstBeforeConstantUse(context, class_decl_id, class_decl);

  if (is_new_class) {
    // TODO: Form this as part of building the definition, not as part of the
    // declaration.
    SetClassSelfType(context, class_decl.class_id);
  }

  if (!is_definition && context.sem_ir().is_impl() && !is_extern) {
    context.definitions_required_by_decl().push_back(class_decl_id);
  }

  return {class_decl.class_id, class_decl_id};
}

auto HandleParseNode(Context& context, Parse::ClassDeclId node_id) -> bool {
  BuildClassDecl(context, node_id, /*is_definition=*/false);
  context.decl_name_stack().PopScope();
  return true;
}

auto HandleParseNode(Context& context, Parse::ClassDefinitionStartId node_id)
    -> bool {
  auto [class_id, class_decl_id] =
      BuildClassDecl(context, node_id, /*is_definition=*/true);
  auto& class_info = context.classes().Get(class_id);
  StartClassDefinition(context, class_info, class_decl_id);

  // Enter the class scope.
  context.scope_stack().PushForEntity(
      class_decl_id, class_info.scope_id,
      context.generics().GetSelfSpecific(class_info.generic_id));
  StartGenericDefinition(context, class_info.generic_id);

  context.inst_block_stack().Push();
  context.node_stack().Push(node_id, class_id);
  context.field_decls_stack().PushArray();
  context.vtable_stack().Push();

  // TODO: Handle the case where there's control flow in the class body. For
  // example:
  //
  //   class C {
  //     var v: if true then i32 else f64;
  //   }
  //
  // We may need to track a list of instruction blocks here, as we do for a
  // function.
  class_info.body_block_id = context.inst_block_stack().PeekOrAdd();
  return true;
}

// Diagnoses a class-specific declaration appearing outside a class.
static auto DiagnoseClassSpecificDeclOutsideClass(Context& context,
                                                  SemIR::LocId loc_id,
                                                  Lex::TokenKind tok) -> void {
  CARBON_DIAGNOSTIC(ClassSpecificDeclOutsideClass, Error,
                    "`{0}` declaration outside class", Lex::TokenKind);
  context.emitter().Emit(loc_id, ClassSpecificDeclOutsideClass, tok);
}

// Returns the current scope's class declaration, or diagnoses if it isn't a
// class.
static auto GetCurrentScopeAsClassOrDiagnose(Context& context,
                                             SemIR::LocId loc_id,
                                             Lex::TokenKind tok)
    -> std::optional<SemIR::ClassDecl> {
  auto class_scope =
      context.scope_stack().GetCurrentScopeAs<SemIR::ClassDecl>();
  if (!class_scope) {
    DiagnoseClassSpecificDeclOutsideClass(context, loc_id, tok);
  }
  return class_scope;
}

// Diagnoses a class-specific declaration that is repeated within a class, but
// is not permitted to be repeated.
static auto DiagnoseClassSpecificDeclRepeated(Context& context,
                                              SemIR::LocId new_loc_id,
                                              SemIR::LocId prev_loc_id,
                                              Lex::TokenKind tok) -> void {
  CARBON_DIAGNOSTIC(AdaptDeclRepeated, Error,
                    "multiple `adapt` declarations in class");
  CARBON_DIAGNOSTIC(BaseDeclRepeated, Error,
                    "multiple `base` declarations in class; multiple "
                    "inheritance is not permitted");
  CARBON_DIAGNOSTIC(ClassSpecificDeclPrevious, Note,
                    "previous `{0}` declaration is here", Lex::TokenKind);
  CARBON_CHECK(tok == Lex::TokenKind::Adapt || tok == Lex::TokenKind::Base);
  context.emitter()
      .Build(new_loc_id, tok == Lex::TokenKind::Adapt ? AdaptDeclRepeated
                                                      : BaseDeclRepeated)
      .Note(prev_loc_id, ClassSpecificDeclPrevious, tok)
      .Emit();
}

auto HandleParseNode(Context& context, Parse::AdaptIntroducerId /*node_id*/)
    -> bool {
  context.decl_introducer_state_stack().Push<Lex::TokenKind::Adapt>();
  return true;
}

auto HandleParseNode(Context& context, Parse::AdaptDeclId node_id) -> bool {
  auto [adapted_type_node, adapted_type_expr_id] =
      context.node_stack().PopExprWithNodeId();

  // Process modifiers. `extend` is permitted, no others are allowed.
  auto introducer =
      context.decl_introducer_state_stack().Pop<Lex::TokenKind::Adapt>();
  LimitModifiersOnDecl(context, introducer, KeywordModifierSet::Extend);

  auto parent_class_decl =
      GetCurrentScopeAsClassOrDiagnose(context, node_id, Lex::TokenKind::Adapt);
  if (!parent_class_decl) {
    return true;
  }

  auto& class_info = context.classes().Get(parent_class_decl->class_id);
  if (class_info.adapt_id.has_value()) {
    DiagnoseClassSpecificDeclRepeated(context, node_id,
                                      SemIR::LocId(class_info.adapt_id),
                                      Lex::TokenKind::Adapt);
    return true;
  }

  auto [adapted_type_inst_id, adapted_type_id] =
      ExprAsType(context, node_id, adapted_type_expr_id);
  adapted_type_id = AsConcreteType(
      context, adapted_type_id, node_id,
      [&] {
        CARBON_DIAGNOSTIC(IncompleteTypeInAdaptDecl, Error,
                          "adapted type {0} is an incomplete type",
                          InstIdAsType);
        return context.emitter().Build(node_id, IncompleteTypeInAdaptDecl,
                                       adapted_type_inst_id);
      },
      [&] {
        CARBON_DIAGNOSTIC(AbstractTypeInAdaptDecl, Error,
                          "adapted type {0} is an abstract type", InstIdAsType);
        return context.emitter().Build(node_id, AbstractTypeInAdaptDecl,
                                       adapted_type_inst_id);
      });
  if (adapted_type_id == SemIR::ErrorInst::TypeId) {
    adapted_type_inst_id = SemIR::ErrorInst::TypeInstId;
  }

  // Build a SemIR representation for the declaration.
  class_info.adapt_id = AddInst<SemIR::AdaptDecl>(
      context, node_id, {.adapted_type_inst_id = adapted_type_inst_id});

  // Extend the class scope with the adapted type's scope if requested.
  if (introducer.modifier_set.HasAnyOf(KeywordModifierSet::Extend)) {
    auto& class_scope = context.name_scopes().Get(class_info.scope_id);
    class_scope.AddExtendedScope(adapted_type_inst_id);
  }
  return true;
}

auto HandleParseNode(Context& context, Parse::BaseIntroducerId /*node_id*/)
    -> bool {
  context.decl_introducer_state_stack().Push<Lex::TokenKind::Base>();
  return true;
}

auto HandleParseNode(Context& /*context*/, Parse::BaseColonId /*node_id*/)
    -> bool {
  return true;
}

namespace {
// Information gathered about a base type specified in a `base` declaration.
struct BaseInfo {
  // A `BaseInfo` representing an erroneous base.
  static const BaseInfo Error;

  SemIR::TypeId type_id;
  SemIR::NameScopeId scope_id;
  SemIR::TypeInstId inst_id;
};
constexpr BaseInfo BaseInfo::Error = {.type_id = SemIR::ErrorInst::TypeId,
                                      .scope_id = SemIR::NameScopeId::None,
                                      .inst_id = SemIR::ErrorInst::TypeInstId};
}  // namespace

// Diagnoses an attempt to derive from a final type.
static auto DiagnoseBaseIsFinal(Context& context, Parse::NodeId node_id,
                                SemIR::TypeInstId base_type_inst_id) -> void {
  CARBON_DIAGNOSTIC(BaseIsFinal, Error,
                    "deriving from final type {0}; base type must be an "
                    "`abstract` or `base` class",
                    InstIdAsType);
  context.emitter().Emit(node_id, BaseIsFinal, base_type_inst_id);
}

// Checks that the specified base type is valid.
static auto CheckBaseType(Context& context, Parse::NodeId node_id,
                          SemIR::InstId base_expr_id) -> BaseInfo {
  auto [base_type_inst_id, base_type_id] =
      ExprAsType(context, node_id, base_expr_id);
  base_type_id = AsCompleteType(context, base_type_id, node_id, [&] {
    CARBON_DIAGNOSTIC(IncompleteTypeInBaseDecl, Error,
                      "base {0} is an incomplete type", InstIdAsType);
    return context.emitter().Build(node_id, IncompleteTypeInBaseDecl,
                                   base_type_inst_id);
  });

  if (base_type_id == SemIR::ErrorInst::TypeId) {
    return BaseInfo::Error;
  }

  auto class_type = context.types().TryGetAs<SemIR::ClassType>(base_type_id);

  // The base must not be a final class.
  if (!class_type) {
    // For now, we treat all types that aren't introduced by a `class`
    // declaration as being final classes.
    // TODO: Once we have a better idea of which types are considered to be
    // classes, produce a better diagnostic for deriving from a non-class type.
    DiagnoseBaseIsFinal(context, node_id, base_type_inst_id);
    return BaseInfo::Error;
  }

  const auto& base_class_info = context.classes().Get(class_type->class_id);

  if (base_class_info.inheritance_kind == SemIR::Class::Final) {
    DiagnoseBaseIsFinal(context, node_id, base_type_inst_id);
  }

  CARBON_CHECK(base_class_info.scope_id.has_value(),
               "Complete class should have a scope");
  return {.type_id = base_type_id,
          .scope_id = base_class_info.scope_id,
          .inst_id = base_type_inst_id};
}

auto HandleParseNode(Context& context, Parse::BaseDeclId node_id) -> bool {
  auto [base_type_node_id, base_type_expr_id] =
      context.node_stack().PopExprWithNodeId();

  // Process modifiers. `extend` is required, no others are allowed.
  auto introducer =
      context.decl_introducer_state_stack().Pop<Lex::TokenKind::Base>();
  LimitModifiersOnDecl(context, introducer, KeywordModifierSet::Extend);
  if (!introducer.modifier_set.HasAnyOf(KeywordModifierSet::Extend)) {
    CARBON_DIAGNOSTIC(BaseMissingExtend, Error,
                      "missing `extend` before `base` declaration");
    context.emitter().Emit(node_id, BaseMissingExtend);
  }

  auto parent_class_decl =
      GetCurrentScopeAsClassOrDiagnose(context, node_id, Lex::TokenKind::Base);
  if (!parent_class_decl) {
    return true;
  }

  auto& class_info = context.classes().Get(parent_class_decl->class_id);
  if (class_info.base_id.has_value()) {
    DiagnoseClassSpecificDeclRepeated(context, node_id,
                                      SemIR::LocId(class_info.base_id),
                                      Lex::TokenKind::Base);
    return true;
  }

  if (!context.field_decls_stack().PeekArray().empty()) {
    // TODO: Add note that includes the first field location as an example.
    CARBON_DIAGNOSTIC(
        BaseDeclAfterFieldDecl, Error,
        "`base` declaration must appear before field declarations");
    context.emitter().Emit(node_id, BaseDeclAfterFieldDecl);
    return true;
  }

  auto base_info = CheckBaseType(context, base_type_node_id, base_type_expr_id);

  // TODO: Should we diagnose if there are already any fields?

  // The `base` value in the class scope has an unbound element type. Instance
  // binding will be performed when it's found by name lookup into an instance.
  auto field_type_id = GetUnboundElementType(
      context, context.types().GetInstId(class_info.self_type_id),
      base_info.inst_id);
  class_info.base_id =
      AddInst<SemIR::BaseDecl>(context, node_id,
                               {.type_id = field_type_id,
                                .base_type_inst_id = base_info.inst_id,
                                .index = SemIR::ElementIndex::None});

  if (base_info.type_id != SemIR::ErrorInst::TypeId) {
    auto base_class_info = context.classes().Get(
        context.types().GetAs<SemIR::ClassType>(base_info.type_id).class_id);
    class_info.is_dynamic |= base_class_info.is_dynamic;
  }

  // Bind the name `base` in the class to the base field.
  context.decl_name_stack().AddNameOrDiagnose(
      context.decl_name_stack().MakeUnqualifiedName(node_id,
                                                    SemIR::NameId::Base),
      class_info.base_id, introducer.modifier_set.GetAccessKind());

  // Extend the class scope with the base class.
  if (introducer.modifier_set.HasAnyOf(KeywordModifierSet::Extend)) {
    auto& class_scope = context.name_scopes().Get(class_info.scope_id);
    if (base_info.scope_id.has_value()) {
      class_scope.AddExtendedScope(base_info.inst_id);
    } else {
      class_scope.set_has_error();
    }
  }
  return true;
}

auto HandleParseNode(Context& context, Parse::ClassDefinitionId node_id)
    -> bool {
  auto class_id =
      context.node_stack().Pop<Parse::NodeKind::ClassDefinitionStart>();

  // The class type is now fully defined. Compute its object representation.
  ComputeClassObjectRepr(context, node_id, class_id,
                         context.field_decls_stack().PeekArray(),
                         context.vtable_stack().PeekCurrentBlockContents(),
                         context.inst_block_stack().PeekCurrentBlockContents());

  context.inst_block_stack().Pop();
  context.field_decls_stack().PopArray();
  context.vtable_stack().Pop();

  FinishGenericDefinition(context, context.classes().Get(class_id).generic_id);

  // The decl_name_stack and scopes are popped by `ProcessNodeIds`.
  return true;
}

}  // namespace Carbon::Check
