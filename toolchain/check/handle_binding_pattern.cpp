// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/context.h"
#include "toolchain/check/convert.h"
#include "toolchain/check/handle.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/interface.h"
#include "toolchain/check/name_lookup.h"
#include "toolchain/check/pattern.h"
#include "toolchain/check/return.h"
#include "toolchain/check/type.h"
#include "toolchain/check/type_completion.h"
#include "toolchain/diagnostics/format_providers.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/pattern.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

auto HandleParseNode(Context& context, Parse::UnderscoreNameId node_id)
    -> bool {
  context.node_stack().Push(node_id, SemIR::NameId::Underscore);
  return true;
}

// TODO: make this function shorter by factoring pieces out.
static auto HandleAnyBindingPattern(Context& context, Parse::NodeId node_id,
                                    Parse::NodeKind node_kind) -> bool {
  // TODO: split this into smaller, more focused functions.
  auto [type_node, parsed_type_id] = context.node_stack().PopExprWithNodeId();
  auto [cast_type_inst_id, cast_type_id] =
      ExprAsType(context, type_node, parsed_type_id);

  SemIR::ExprRegionId type_expr_region_id =
      EndSubpatternAsExpr(context, cast_type_inst_id);

  // The name in a template binding may be wrapped in `template`.
  bool is_generic = node_kind == Parse::NodeKind::CompileTimeBindingPattern;
  auto is_template =
      context.node_stack()
          .PopAndDiscardSoloNodeIdIf<Parse::NodeKind::TemplateBindingName>();
  // A non-generic template binding is diagnosed by the parser.
  is_template &= is_generic;

  auto [name_node, name_id] = context.node_stack().PopNameWithNodeId();

  const DeclIntroducerState& introducer =
      context.decl_introducer_state_stack().innermost();

  auto make_binding_pattern = [&]() -> SemIR::InstId {
    // TODO: Eventually the name will need to support associations with other
    // scopes, but right now we don't support qualified names here.
    auto binding =
        AddBindingPattern(context, name_node, name_id, cast_type_id,
                          type_expr_region_id, is_generic, is_template);

    if (name_id != SemIR::NameId::Underscore) {
      // Add name to lookup immediately, so it can be used in the rest of the
      // enclosing pattern.
      auto name_context =
          context.decl_name_stack().MakeUnqualifiedName(name_node, name_id);
      context.decl_name_stack().AddNameOrDiagnose(
          name_context, binding.bind_id,
          introducer.modifier_set.GetAccessKind());
      context.full_pattern_stack().AddBindName(name_id);
    }

    return binding.pattern_id;
  };

  // A `self` binding can only appear in an implicit parameter list.
  if (name_id == SemIR::NameId::SelfValue &&
      !context.node_stack().PeekIs(Parse::NodeKind::ImplicitParamListStart)) {
    CARBON_DIAGNOSTIC(
        SelfOutsideImplicitParamList, Error,
        "`self` can only be declared in an implicit parameter list");
    context.emitter().Emit(node_id, SelfOutsideImplicitParamList);
  }

  // A binding in an interface scope declares an associated constant, not a
  // true binding, so we handle it separately.
  if (auto parent_interface_decl =
          context.scope_stack().GetCurrentScopeAs<SemIR::InterfaceDecl>();
      parent_interface_decl.has_value()) {
    // TODO: diagnose this during parsing, to avoid near-duplicate error
    // messages.
    if (!is_generic) {
      CARBON_DIAGNOSTIC(ExpectedSymbolicBindingInAssociatedConstant, Error,
                        "found runtime binding pattern in associated constant "
                        "declaration; expected a `:!` binding");
      context.emitter().Emit(node_id,
                             ExpectedSymbolicBindingInAssociatedConstant);
      context.node_stack().Push(node_id, SemIR::ErrorInst::InstId);
      return true;
    }
    if (name_id == SemIR::NameId::Underscore) {
      // The action item here may be to document this as not allowed, and
      // add a proper diagnostic.
      context.TODO(node_id, "_ used as associated constant name");
    }
    cast_type_id = AsCompleteType(context, cast_type_id, type_node, [&] {
      CARBON_DIAGNOSTIC(IncompleteTypeInAssociatedConstantDecl, Error,
                        "associated constant has incomplete type {0}",
                        SemIR::TypeId);
      return context.emitter().Build(
          type_node, IncompleteTypeInAssociatedConstantDecl, cast_type_id);
    });
    if (is_template) {
      CARBON_DIAGNOSTIC(TemplateBindingInAssociatedConstantDecl, Error,
                        "associated constant has `template` binding");
      context.emitter().Emit(type_node,
                             TemplateBindingInAssociatedConstantDecl);
    }

    SemIR::AssociatedConstantDecl assoc_const_decl = {
        .type_id = cast_type_id,
        .assoc_const_id = SemIR::AssociatedConstantId::None,
        .decl_block_id = SemIR::InstBlockId::None};
    auto decl_id = AddPlaceholderInstInNoBlock(
        context,
        context.parse_tree().As<Parse::CompileTimeBindingPatternId>(node_id),
        assoc_const_decl);
    assoc_const_decl.assoc_const_id = context.associated_constants().Add(
        {.name_id = name_id,
         .parent_scope_id = context.scope_stack().PeekNameScopeId(),
         .decl_id = decl_id,
         .generic_id = SemIR::GenericId::None,
         .default_value_id = SemIR::InstId::None});
    ReplaceInstBeforeConstantUse(context, decl_id, assoc_const_decl);

    context.node_stack().Push(node_id, decl_id);
    return true;
  }

  // Allocate an instruction of the appropriate kind, linked to the name for
  // error locations.
  switch (context.full_pattern_stack().CurrentKind()) {
    case FullPatternStack::Kind::ImplicitParamList:
    case FullPatternStack::Kind::ExplicitParamList: {
      // Parameters can have incomplete types in a function declaration, but not
      // in a function definition. We don't know which kind we have here.
      bool had_error = false;
      switch (introducer.kind) {
        case Lex::TokenKind::Fn: {
          if (context.full_pattern_stack().CurrentKind() ==
                  FullPatternStack::Kind::ImplicitParamList &&
              !(is_generic || name_id == SemIR::NameId::SelfValue)) {
            CARBON_DIAGNOSTIC(
                ImplictParamMustBeConstant, Error,
                "implicit parameters of functions must be constant or `self`");
            context.emitter().Emit(node_id, ImplictParamMustBeConstant);
            had_error = true;
          }
          break;
        }
        case Lex::TokenKind::Choice:
          if (context.scope_stack().PeekInstId().has_value()) {
            // We are building a pattern for a choice alternative, not the
            // choice type itself.

            // Implicit param lists are prevented during parse.
            CARBON_CHECK(context.full_pattern_stack().CurrentKind() !=
                             FullPatternStack::Kind::ImplicitParamList,
                         "choice alternative with implicit parameters");
            // Don't fall through to the `Class` logic for choice alternatives.
            break;
          }
          [[fallthrough]];
        case Lex::TokenKind::Class:
        case Lex::TokenKind::Impl:
        case Lex::TokenKind::Interface: {
          if (name_id == SemIR::NameId::SelfValue) {
            CARBON_DIAGNOSTIC(SelfParameterNotAllowed, Error,
                              "`self` parameter only allowed on functions");
            context.emitter().Emit(node_id, SelfParameterNotAllowed);
            had_error = true;
          } else if (!is_generic) {
            CARBON_DIAGNOSTIC(GenericParamMustBeConstant, Error,
                              "parameters of generic types must be constant");
            context.emitter().Emit(node_id, GenericParamMustBeConstant);
            had_error = true;
          }
          break;
        }
        default:
          break;
      }
      auto result_inst_id = SemIR::InstId::None;
      if (had_error) {
        if (name_id != SemIR::NameId::Underscore) {
          AddNameToLookup(context, name_id, SemIR::ErrorInst::InstId);
        }
        // Replace the parameter with `ErrorInst` so that we don't try
        // constructing a generic based on it.
        result_inst_id = SemIR::ErrorInst::InstId;
      } else {
        result_inst_id = make_binding_pattern();
        if (node_kind == Parse::NodeKind::LetBindingPattern) {
          // A value binding pattern in a function signature is a `Call`
          // parameter, but a variable binding pattern is not (instead the
          // enclosing `var` pattern is), and a symbolic binding pattern is not
          // (because it's not passed to the `Call` inst).
          result_inst_id = AddPatternInst<SemIR::ValueParamPattern>(
              context, node_id,
              {.type_id = context.insts().Get(result_inst_id).type_id(),
               .subpattern_id = result_inst_id,
               .index = SemIR::CallParamIndex::None});
        }
      }
      context.node_stack().Push(node_id, result_inst_id);
      break;
    }

    case FullPatternStack::Kind::NameBindingDecl: {
      auto incomplete_diagnoser = [&] {
        CARBON_DIAGNOSTIC(IncompleteTypeInBindingDecl, Error,
                          "binding pattern has incomplete type {0} in name "
                          "binding declaration",
                          InstIdAsType);
        return context.emitter().Build(type_node, IncompleteTypeInBindingDecl,
                                       cast_type_inst_id);
      };
      if (node_kind == Parse::NodeKind::VarBindingPattern) {
        cast_type_id = AsConcreteType(
            context, cast_type_id, type_node, incomplete_diagnoser, [&] {
              CARBON_DIAGNOSTIC(
                  AbstractTypeInVarPattern, Error,
                  "binding pattern has abstract type {0} in `var` "
                  "pattern",
                  SemIR::TypeId);
              return context.emitter().Build(
                  type_node, AbstractTypeInVarPattern, cast_type_id);
            });
      } else {
        cast_type_id = AsCompleteType(context, cast_type_id, type_node,
                                      incomplete_diagnoser);
      }
      auto binding_pattern_id = make_binding_pattern();
      if (node_kind == Parse::NodeKind::VarBindingPattern) {
        CARBON_CHECK(!is_generic);

        if (introducer.modifier_set.HasAnyOf(KeywordModifierSet::Returned)) {
          // TODO: Should we check this for the `var` as a whole, rather than
          // for the name binding?
          auto bind_id = context.bind_name_map()
                             .Lookup(binding_pattern_id)
                             .value()
                             .bind_name_id;
          RegisterReturnedVar(context,
                              introducer.modifier_node_id(ModifierOrder::Decl),
                              type_node, cast_type_id, bind_id);
        }
      }
      context.node_stack().Push(node_id, binding_pattern_id);
      break;
    }
  }
  return true;
}

auto HandleParseNode(Context& context, Parse::LetBindingPatternId node_id)
    -> bool {
  return HandleAnyBindingPattern(context, node_id,
                                 Parse::NodeKind::LetBindingPattern);
}

auto HandleParseNode(Context& context, Parse::VarBindingPatternId node_id)
    -> bool {
  return HandleAnyBindingPattern(context, node_id,
                                 Parse::NodeKind::VarBindingPattern);
}

auto HandleParseNode(Context& context,
                     Parse::CompileTimeBindingPatternId node_id) -> bool {
  auto node_kind = Parse::NodeKind::CompileTimeBindingPattern;
  if (context.decl_introducer_state_stack().innermost().kind ==
      Lex::TokenKind::Let) {
    // Disallow `let` outside of function and interface definitions.
    // TODO: Find a less brittle way of doing this. A `scope_inst_id` of `None`
    // can represent a block scope, but is also used for other kinds of scopes
    // that aren't necessarily part of an interface or function decl.
    auto scope_inst_id = context.scope_stack().PeekInstId();
    if (scope_inst_id.has_value()) {
      auto scope_inst = context.insts().Get(scope_inst_id);
      if (!scope_inst.Is<SemIR::InterfaceDecl>() &&
          !scope_inst.Is<SemIR::FunctionDecl>()) {
        context.TODO(
            node_id,
            "`let` compile time binding outside function or interface");
        node_kind = Parse::NodeKind::LetBindingPattern;
      }
    }
  }

  return HandleAnyBindingPattern(context, node_id, node_kind);
}

auto HandleParseNode(Context& context, Parse::FieldNameAndTypeId node_id)
    -> bool {
  auto [type_node, parsed_type_id] = context.node_stack().PopExprWithNodeId();
  auto [cast_type_inst_id, cast_type_id] =
      ExprAsType(context, type_node, parsed_type_id);
  auto [name_node, name_id] = context.node_stack().PopNameWithNodeId();

  auto parent_class_decl =
      context.scope_stack().GetCurrentScopeAs<SemIR::ClassDecl>();
  CARBON_CHECK(parent_class_decl);
  cast_type_id = AsConcreteType(
      context, cast_type_id, type_node,
      [&] {
        CARBON_DIAGNOSTIC(IncompleteTypeInFieldDecl, Error,
                          "field has incomplete type {0}", SemIR::TypeId);
        return context.emitter().Build(type_node, IncompleteTypeInFieldDecl,
                                       cast_type_id);
      },
      [&] {
        CARBON_DIAGNOSTIC(AbstractTypeInFieldDecl, Error,
                          "field has abstract type {0}", SemIR::TypeId);
        return context.emitter().Build(type_node, AbstractTypeInFieldDecl,
                                       cast_type_id);
      });
  if (cast_type_id == SemIR::ErrorInst::TypeId) {
    cast_type_inst_id = SemIR::ErrorInst::TypeInstId;
  }
  auto& class_info = context.classes().Get(parent_class_decl->class_id);
  auto field_type_id = GetUnboundElementType(
      context, context.types().GetInstId(class_info.self_type_id),
      cast_type_inst_id);
  auto field_id =
      AddInst<SemIR::FieldDecl>(context, node_id,
                                {.type_id = field_type_id,
                                 .name_id = name_id,
                                 .index = SemIR::ElementIndex::None});
  context.field_decls_stack().AppendToTop(field_id);

  auto name_context =
      context.decl_name_stack().MakeUnqualifiedName(node_id, name_id);
  context.decl_name_stack().AddNameOrDiagnose(
      name_context, field_id,
      context.decl_introducer_state_stack()
          .innermost()
          .modifier_set.GetAccessKind());
  return true;
}

auto HandleParseNode(Context& context, Parse::AddrId node_id) -> bool {
  auto param_pattern_id = context.node_stack().PopPattern();
  if (SemIR::IsSelfPattern(context.sem_ir(), param_pattern_id)) {
    auto param_type_id = ExtractScrutineeType(
        context.sem_ir(), context.insts().Get(param_pattern_id).type_id());
    auto pointer_type =
        context.types().TryGetAs<SemIR::PointerType>(param_type_id);
    if (pointer_type) {
      auto addr_pattern_id = AddPatternInst<SemIR::AddrPattern>(
          context, node_id,
          {.type_id = GetPatternType(context, SemIR::AutoType::TypeId),
           .inner_id = param_pattern_id});
      context.node_stack().Push(node_id, addr_pattern_id);
    } else {
      CARBON_DIAGNOSTIC(
          AddrOnNonPointerType, Error,
          "`addr` can only be applied to a binding with a pointer type");
      context.emitter().Emit(node_id, AddrOnNonPointerType);
      context.node_stack().Push(node_id, param_pattern_id);
    }
  } else {
    CARBON_DIAGNOSTIC(AddrOnNonSelfParam, Error,
                      "`addr` can only be applied to a `self` parameter");
    context.emitter().Emit(LocIdForDiagnostics::TokenOnly(node_id),
                           AddrOnNonSelfParam);
    context.node_stack().Push(node_id, param_pattern_id);
  }
  return true;
}

auto HandleParseNode(Context& context, Parse::TemplateBindingNameId node_id)
    -> bool {
  context.node_stack().Push(node_id);
  return true;
}

}  // namespace Carbon::Check
