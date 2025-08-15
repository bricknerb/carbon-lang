// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/class.h"

#include "toolchain/check/context.h"
#include "toolchain/check/convert.h"
#include "toolchain/check/eval.h"
#include "toolchain/check/function.h"
#include "toolchain/check/generic.h"
#include "toolchain/check/impl.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/name_lookup.h"
#include "toolchain/check/name_ref.h"
#include "toolchain/check/pattern.h"
#include "toolchain/check/pattern_match.h"
#include "toolchain/check/type.h"
#include "toolchain/parse/node_ids.h"
#include "toolchain/sem_ir/builtin_function_kind.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

auto SetClassSelfType(Context& context, SemIR::ClassId class_id) -> void {
  auto& class_info = context.classes().Get(class_id);
  auto specific_id = context.generics().GetSelfSpecific(class_info.generic_id);
  class_info.self_type_id = GetClassType(context, class_id, specific_id);
}

auto StartClassDefinition(Context& context, SemIR::Class& class_info,
                          SemIR::InstId definition_id) -> void {
  // Track that this declaration is the definition.
  CARBON_CHECK(!class_info.has_definition_started());
  class_info.definition_id = definition_id;
  class_info.scope_id = context.name_scopes().Add(
      definition_id, SemIR::NameId::None, class_info.parent_scope_id);

  // Introduce `Self`.
  context.name_scopes().AddRequiredName(
      class_info.scope_id, SemIR::NameId::SelfType,
      context.types().GetInstId(class_info.self_type_id));
}

// Checks that the specified finished adapter definition is valid and builds and
// returns a corresponding complete type witness instruction.
static auto CheckCompleteAdapterClassType(
    Context& context, Parse::NodeId node_id, SemIR::ClassId class_id,
    llvm::ArrayRef<SemIR::InstId> field_decls,
    llvm::ArrayRef<SemIR::InstId> body) -> SemIR::InstId {
  const auto& class_info = context.classes().Get(class_id);
  if (class_info.base_id.has_value()) {
    CARBON_DIAGNOSTIC(AdaptWithBase, Error, "adapter with base class");
    CARBON_DIAGNOSTIC(AdaptWithBaseHere, Note, "`base` declaration is here");
    context.emitter()
        .Build(class_info.adapt_id, AdaptWithBase)
        .Note(class_info.base_id, AdaptWithBaseHere)
        .Emit();
    return SemIR::ErrorInst::InstId;
  }

  if (!field_decls.empty()) {
    CARBON_DIAGNOSTIC(AdaptWithFields, Error, "adapter with fields");
    CARBON_DIAGNOSTIC(AdaptWithFieldHere, Note,
                      "first field declaration is here");
    context.emitter()
        .Build(class_info.adapt_id, AdaptWithFields)
        .Note(field_decls.front(), AdaptWithFieldHere)
        .Emit();
    return SemIR::ErrorInst::InstId;
  }

  for (auto inst_id : body) {
    if (auto function_decl =
            context.insts().TryGetAs<SemIR::FunctionDecl>(inst_id)) {
      auto& function = context.functions().Get(function_decl->function_id);
      if (function.virtual_modifier ==
          SemIR::Function::VirtualModifier::Virtual) {
        CARBON_DIAGNOSTIC(AdaptWithVirtual, Error,
                          "adapter with virtual function");
        CARBON_DIAGNOSTIC(AdaptWithVirtualHere, Note,
                          "first virtual function declaration is here");
        context.emitter()
            .Build(class_info.adapt_id, AdaptWithVirtual)
            .Note(inst_id, AdaptWithVirtualHere)
            .Emit();
        return SemIR::ErrorInst::InstId;
      }
    }
  }

  // The object representation of the adapter is the object representation
  // of the adapted type.
  auto adapted_type_id =
      class_info.GetAdaptedType(context.sem_ir(), SemIR::SpecificId::None);
  auto object_repr_id = context.types().GetObjectRepr(adapted_type_id);

  return AddInst<SemIR::CompleteTypeWitness>(
      context, node_id,
      {.type_id = GetSingletonType(context, SemIR::WitnessType::TypeInstId),
       // TODO: Use InstId from the adapt declaration.
       .object_repr_type_inst_id = context.types().GetInstId(object_repr_id)});
}

static auto AddStructTypeFields(
    Context& context,
    llvm::SmallVector<SemIR::StructTypeField>& struct_type_fields,
    llvm::ArrayRef<SemIR::InstId> field_decls) -> SemIR::StructTypeFieldsId {
  for (auto field_decl_id : field_decls) {
    auto field_decl = context.insts().GetAs<SemIR::FieldDecl>(field_decl_id);
    field_decl.index =
        SemIR::ElementIndex{static_cast<int>(struct_type_fields.size())};
    ReplaceInstPreservingConstantValue(context, field_decl_id, field_decl);
    if (field_decl.type_id == SemIR::ErrorInst::TypeId) {
      struct_type_fields.push_back(
          {.name_id = field_decl.name_id,
           .type_inst_id = SemIR::ErrorInst::TypeInstId});
      continue;
    }
    auto unbound_element_type =
        context.sem_ir().types().GetAs<SemIR::UnboundElementType>(
            field_decl.type_id);
    struct_type_fields.push_back(
        {.name_id = field_decl.name_id,
         .type_inst_id = unbound_element_type.element_type_inst_id});
  }
  auto fields_id =
      context.struct_type_fields().AddCanonical(struct_type_fields);
  return fields_id;
}

// Builds and returns a vtable for the current class. Assumes that the virtual
// functions for the class are listed as the top element of the `vtable_stack`.
static auto BuildVtable(Context& context, Parse::ClassDefinitionId node_id,
                        SemIR::ClassId class_id,
                        std::optional<SemIR::ClassType> base_class_type,
                        llvm::ArrayRef<SemIR::InstId> vtable_contents)
    -> SemIR::VtableId {
  auto base_vtable_id = SemIR::VtableId::None;
  auto base_class_specific_id = SemIR::SpecificId::None;

  // Get some base class/type/specific info.
  if (base_class_type) {
    auto& base_class_info = context.classes().Get(base_class_type->class_id);
    auto base_vtable_decl_inst_id = base_class_info.vtable_decl_id;
    if (base_vtable_decl_inst_id.has_value()) {
      LoadImportRef(context, base_vtable_decl_inst_id);
      auto canonical_base_vtable_inst_id =
          context.constant_values().GetConstantInstId(base_vtable_decl_inst_id);
      const auto& base_vtable_decl_inst =
          context.insts().GetAs<SemIR::VtableDecl>(
              canonical_base_vtable_inst_id);
      base_vtable_id = base_vtable_decl_inst.vtable_id;
      base_class_specific_id = base_class_type->specific_id;
    }
  }

  const auto& class_info = context.classes().Get(class_id);
  auto class_generic_id = class_info.generic_id;

  // Wrap vtable entries in SpecificFunctions as needed/in generic classes.
  auto build_specific_function =
      [&](SemIR::InstId fn_decl_id) -> SemIR::InstId {
    if (!class_generic_id.has_value()) {
      return fn_decl_id;
    }
    const auto& fn_decl =
        context.insts().GetAs<SemIR::FunctionDecl>(fn_decl_id);
    const auto& function = context.functions().Get(fn_decl.function_id);
    return GetOrAddInst<SemIR::SpecificFunction>(
        context, node_id,
        {.type_id =
             GetSingletonType(context, SemIR::SpecificFunctionType::TypeInstId),
         .callee_id = fn_decl_id,
         .specific_id =
             context.generics().GetSelfSpecific(function.generic_id)});
  };

  llvm::SmallVector<SemIR::InstId> vtable;
  Set<SemIR::FunctionId> implemented_impls;
  if (base_vtable_id.has_value()) {
    auto base_vtable_inst_block = context.inst_blocks().Get(
        context.vtables().Get(base_vtable_id).virtual_functions_id);
    // TODO: Avoid quadratic search. Perhaps build a map from `NameId` to the
    // elements of the top of `vtable_stack`.
    for (auto base_vtable_entry_id : base_vtable_inst_block) {
      auto [derived_vtable_entry_id, derived_vtable_entry_const_id, fn_id,
            specific_id] =
          DecomposeVirtualFunction(context.sem_ir(), base_vtable_entry_id,
                                   base_class_specific_id);
      const auto& fn = context.sem_ir().functions().Get(fn_id);
      const auto* i = llvm::find_if(
          vtable_contents, [&](SemIR::InstId override_fn_decl_id) -> bool {
            const auto& override_fn = context.functions().Get(
                context.insts()
                    .GetAs<SemIR::FunctionDecl>(override_fn_decl_id)
                    .function_id);
            return override_fn.virtual_modifier ==
                       SemIR::FunctionFields::VirtualModifier::Impl &&
                   override_fn.name_id == fn.name_id;
          });
      if (i != vtable_contents.end()) {
        auto override_fn_id =
            context.insts().GetAs<SemIR::FunctionDecl>(*i).function_id;
        implemented_impls.Insert(override_fn_id);
        auto& override_fn = context.functions().Get(override_fn_id);
        CheckFunctionTypeMatches(context, override_fn, fn, specific_id,
                                 /*check_syntax=*/false,
                                 /*check_self=*/false);
        derived_vtable_entry_id = build_specific_function(*i);
        override_fn.virtual_index = vtable.size();
        CARBON_CHECK(override_fn.virtual_index == fn.virtual_index);
      } else if (auto base_vtable_specific_function =
                     context.sem_ir().insts().TryGetAs<SemIR::SpecificFunction>(
                         derived_vtable_entry_id)) {
        if (derived_vtable_entry_const_id.is_symbolic()) {
          // Create a new instruction here that is otherwise identical to
          // `derived_vtable_entry_id` but is dependent within the derived
          // class. This ensures we can `GetConstantValueInSpecific` for it
          // with the derived class's specific (when forming further derived
          // classes, lowering the vtable, etc).
          derived_vtable_entry_id = GetOrAddInst<SemIR::SpecificFunction>(
              context, node_id,
              {.type_id = GetSingletonType(
                   context, SemIR::SpecificFunctionType::TypeInstId),
               .callee_id = base_vtable_specific_function->callee_id,
               .specific_id = base_vtable_specific_function->specific_id});
        }
      }
      vtable.push_back(derived_vtable_entry_id);
    }
  }

  for (auto inst_id : vtable_contents) {
    auto fn_decl = context.insts().GetAs<SemIR::FunctionDecl>(inst_id);
    auto& fn = context.functions().Get(fn_decl.function_id);
    if (fn.virtual_modifier != SemIR::FunctionFields::VirtualModifier::Impl) {
      fn.virtual_index = vtable.size();
      vtable.push_back(build_specific_function(inst_id));
    } else if (!implemented_impls.Lookup(fn_decl.function_id)) {
      CARBON_DIAGNOSTIC(ImplWithoutVirtualInBase, Error,
                        "impl without compatible virtual in base class");
      context.emitter().Emit(SemIR::LocId(inst_id), ImplWithoutVirtualInBase);
    }
  }

  return context.vtables().Add(
      {{.class_id = class_id,
        .virtual_functions_id = context.inst_blocks().Add(vtable)}});
}

// Checks that the specified finished class definition is valid and builds and
// returns a corresponding complete type witness instruction.
static auto CheckCompleteClassType(
    Context& context, Parse::ClassDefinitionId node_id, SemIR::ClassId class_id,
    llvm::ArrayRef<SemIR::InstId> field_decls,
    llvm::ArrayRef<SemIR::InstId> vtable_contents,
    llvm::ArrayRef<SemIR::InstId> body) -> SemIR::InstId {
  auto& class_info = context.classes().Get(class_id);
  if (class_info.adapt_id.has_value()) {
    return CheckCompleteAdapterClassType(context, node_id, class_id,
                                         field_decls, body);
  }

  bool defining_vptr = class_info.is_dynamic;
  auto base_type_id =
      class_info.GetBaseType(context.sem_ir(), SemIR::SpecificId::None);
  // TODO: Use InstId from base declaration.
  auto base_type_inst_id = context.types().GetInstId(base_type_id);
  std::optional<SemIR::ClassType> base_class_type;
  if (base_type_id.has_value()) {
    // TODO: If the base class is template dependent, we will need to decide
    // whether to add a vptr as part of instantiation.
    base_class_type = context.types().TryGetAs<SemIR::ClassType>(base_type_id);
    if (base_class_type &&
        context.classes().Get(base_class_type->class_id).is_dynamic) {
      defining_vptr = false;
    }
  }

  llvm::SmallVector<SemIR::StructTypeField> struct_type_fields;
  struct_type_fields.reserve(defining_vptr + class_info.base_id.has_value() +
                             field_decls.size());
  if (defining_vptr) {
    struct_type_fields.push_back(
        {.name_id = SemIR::NameId::Vptr,
         .type_inst_id = context.types().GetInstId(
             GetPointerType(context, SemIR::VtableType::TypeInstId))});
  }
  if (base_type_id.has_value()) {
    auto base_decl = context.insts().GetAs<SemIR::BaseDecl>(class_info.base_id);
    base_decl.index =
        SemIR::ElementIndex{static_cast<int>(struct_type_fields.size())};
    ReplaceInstPreservingConstantValue(context, class_info.base_id, base_decl);
    struct_type_fields.push_back(
        {.name_id = SemIR::NameId::Base, .type_inst_id = base_type_inst_id});
  }

  if (class_info.is_dynamic) {
    auto vtable_id = BuildVtable(context, node_id, class_id, base_class_type,
                                 vtable_contents);
    auto vptr_type_id = GetPointerType(context, SemIR::VtableType::TypeInstId);
    class_info.vtable_decl_id = AddInst<SemIR::VtableDecl>(
        context, node_id, {.type_id = vptr_type_id, .vtable_id = vtable_id});
  }

  auto struct_type_id = GetStructType(
      context, AddStructTypeFields(context, struct_type_fields, field_decls));

  return AddInst<SemIR::CompleteTypeWitness>(
      context, node_id,
      {.type_id = GetSingletonType(context, SemIR::WitnessType::TypeInstId),
       .object_repr_type_inst_id = context.types().GetInstId(struct_type_id)});
}

auto ComputeClassObjectRepr(Context& context, Parse::ClassDefinitionId node_id,
                            SemIR::ClassId class_id,
                            llvm::ArrayRef<SemIR::InstId> field_decls,
                            llvm::ArrayRef<SemIR::InstId> vtable_contents,
                            llvm::ArrayRef<SemIR::InstId> body) -> void {
  auto complete_type_witness_id = CheckCompleteClassType(
      context, node_id, class_id, field_decls, vtable_contents, body);
  auto& class_info = context.classes().Get(class_id);
  class_info.complete_type_witness_id = complete_type_witness_id;
}

}  // namespace Carbon::Check
