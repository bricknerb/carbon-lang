// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/member_access.h"

#include <optional>

#include "llvm/ADT/STLExtras.h"
#include "toolchain/base/kind_switch.h"
#include "toolchain/check/action.h"
#include "toolchain/check/context.h"
#include "toolchain/check/convert.h"
#include "toolchain/check/eval.h"
#include "toolchain/check/facet_type.h"
#include "toolchain/check/impl_lookup.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/interface.h"
#include "toolchain/check/name_lookup.h"
#include "toolchain/check/type.h"
#include "toolchain/check/type_completion.h"
#include "toolchain/diagnostics/diagnostic_emitter.h"
#include "toolchain/sem_ir/expr_info.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/generic.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/name_scope.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

// Returns the index of the specified class element within the class's
// representation.
static auto GetClassElementIndex(Context& context, SemIR::InstId element_id)
    -> SemIR::ElementIndex {
  auto element_inst = context.insts().Get(element_id);
  if (auto field = element_inst.TryAs<SemIR::FieldDecl>()) {
    return field->index;
  }
  if (auto base = element_inst.TryAs<SemIR::BaseDecl>()) {
    return base->index;
  }
  CARBON_FATAL("Unexpected value {0} in class element name", element_inst);
}

// Returns whether `function_id` is an instance method: in other words, whether
// it has an implicit `self` parameter.
static auto IsInstanceMethod(const SemIR::File& sem_ir,
                             SemIR::FunctionId function_id) -> bool {
  const auto& function = sem_ir.functions().Get(function_id);
  return function.self_param_id.has_value();
}

// For callee functions which are instance methods, returns the `self_id` (which
// may be `None`). This may be an instance method either because it's a Carbon
// instance method or because it's a C++ overload set that might contain an
// instance method.
static auto GetSelfIfInstanceMethod(const SemIR::File& sem_ir,
                                    const SemIR::Callee& callee)
    -> std::optional<SemIR::InstId> {
  CARBON_KIND_SWITCH(callee) {
    case CARBON_KIND(SemIR::CalleeFunction fn): {
      if (IsInstanceMethod(sem_ir, fn.function_id)) {
        return fn.self_id;
      }
      return std::nullopt;
    }
    case CARBON_KIND(SemIR::CalleeCppOverloadSet overload): {
      // For now, treat all C++ overload sets as potentially containing instance
      // methods. Overload resolution will handle the case where we actually
      // found a static method.
      // TODO: Consider returning `None` if there are no non-instance methods
      // in the overload set. This would cause us to reject
      // `instance.(Class.StaticMethod)()` like we do in pure Carbon code.
      return overload.self_id;
    }

    case CARBON_KIND(SemIR::CalleeError _): {
      return std::nullopt;
    }
    case CARBON_KIND(SemIR::CalleeNonFunction _): {
      return std::nullopt;
    }
  }
}

// Return whether `type_id`, the type of an associated entity, is for an
// instance member (currently true only for instance methods).
static auto IsInstanceType(Context& context, SemIR::TypeId type_id) -> bool {
  if (auto function_type =
          context.types().TryGetAs<SemIR::FunctionType>(type_id)) {
    return IsInstanceMethod(context.sem_ir(), function_type->function_id);
  }
  return false;
}

auto GetHighestAllowedAccess(Context& context, SemIR::LocId loc_id,
                             SemIR::ConstantId name_scope_const_id)
    -> SemIR::AccessKind {
  SemIR::ScopeLookupResult lookup_result =
      LookupUnqualifiedName(context, loc_id, SemIR::NameId::SelfType,
                            /*required=*/false)
          .scope_result;
  CARBON_CHECK(!lookup_result.is_poisoned());
  if (!lookup_result.is_found()) {
    return SemIR::AccessKind::Public;
  }

  // TODO: Support other types for `Self`.
  auto self_class_type = context.insts().TryGetAs<SemIR::ClassType>(
      lookup_result.target_inst_id());
  if (!self_class_type) {
    return SemIR::AccessKind::Public;
  }

  auto self_class_info = context.classes().Get(self_class_type->class_id);

  // TODO: Support other types.
  if (auto class_type = context.insts().TryGetAs<SemIR::ClassType>(
          context.constant_values().GetInstId(name_scope_const_id))) {
    auto class_info = context.classes().Get(class_type->class_id);

    if (self_class_info.self_type_id == class_info.self_type_id) {
      return SemIR::AccessKind::Private;
    }

    // If the `type_id` of `Self` does not match with the one we're currently
    // accessing, try checking if this class is of the parent type of `Self`.
    if (auto base_type_id = self_class_info.GetBaseType(
            context.sem_ir(), self_class_type->specific_id);
        base_type_id.has_value()) {
      if (context.types().GetConstantId(base_type_id) == name_scope_const_id) {
        return SemIR::AccessKind::Protected;
      }
      // TODO: Also check whether this base class has a base class of its own.
    } else if (auto adapt_type_id = self_class_info.GetAdaptedType(
                   context.sem_ir(), self_class_type->specific_id);
               adapt_type_id.has_value()) {
      if (context.types().GetConstantId(adapt_type_id) == name_scope_const_id) {
        // TODO: Should we be allowed to access protected fields of a type we
        // are adapting? The design doesn't allow this.
        return SemIR::AccessKind::Protected;
      }
    }
  }

  return SemIR::AccessKind::Public;
}

// Returns whether `scope` is a scope for which impl lookup should be performed
// if we find an associated entity.
static auto ScopeNeedsImplLookup(Context& context,
                                 SemIR::ConstantId name_scope_const_id)
    -> bool {
  SemIR::InstId inst_id =
      context.constant_values().GetInstId(name_scope_const_id);
  CARBON_CHECK(inst_id.has_value());
  SemIR::Inst inst = context.insts().Get(inst_id);

  if (inst.Is<SemIR::FacetType>()) {
    // Don't perform impl lookup if an associated entity is named as a member of
    // a facet type.
    return false;
  }
  if (inst.Is<SemIR::Namespace>()) {
    // Don't perform impl lookup if an associated entity is named as a namespace
    // member.
    // TODO: This case is not yet listed in the design.
    return false;
  }
  // Any other kind of scope is assumed to be a type that implements the
  // interface containing the associated entity, and impl lookup is performed.
  return true;
}

static auto AccessMemberOfImplWitness(Context& context, SemIR::LocId loc_id,
                                      SemIR::TypeId self_type_id,
                                      SemIR::InstId witness_id,
                                      SemIR::SpecificId interface_specific_id,
                                      SemIR::InstId member_id)
    -> SemIR::InstId {
  auto member_value_id = context.constant_values().GetConstantInstId(member_id);
  if (!member_value_id.has_value()) {
    if (member_value_id != SemIR::ErrorInst::InstId) {
      context.TODO(member_id, "non-constant associated entity");
    }
    return SemIR::ErrorInst::InstId;
  }

  auto assoc_entity =
      context.insts().TryGetAs<SemIR::AssociatedEntity>(member_value_id);
  if (!assoc_entity) {
    context.TODO(member_id, "unexpected value for associated entity");
    return SemIR::ErrorInst::InstId;
  }

  // Substitute the interface specific and `Self` type into the type of the
  // associated entity to find the type of the member access.
  LoadImportRef(context, assoc_entity->decl_id);
  auto assoc_type_id = GetTypeForSpecificAssociatedEntity(
      context, loc_id, interface_specific_id, assoc_entity->decl_id,
      self_type_id, witness_id);

  return GetOrAddInst<SemIR::ImplWitnessAccess>(context, loc_id,
                                                {.type_id = assoc_type_id,
                                                 .witness_id = witness_id,
                                                 .index = assoc_entity->index});
}

// For an impl lookup query with a single interface in it, we can convert the
// result to a single witness InstId.
//
// This CHECKs that the result (and thus the query) was a single interface. This
// generally only makes sense in member access, where the lookup query's
// interface is found through name lookup, and we don't have an arbitrary
// `FacetType`.
static auto GetWitnessFromSingleImplLookupResult(
    Context& context, SemIR::InstBlockIdOrError lookup_result)
    -> SemIR::InstId {
  auto witness_id = SemIR::InstId::None;
  if (lookup_result.has_error_value()) {
    witness_id = SemIR::ErrorInst::InstId;
  } else {
    auto witnesses = context.inst_blocks().Get(lookup_result.inst_block_id());
    CARBON_CHECK(witnesses.size() == 1);
    witness_id = witnesses[0];
  }
  return witness_id;
}

// Performs impl lookup for a member name expression. This finds the relevant
// impl witness and extracts the corresponding impl member.
static auto PerformImplLookup(
    Context& context, SemIR::LocId loc_id, SemIR::ConstantId type_const_id,
    SemIR::AssociatedEntityType assoc_type, SemIR::InstId member_id,
    MakeDiagnosticBuilderFn missing_impl_diagnoser = nullptr) -> SemIR::InstId {
  auto self_type_id = context.types().GetTypeIdForTypeConstantId(type_const_id);
  // TODO: Avoid forming and then immediately decomposing a `FacetType` here.
  auto interface_type_id = GetInterfaceType(context, assoc_type.interface_id,
                                            assoc_type.interface_specific_id);
  auto lookup_result = LookupImplWitness(context, loc_id, type_const_id,
                                         interface_type_id.AsConstantId());
  if (!lookup_result.has_value()) {
    if (missing_impl_diagnoser) {
      // TODO: Pass in the expression whose type we are printing.
      CARBON_DIAGNOSTIC(MissingImplInMemberAccessNote, Note,
                        "type {1} does not implement interface {0}",
                        SemIR::TypeId, SemIR::TypeId);
      missing_impl_diagnoser()
          .Note(loc_id, MissingImplInMemberAccessNote, interface_type_id,
                self_type_id)
          .Emit();
    } else {
      // TODO: Pass in the expression whose type we are printing.
      CARBON_DIAGNOSTIC(MissingImplInMemberAccess, Error,
                        "cannot access member of interface {0} in type {1} "
                        "that does not implement that interface",
                        SemIR::TypeId, SemIR::TypeId);
      context.emitter().Emit(loc_id, MissingImplInMemberAccess,
                             interface_type_id, self_type_id);
    }
    return SemIR::ErrorInst::InstId;
  }

  auto witness_id =
      GetWitnessFromSingleImplLookupResult(context, lookup_result);
  return AccessMemberOfImplWitness(context, loc_id, self_type_id, witness_id,
                                   assoc_type.interface_specific_id, member_id);
}

// Performs a member name lookup into the specified scope, including performing
// impl lookup if necessary. If the scope result is `None`, assume an error has
// already been diagnosed, and return `ErrorInst`.
static auto LookupMemberNameInScope(Context& context, SemIR::LocId loc_id,
                                    SemIR::InstId base_id,
                                    SemIR::NameId name_id,
                                    SemIR::ConstantId name_scope_const_id,
                                    llvm::ArrayRef<LookupScope> lookup_scopes,
                                    bool lookup_in_type_of_base, bool required)
    -> SemIR::InstId {
  AccessInfo access_info = {
      .constant_id = name_scope_const_id,
      .highest_allowed_access =
          GetHighestAllowedAccess(context, loc_id, name_scope_const_id),
  };
  LookupResult result = LookupQualifiedName(
      context, loc_id, name_id, lookup_scopes, required, access_info);

  if (!result.scope_result.is_found()) {
    return SemIR::ErrorInst::InstId;
  }

  // TODO: This duplicates the work that HandleNameAsExpr does. Factor this out.
  auto type_id =
      SemIR::GetTypeOfInstInSpecific(context.sem_ir(), result.specific_id,
                                     result.scope_result.target_inst_id());
  CARBON_CHECK(type_id.has_value(), "Missing type for member {0}",
               context.insts().Get(result.scope_result.target_inst_id()));

  // If the named entity has a constant value that depends on its specific,
  // store the specific too.
  if (result.specific_id.has_value() &&
      context.constant_values()
          .Get(result.scope_result.target_inst_id())
          .is_symbolic()) {
    result.scope_result = SemIR::ScopeLookupResult::MakeFound(
        GetOrAddInst<SemIR::SpecificConstant>(
            context, loc_id,
            {.type_id = type_id,
             .inst_id = result.scope_result.target_inst_id(),
             .specific_id = result.specific_id}),
        SemIR::AccessKind::Public);
  }

  // TODO: Use a different kind of instruction that also references the
  // `base_id` so that `SemIR` consumers can find it.
  auto member_id = GetOrAddInst<SemIR::NameRef>(
      context, loc_id,
      {.type_id = type_id,
       .name_id = name_id,
       .value_id = result.scope_result.target_inst_id()});

  // If member name lookup finds an associated entity name, and the scope is not
  // a facet type, perform impl lookup.
  //
  // TODO: We need to do this as part of searching extended scopes, because a
  // lookup that finds an associated entity and also finds the corresponding
  // impl member is not supposed to be treated as ambiguous.
  if (auto assoc_type =
          context.types().TryGetAs<SemIR::AssociatedEntityType>(type_id)) {
    if (lookup_in_type_of_base) {
      auto base_type_id = context.insts().Get(base_id).type_id();

      // When performing access `T.F` on a facet value `T`, convert the facet
      // value `T` itself to a type (`T as type`) to look inside the facet type
      // for a witness. This makes the lookup equivalent to `x.F` where the type
      // of `x` is a facet value `T`.
      if (context.types().Is<SemIR::FacetType>(base_type_id)) {
        base_type_id = ExprAsType(context, loc_id, base_id).type_id;
      }

      member_id = PerformImplLookup(context, loc_id,
                                    context.types().GetConstantId(base_type_id),
                                    *assoc_type, member_id);
    } else if (ScopeNeedsImplLookup(context, name_scope_const_id)) {
      // Handles `T.F` where `T` is a type extending an interface containing
      // `F`.
      member_id = PerformImplLookup(context, loc_id, name_scope_const_id,
                                    *assoc_type, member_id);
    }
  }

  if (!context.rewrites_stack().empty()) {
    if (auto access =
            context.insts().TryGetAs<SemIR::ImplWitnessAccess>(member_id)) {
      if (auto result = context.rewrites_stack().back().Lookup(
              context.constant_values().Get(member_id))) {
        return GetOrAddInst<SemIR::ImplWitnessAccessSubstituted>(
            context, loc_id,
            {.type_id = access->type_id,
             .impl_witness_access_id = member_id,
             .value_id = result.value()});
      }
    }
  }

  return member_id;
}

// Performs the instance binding step in member access. If the found member is a
// field, forms a class member access. If the found member is an instance
// method, forms a bound method. Otherwise, the member is returned unchanged.
static auto PerformInstanceBinding(Context& context, SemIR::LocId loc_id,
                                   SemIR::InstId base_id,
                                   SemIR::InstId member_id) -> SemIR::InstId {
  // If the member is a function, check whether it's an instance method.
  if (auto self_id = GetSelfIfInstanceMethod(
          context.sem_ir(), SemIR::GetCallee(context.sem_ir(), member_id))) {
    if (self_id->has_value()) {
      // Found an already-bound method.
      return member_id;
    }

    return GetOrAddInst<SemIR::BoundMethod>(
        context, loc_id,
        {.type_id =
             GetSingletonType(context, SemIR::BoundMethodType::TypeInstId),
         .object_id = base_id,
         .function_decl_id = member_id});
  }

  // Otherwise, if it's a field, form a class element access.
  if (auto unbound_element_type =
          context.types().TryGetAs<SemIR::UnboundElementType>(
              context.insts().Get(member_id).type_id())) {
    // Convert the base to the type of the element if necessary.
    base_id = ConvertToValueOrRefOfType(
        context, loc_id, base_id,
        context.types().GetTypeIdForTypeInstId(
            unbound_element_type->class_type_inst_id));

    // Find the specified element, which could be either a field or a base
    // class, and build an element access expression.
    auto element_id = context.constant_values().GetConstantInstId(member_id);
    CARBON_CHECK(element_id.has_value(),
                 "Non-constant value {0} of unbound element type",
                 context.insts().Get(member_id));
    auto index = GetClassElementIndex(context, element_id);
    auto access_id = GetOrAddInst<SemIR::ClassElementAccess>(
        context, loc_id,
        {.type_id = context.types().GetTypeIdForTypeInstId(
             unbound_element_type->element_type_inst_id),
         .base_id = base_id,
         .index = index});
    if (SemIR::GetExprCategory(context.sem_ir(), base_id) ==
            SemIR::ExprCategory::Value &&
        SemIR::GetExprCategory(context.sem_ir(), access_id) !=
            SemIR::ExprCategory::Value) {
      // Class element access on a value expression produces an ephemeral
      // reference if the class's value representation is a pointer to the
      // object representation. Add a value binding in that case so that the
      // expression category of the result matches the expression category
      // of the base.
      access_id = ConvertToValueExpr(context, access_id);
    }
    return access_id;
  }

  // Not an instance member: no instance binding.
  return member_id;
}

// Validates that the index (required to be an IntValue) is valid within the
// tuple size. Returns the index on success, or nullptr on failure.
static auto ValidateTupleIndex(Context& context, SemIR::LocId loc_id,
                               SemIR::InstId operand_inst_id,
                               SemIR::IntValue index_inst, int size)
    -> std::optional<llvm::APInt> {
  llvm::APInt index_val = context.ints().Get(index_inst.int_id);
  if (index_val.uge(size)) {
    CARBON_DIAGNOSTIC(TupleIndexOutOfBounds, Error,
                      "tuple element index `{0}` is past the end of type {1}",
                      TypedInt, TypeOfInstId);
    context.emitter().Emit(loc_id, TupleIndexOutOfBounds,
                           {.type = index_inst.type_id, .value = index_val},
                           operand_inst_id);
    return std::nullopt;
  }
  return index_val;
}

auto PerformMemberAccess(Context& context, SemIR::LocId loc_id,
                         SemIR::InstId base_id, SemIR::NameId name_id,
                         bool required) -> SemIR::InstId {
  // TODO: Member access for dependent member names is supposed to perform a
  // lookup in both the template definition context and the template
  // instantiation context, and reject if both succeed but find different
  // things.
  if (required) {
    return HandleAction<SemIR::AccessMemberAction>(
        context, loc_id,
        {.type_id = SemIR::InstType::TypeId,
         .base_id = base_id,
         .name_id = name_id});
  } else {
    return HandleAction<SemIR::AccessOptionalMemberAction>(
        context, loc_id,
        {.type_id = SemIR::InstType::TypeId,
         .base_id = base_id,
         .name_id = name_id});
  }
}

// Common logic for `AccessMemberAction` and `AccessOptionalMemberAction`.
static auto PerformActionHelper(Context& context, SemIR::LocId loc_id,
                                SemIR::InstId base_id, SemIR::NameId name_id,
                                bool required) -> SemIR::InstId {
  // If the base is a name scope, such as a class or namespace, perform lookup
  // into that scope.
  if (auto base_const_id = context.constant_values().Get(base_id);
      base_const_id.is_constant()) {
    llvm::SmallVector<LookupScope> lookup_scopes;
    if (AppendLookupScopesForConstant(context, loc_id, base_const_id,
                                      &lookup_scopes)) {
      return LookupMemberNameInScope(
          context, loc_id, base_id, name_id, base_const_id, lookup_scopes,
          /*lookup_in_type_of_base=*/false, /*required=*/required);
    }
  }

  // If the base isn't a scope, it must have a complete type.
  auto base_type_id = context.insts().Get(base_id).type_id();
  if (!RequireCompleteType(context, base_type_id, SemIR::LocId(base_id), [&] {
        CARBON_DIAGNOSTIC(IncompleteTypeInMemberAccess, Error,
                          "member access into object of incomplete type {0}",
                          TypeOfInstId);
        return context.emitter().Build(base_id, IncompleteTypeInMemberAccess,
                                       base_id);
      })) {
    return SemIR::ErrorInst::InstId;
  }

  // Materialize a temporary for the base expression if necessary.
  base_id = ConvertToValueOrRefExpr(context, base_id);
  base_type_id = context.insts().Get(base_id).type_id();
  auto base_type_const_id = context.types().GetConstantId(base_type_id);

  // Find the scope corresponding to the base type.
  llvm::SmallVector<LookupScope> lookup_scopes;
  if (!AppendLookupScopesForConstant(context, loc_id, base_type_const_id,
                                     &lookup_scopes)) {
    // The base type is not a name scope. Try some fallback options.
    if (auto struct_type = context.insts().TryGetAs<SemIR::StructType>(
            context.constant_values().GetInstId(base_type_const_id))) {
      // TODO: Do we need to optimize this with a lookup table for O(1)?
      for (auto [i, field] : llvm::enumerate(
               context.struct_type_fields().Get(struct_type->fields_id))) {
        if (name_id == field.name_id) {
          // TODO: Model this as producing a lookup result, and do instance
          // binding separately. Perhaps a struct type should be a name scope.
          return GetOrAddInst<SemIR::StructAccess>(
              context, loc_id,
              {.type_id =
                   context.types().GetTypeIdForTypeInstId(field.type_inst_id),
               .struct_id = base_id,
               .index = SemIR::ElementIndex(i)});
        }
      }
      if (required) {
        CARBON_DIAGNOSTIC(QualifiedExprNameNotFound, Error,
                          "type {0} does not have a member `{1}`", TypeOfInstId,
                          SemIR::NameId);
        context.emitter().Emit(loc_id, QualifiedExprNameNotFound, base_id,
                               name_id);
        return SemIR::ErrorInst::InstId;
      } else {
        return SemIR::InstId::None;
      }
    }

    if (base_type_id != SemIR::ErrorInst::TypeId) {
      CARBON_DIAGNOSTIC(QualifiedExprUnsupported, Error,
                        "type {0} does not support qualified expressions",
                        TypeOfInstId);
      context.emitter().Emit(loc_id, QualifiedExprUnsupported, base_id);
    }
    return SemIR::ErrorInst::InstId;
  }

  // Perform lookup into the base type.
  auto member_id = LookupMemberNameInScope(
      context, loc_id, base_id, name_id, base_type_const_id, lookup_scopes,
      /*lookup_in_type_of_base=*/true, /*required=*/required);

  // For name lookup into a facet, never perform instance binding.
  // TODO: According to the design, this should be a "lookup in base" lookup,
  // not a "lookup in type of base" lookup, and the facet itself should have
  // member names that directly name members of the `impl`.
  if (context.types().IsFacetType(base_type_id)) {
    return member_id;
  }

  // Perform instance binding if we found an instance member.
  member_id = PerformInstanceBinding(context, loc_id, base_id, member_id);

  return member_id;
}

auto PerformAction(Context& context, SemIR::LocId loc_id,
                   SemIR::AccessMemberAction action) -> SemIR::InstId {
  return PerformActionHelper(context, loc_id, action.base_id, action.name_id,
                             /*required=*/true);
}

auto PerformAction(Context& context, SemIR::LocId loc_id,
                   SemIR::AccessOptionalMemberAction action) -> SemIR::InstId {
  return PerformActionHelper(context, loc_id, action.base_id, action.name_id,
                             /*required=*/false);
}

// Logic shared by GetAssociatedValue() and PerformCompoundMemberAccess().
static auto GetAssociatedValueImpl(Context& context, SemIR::LocId loc_id,
                                   SemIR::InstId base_id,
                                   const SemIR::AssociatedEntity& assoc_entity,
                                   SemIR::SpecificInterface interface)
    -> SemIR::InstId {
  // Convert to the interface type of the associated member, to get a facet
  // value.
  auto interface_type_id =
      GetInterfaceType(context, interface.interface_id, interface.specific_id);
  auto facet_inst_id =
      ConvertToValueOfType(context, loc_id, base_id, interface_type_id);
  if (facet_inst_id == SemIR::ErrorInst::InstId) {
    return SemIR::ErrorInst::InstId;
  }
  // That facet value has both the self type we need below and the witness
  // we are going to use to look up the value of the associated member.
  auto self_type_const_id = TryEvalInst(
      context, SemIR::FacetAccessType{.type_id = SemIR::TypeType::TypeId,
                                      .facet_value_inst_id = facet_inst_id});
  // TODO: We should be able to lookup constant associated values from runtime
  // facet values by using their FacetType only, but we assume constant values
  // for impl lookup at the moment.
  if (!self_type_const_id.is_constant()) {
    context.TODO(loc_id, "associated value lookup on runtime facet value");
    return SemIR::ErrorInst::InstId;
  }
  auto self_type_id =
      context.types().GetTypeIdForTypeConstantId(self_type_const_id);

  auto lookup_result = LookupImplWitness(
      context, loc_id, context.constant_values().Get(facet_inst_id),
      EvalOrAddInst(context, loc_id,
                    FacetTypeFromInterface(context, interface.interface_id,
                                           interface.specific_id)));
  CARBON_CHECK(lookup_result.has_value());
  auto witness_id =
      GetWitnessFromSingleImplLookupResult(context, lookup_result);

  // Before we can access the element of the witness, we need to figure out
  // the type of that element. It depends on the self type and the specific
  // interface.
  auto assoc_type_id = GetTypeForSpecificAssociatedEntity(
      context, loc_id, interface.specific_id, assoc_entity.decl_id,
      self_type_id, witness_id);
  // Now that we have the witness, an index into it, and the type of the
  // result, return the element of the witness.
  return GetOrAddInst<SemIR::ImplWitnessAccess>(context, loc_id,
                                                {.type_id = assoc_type_id,
                                                 .witness_id = witness_id,
                                                 .index = assoc_entity.index});
}

auto GetAssociatedValue(Context& context, SemIR::LocId loc_id,
                        SemIR::InstId base_id,
                        SemIR::ConstantId assoc_entity_const_id,
                        SemIR::SpecificInterface interface) -> SemIR::InstId {
  // TODO: This function shares a code with PerformCompoundMemberAccess(),
  // it would be nice to reduce the duplication.

  auto value_inst_id =
      context.constant_values().GetInstId(assoc_entity_const_id);
  auto assoc_entity =
      context.insts().GetAs<SemIR::AssociatedEntity>(value_inst_id);
  auto decl_id = assoc_entity.decl_id;
  LoadImportRef(context, decl_id);

  return GetAssociatedValueImpl(context, loc_id, base_id, assoc_entity,
                                interface);
}

auto PerformCompoundMemberAccess(Context& context, SemIR::LocId loc_id,
                                 SemIR::InstId base_id,
                                 SemIR::InstId member_expr_id,
                                 MakeDiagnosticBuilderFn missing_impl_diagnoser)
    -> SemIR::InstId {
  auto base_type_id = context.insts().Get(base_id).type_id();
  auto base_type_const_id = context.types().GetConstantId(base_type_id);

  auto member_id = member_expr_id;
  auto member = context.insts().Get(member_id);

  // If the member expression names an associated entity, impl lookup is always
  // performed using the type of the base expression.
  if (auto assoc_type = context.types().TryGetAs<SemIR::AssociatedEntityType>(
          member.type_id())) {
    // Step 1: figure out the type of the associated entity from the interface.

    auto value_inst_id = context.constant_values().GetConstantInstId(member_id);
    // TODO: According to
    // https://docs.carbon-lang.dev/docs/design/expressions/member_access.html#member-resolution
    // > For a compound member access, the second operand is evaluated as a
    // > compile-time constant to determine the member being accessed. The
    // > evaluation is required to succeed [...]
    if (!value_inst_id.has_value()) {
      context.TODO(loc_id, "Non-constant associated entity value");
      return SemIR::ErrorInst::InstId;
    }
    auto assoc_entity =
        context.insts().GetAs<SemIR::AssociatedEntity>(value_inst_id);
    auto decl_id = assoc_entity.decl_id;
    LoadImportRef(context, decl_id);
    auto decl_value_id = context.constant_values().GetConstantInstId(decl_id);
    auto decl_type_id = context.insts().Get(decl_value_id).type_id();

    if (IsInstanceType(context, decl_type_id)) {
      // Step 2a: For instance methods, lookup the impl of the interface for
      // this type and get the method.
      member_id =
          PerformImplLookup(context, loc_id, base_type_const_id, *assoc_type,
                            member_id, missing_impl_diagnoser);
      // Next we will perform instance binding.
    } else {
      // Step 2b: For non-instance methods and associated constants, we access
      // the value of the associated constant, and don't do any instance
      // binding.
      return GetAssociatedValueImpl(context, loc_id, base_id, assoc_entity,
                                    assoc_type->GetSpecificInterface());
    }
  } else if (context.insts().Is<SemIR::TupleType>(
                 context.constant_values().GetInstId(base_type_const_id))) {
    return PerformTupleAccess(context, loc_id, base_id, member_expr_id);
  }

  // Perform instance binding if we found an instance member.
  member_id = PerformInstanceBinding(context, loc_id, base_id, member_id);

  // If we didn't perform impl lookup or instance binding, that's an error
  // because the base expression is not used for anything.
  if (member_id == member_expr_id &&
      member.type_id() != SemIR::ErrorInst::TypeId) {
    CARBON_DIAGNOSTIC(CompoundMemberAccessDoesNotUseBase, Error,
                      "member name of type {0} in compound member access is "
                      "not an instance member or an interface member",
                      TypeOfInstId);
    context.emitter().Emit(loc_id, CompoundMemberAccessDoesNotUseBase,
                           member_id);
  }

  return member_id;
}

auto PerformTupleAccess(Context& context, SemIR::LocId loc_id,
                        SemIR::InstId tuple_inst_id,
                        SemIR::InstId index_inst_id) -> SemIR::InstId {
  tuple_inst_id = ConvertToValueOrRefExpr(context, tuple_inst_id);
  auto tuple_type_id = context.insts().Get(tuple_inst_id).type_id();

  auto tuple_type = context.types().TryGetAs<SemIR::TupleType>(tuple_type_id);
  if (!tuple_type) {
    CARBON_DIAGNOSTIC(TupleIndexOnANonTupleType, Error,
                      "type {0} does not support tuple indexing; only "
                      "tuples can be indexed that way",
                      TypeOfInstId);
    context.emitter().Emit(loc_id, TupleIndexOnANonTupleType, tuple_inst_id);
    return SemIR::ErrorInst::InstId;
  }

  auto diag_non_constant_index = [&] {
    // TODO: Decide what to do if the index is a symbolic constant.
    CARBON_DIAGNOSTIC(TupleIndexNotConstant, Error,
                      "tuple index must be a constant");
    context.emitter().Emit(loc_id, TupleIndexNotConstant);
    return SemIR::ErrorInst::InstId;
  };
  // Diagnose a non-constant index prior to conversion to IntLiteral, because
  // the conversion will fail if the index is not constant.
  if (!context.constant_values().Get(index_inst_id).is_concrete()) {
    return diag_non_constant_index();
  }

  SemIR::TypeId element_type_id = SemIR::ErrorInst::TypeId;
  index_inst_id = ConvertToValueOfType(
      context, SemIR::LocId(index_inst_id), index_inst_id,
      GetSingletonType(context, SemIR::IntLiteralType::TypeInstId));
  auto index_const_id = context.constant_values().Get(index_inst_id);
  if (index_const_id == SemIR::ErrorInst::ConstantId) {
    return SemIR::ErrorInst::InstId;
  } else if (!index_const_id.is_concrete()) {
    return diag_non_constant_index();
  }

  auto index_literal = context.insts().GetAs<SemIR::IntValue>(
      context.constant_values().GetInstId(index_const_id));
  auto type_block = context.inst_blocks().Get(tuple_type->type_elements_id);
  std::optional<llvm::APInt> index_val = ValidateTupleIndex(
      context, loc_id, tuple_inst_id, index_literal, type_block.size());
  if (!index_val) {
    return SemIR::ErrorInst::InstId;
  }

  // TODO: Handle the case when `index_val->getZExtValue()` has too many bits.
  element_type_id = context.types().GetTypeIdForTypeInstId(
      type_block[index_val->getZExtValue()]);
  auto tuple_index = SemIR::ElementIndex(index_val->getZExtValue());

  return GetOrAddInst<SemIR::TupleAccess>(context, loc_id,
                                          {.type_id = element_type_id,
                                           .tuple_id = tuple_inst_id,
                                           .index = tuple_index});
}

}  // namespace Carbon::Check
