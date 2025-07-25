// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/type.h"

#include "toolchain/check/eval.h"
#include "toolchain/check/facet_type.h"
#include "toolchain/check/type_completion.h"

namespace Carbon::Check {

// Enforces that an integer type has a valid bit width.
auto ValidateIntType(Context& context, SemIR::LocId loc_id,
                     SemIR::IntType result) -> bool {
  auto bit_width =
      context.insts().TryGetAs<SemIR::IntValue>(result.bit_width_id);
  if (!bit_width) {
    // Symbolic bit width.
    return true;
  }
  const auto& bit_width_val = context.ints().Get(bit_width->int_id);
  if (bit_width_val.isZero() ||
      (context.types().IsSignedInt(bit_width->type_id) &&
       bit_width_val.isNegative())) {
    CARBON_DIAGNOSTIC(IntWidthNotPositive, Error,
                      "integer type width of {0} is not positive", TypedInt);
    context.emitter().Emit(
        loc_id, IntWidthNotPositive,
        {.type = bit_width->type_id, .value = bit_width_val});
    return false;
  }
  if (bit_width_val.ugt(IntStore::MaxIntWidth)) {
    CARBON_DIAGNOSTIC(IntWidthTooLarge, Error,
                      "integer type width of {0} is greater than the "
                      "maximum supported width of {1}",
                      TypedInt, int);
    context.emitter().Emit(loc_id, IntWidthTooLarge,
                           {.type = bit_width->type_id, .value = bit_width_val},
                           IntStore::MaxIntWidth);
    return false;
  }
  return true;
}

// Enforces that the bit width is 64 for a float.
auto ValidateFloatBitWidth(Context& context, SemIR::LocId loc_id,
                           SemIR::InstId inst_id) -> bool {
  auto inst = context.insts().GetAs<SemIR::IntValue>(inst_id);
  if (context.ints().Get(inst.int_id) == 64) {
    return true;
  }

  CARBON_DIAGNOSTIC(CompileTimeFloatBitWidth, Error, "bit width must be 64");
  context.emitter().Emit(loc_id, CompileTimeFloatBitWidth);
  return false;
}

// Enforces that a float type has a valid bit width.
auto ValidateFloatType(Context& context, SemIR::LocId loc_id,
                       SemIR::FloatType result) -> bool {
  auto bit_width =
      context.insts().TryGetAs<SemIR::IntValue>(result.bit_width_id);
  if (!bit_width) {
    // Symbolic bit width.
    return true;
  }
  return ValidateFloatBitWidth(context, loc_id, result.bit_width_id);
}

// Gets or forms a type_id for a type, given the instruction kind and arguments.
template <typename InstT, typename... EachArgT>
static auto GetTypeImpl(Context& context, EachArgT... each_arg)
    -> SemIR::TypeId {
  InstT inst = {SemIR::TypeType::TypeId, each_arg...};
  return context.types().GetTypeIdForTypeConstantId(TryEvalInst(context, inst));
}

// Gets or forms a type_id for a type, given the instruction kind and arguments,
// and completes the type. This should only be used when type completion cannot
// fail.
template <typename InstT, typename... EachArgT>
static auto GetCompleteTypeImpl(Context& context, EachArgT... each_arg)
    -> SemIR::TypeId {
  auto type_id = GetTypeImpl<InstT>(context, each_arg...);
  CompleteTypeOrCheckFail(context, type_id);
  return type_id;
}

auto GetStructType(Context& context, SemIR::StructTypeFieldsId fields_id)
    -> SemIR::TypeId {
  return GetTypeImpl<SemIR::StructType>(context, fields_id);
}

auto GetTupleType(Context& context, llvm::ArrayRef<SemIR::InstId> type_inst_ids)
    -> SemIR::TypeId {
  return GetTypeImpl<SemIR::TupleType>(
      context, context.inst_blocks().AddCanonical(type_inst_ids));
}

auto GetAssociatedEntityType(Context& context, SemIR::InterfaceId interface_id,
                             SemIR::SpecificId interface_specific_id)
    -> SemIR::TypeId {
  return GetTypeImpl<SemIR::AssociatedEntityType>(context, interface_id,
                                                  interface_specific_id);
}

auto GetConstType(Context& context, SemIR::TypeInstId inner_type_id)
    -> SemIR::TypeId {
  return GetTypeImpl<SemIR::ConstType>(context, inner_type_id);
}

auto GetSingletonType(Context& context, SemIR::TypeInstId singleton_id)
    -> SemIR::TypeId {
  CARBON_CHECK(SemIR::IsSingletonInstId(singleton_id));
  auto type_id = context.types().GetTypeIdForTypeInstId(singleton_id);
  // To keep client code simpler, complete builtin types before returning them.
  CompleteTypeOrCheckFail(context, type_id);
  return type_id;
}

auto GetClassType(Context& context, SemIR::ClassId class_id,
                  SemIR::SpecificId specific_id) -> SemIR::TypeId {
  return GetTypeImpl<SemIR::ClassType>(context, class_id, specific_id);
}

auto GetFunctionType(Context& context, SemIR::FunctionId fn_id,
                     SemIR::SpecificId specific_id) -> SemIR::TypeId {
  return GetCompleteTypeImpl<SemIR::FunctionType>(context, fn_id, specific_id);
}

auto GetFunctionTypeWithSelfType(Context& context,
                                 SemIR::TypeInstId interface_function_type_id,
                                 SemIR::InstId self_id) -> SemIR::TypeId {
  return GetCompleteTypeImpl<SemIR::FunctionTypeWithSelfType>(
      context, interface_function_type_id, self_id);
}

auto GetGenericClassType(Context& context, SemIR::ClassId class_id,
                         SemIR::SpecificId enclosing_specific_id)
    -> SemIR::TypeId {
  return GetCompleteTypeImpl<SemIR::GenericClassType>(context, class_id,
                                                      enclosing_specific_id);
}

auto GetGenericInterfaceType(Context& context, SemIR::InterfaceId interface_id,
                             SemIR::SpecificId enclosing_specific_id)
    -> SemIR::TypeId {
  return GetCompleteTypeImpl<SemIR::GenericInterfaceType>(
      context, interface_id, enclosing_specific_id);
}

auto GetInterfaceType(Context& context, SemIR::InterfaceId interface_id,
                      SemIR::SpecificId specific_id) -> SemIR::TypeId {
  return GetTypeImpl<SemIR::FacetType>(
      context,
      FacetTypeFromInterface(context, interface_id, specific_id).facet_type_id);
}

auto GetPointerType(Context& context, SemIR::TypeInstId pointee_type_id)
    -> SemIR::TypeId {
  return GetTypeImpl<SemIR::PointerType>(context, pointee_type_id);
}

auto GetPatternType(Context& context, SemIR::TypeId scrutinee_type_id)
    -> SemIR::TypeId {
  CARBON_CHECK(!context.types().Is<SemIR::PatternType>(scrutinee_type_id),
               "Type is already a pattern type");
  if (scrutinee_type_id == SemIR::ErrorInst::TypeId) {
    return SemIR::ErrorInst::TypeId;
  }
  return GetTypeImpl<SemIR::PatternType>(
      context, context.types().GetInstId(scrutinee_type_id));
}

auto GetUnboundElementType(Context& context, SemIR::TypeInstId class_type_id,
                           SemIR::TypeInstId element_type_id) -> SemIR::TypeId {
  return GetTypeImpl<SemIR::UnboundElementType>(context, class_type_id,
                                                element_type_id);
}

auto GetCanonicalizedFacetOrTypeValue(Context& context, SemIR::InstId inst_id)
    -> SemIR::InstId {
  // We can have FacetAccessType of a FacetValue, and a FacetValue of a
  // FacetAccessType, but they don't nest indefinitely.
  if (auto access = context.insts().TryGetAs<SemIR::FacetAccessType>(inst_id)) {
    inst_id = access->facet_value_inst_id;
  }
  if (auto value = context.insts().TryGetAs<SemIR::FacetValue>(inst_id)) {
    inst_id = value->type_inst_id;

    if (auto access =
            context.insts().TryGetAs<SemIR::FacetAccessType>(inst_id)) {
      inst_id = access->facet_value_inst_id;
    }
  }

  CARBON_CHECK(!context.insts().Is<SemIR::FacetAccessType>(inst_id));
  CARBON_CHECK(!context.insts().Is<SemIR::FacetValue>(inst_id));

  return context.constant_values().GetConstantInstId(inst_id);
}

auto GetCanonicalizedFacetOrTypeValue(Context& context,
                                      SemIR::ConstantId const_id)
    -> SemIR::ConstantId {
  return context.constant_values().Get(GetCanonicalizedFacetOrTypeValue(
      context, context.constant_values().GetInstId(const_id)));
}

}  // namespace Carbon::Check
