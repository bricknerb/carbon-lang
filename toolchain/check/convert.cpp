// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/convert.h"

#include <optional>
#include <string>
#include <utility>

#include "common/check.h"
#include "common/map.h"
#include "llvm/ADT/STLExtras.h"
#include "toolchain/base/kind_switch.h"
#include "toolchain/check/action.h"
#include "toolchain/check/context.h"
#include "toolchain/check/control_flow.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/check/eval.h"
#include "toolchain/check/impl_lookup.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/operator.h"
#include "toolchain/check/pattern_match.h"
#include "toolchain/check/type.h"
#include "toolchain/check/type_completion.h"
#include "toolchain/diagnostics/format_providers.h"
#include "toolchain/sem_ir/copy_on_write_block.h"
#include "toolchain/sem_ir/expr_info.h"
#include "toolchain/sem_ir/file.h"
#include "toolchain/sem_ir/generic.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/typed_insts.h"

// TODO: This contains a lot of recursion. Consider removing it in order to
// prevent accidents.
// NOLINTBEGIN(misc-no-recursion)

namespace Carbon::Check {

// Marks the initializer `init_id` as initializing `target_id`.
static auto MarkInitializerFor(SemIR::File& sem_ir, SemIR::InstId init_id,
                               SemIR::InstId target_id,
                               PendingBlock& target_block) -> void {
  auto return_slot_arg_id = FindReturnSlotArgForInitializer(sem_ir, init_id);
  if (return_slot_arg_id.has_value()) {
    // Replace the temporary in the return slot with a reference to our target.
    CARBON_CHECK(sem_ir.insts().Get(return_slot_arg_id).kind() ==
                     SemIR::TemporaryStorage::Kind,
                 "Return slot for initializer does not contain a temporary; "
                 "initialized multiple times? Have {0}",
                 sem_ir.insts().Get(return_slot_arg_id));
    target_block.MergeReplacing(return_slot_arg_id, target_id);
  }
}

// For a value or initializing expression using a copy value representation,
// copy the value into a temporary object.
static auto CopyValueToTemporary(Context& context, SemIR::InstId init_id)
    -> SemIR::InstId {
  // TODO: Consider using `None` to mean that we immediately materialize and
  // initialize a temporary, rather than two separate instructions.
  auto init = context.sem_ir().insts().Get(init_id);
  auto temporary_id = AddInstWithCleanup<SemIR::TemporaryStorage>(
      context, SemIR::LocId(init_id), {.type_id = init.type_id()});
  return AddInst<SemIR::Temporary>(context, SemIR::LocId(init_id),
                                   {.type_id = init.type_id(),
                                    .storage_id = temporary_id,
                                    .init_id = init_id});
}

// Commits to using a temporary to store the result of the initializing
// expression described by `init_id`, and returns the location of the
// temporary. If `discarded` is `true`, the result is discarded, and no
// temporary will be created if possible; if no temporary is created, the
// return value will be `SemIR::InstId::None`.
static auto FinalizeTemporary(Context& context, SemIR::InstId init_id,
                              bool discarded) -> SemIR::InstId {
  auto& sem_ir = context.sem_ir();
  auto return_slot_arg_id = FindReturnSlotArgForInitializer(sem_ir, init_id);
  if (return_slot_arg_id.has_value()) {
    // The return slot should already have a materialized temporary in it.
    CARBON_CHECK(sem_ir.insts().Get(return_slot_arg_id).kind() ==
                     SemIR::TemporaryStorage::Kind,
                 "Return slot for initializer does not contain a temporary; "
                 "initialized multiple times? Have {0}",
                 sem_ir.insts().Get(return_slot_arg_id));
    auto init = sem_ir.insts().Get(init_id);
    return AddInst<SemIR::Temporary>(context, SemIR::LocId(init_id),
                                     {.type_id = init.type_id(),
                                      .storage_id = return_slot_arg_id,
                                      .init_id = init_id});
  }

  if (discarded) {
    // Don't invent a temporary that we're going to discard.
    return SemIR::InstId::None;
  }

  // The initializer has no return slot, but we want to produce a temporary
  // object. Materialize one now.
  return CopyValueToTemporary(context, init_id);
}

// Materialize a temporary to hold the result of the given expression if it is
// an initializing expression.
static auto MaterializeIfInitializing(Context& context, SemIR::InstId expr_id)
    -> SemIR::InstId {
  if (GetExprCategory(context.sem_ir(), expr_id) ==
      SemIR::ExprCategory::Initializing) {
    return FinalizeTemporary(context, expr_id, /*discarded=*/false);
  }
  return expr_id;
}

// Helper to allow `MakeElementAccessInst` to call `AddInst` with either a
// `PendingBlock` or `Context` (defined in `inst.h`).
template <typename AccessInstT>
static auto AddInst(PendingBlock& block, SemIR::LocId loc_id, AccessInstT inst)
    -> SemIR::InstId {
  return block.AddInst<AccessInstT>(loc_id, inst);
}

// Creates and adds an instruction to perform element access into an aggregate.
template <typename AccessInstT, typename InstBlockT>
static auto MakeElementAccessInst(Context& context, SemIR::LocId loc_id,
                                  SemIR::InstId aggregate_id,
                                  SemIR::TypeId elem_type_id, InstBlockT& block,
                                  size_t i) -> SemIR::InstId {
  if constexpr (std::is_same_v<AccessInstT, SemIR::ArrayIndex>) {
    // TODO: Add a new instruction kind for indexing an array at a constant
    // index so that we don't need an integer literal instruction here, and
    // remove this special case.
    auto index_id = block.template AddInst<SemIR::IntValue>(
        loc_id, {.type_id = GetSingletonType(context,
                                             SemIR::IntLiteralType::TypeInstId),
                 .int_id = context.ints().Add(static_cast<int64_t>(i))});
    return AddInst<AccessInstT>(block, loc_id,
                                {elem_type_id, aggregate_id, index_id});
  } else {
    return AddInst<AccessInstT>(
        block, loc_id, {elem_type_id, aggregate_id, SemIR::ElementIndex(i)});
  }
}

// Converts an element of one aggregate so that it can be used as an element of
// another aggregate.
//
// For the source: `src_id` is the source aggregate, `src_elem_type` is the
// element type, `src_field_index` is the index, and `SourceAccessInstT` is the
// kind of instruction used to access the source element.
//
// For the target: `kind` is the kind of conversion or initialization,
// `target_elem_type` is the element type. For initialization, `target_id` is
// the destination, `target_block` is a pending block for target location
// calculations that will be spliced as the return slot of the initializer if
// necessary, `target_field_index` is the index, and `TargetAccessInstT` is the
// kind of instruction used to access the destination element.
template <typename SourceAccessInstT, typename TargetAccessInstT>
static auto ConvertAggregateElement(
    Context& context, SemIR::LocId loc_id, SemIR::InstId src_id,
    SemIR::TypeInstId src_elem_type_inst,
    llvm::ArrayRef<SemIR::InstId> src_literal_elems,
    ConversionTarget::Kind kind, SemIR::InstId target_id,
    SemIR::TypeInstId target_elem_type_inst, PendingBlock* target_block,
    size_t src_field_index, size_t target_field_index,
    SemIR::ClassType* vtable_class_type = nullptr) -> SemIR::InstId {
  auto src_elem_type =
      context.types().GetTypeIdForTypeInstId(src_elem_type_inst);
  auto target_elem_type =
      context.types().GetTypeIdForTypeInstId(target_elem_type_inst);

  // Compute the location of the source element. This goes into the current code
  // block, not into the target block.
  // TODO: Ideally we would discard this instruction if it's unused.
  auto src_elem_id = !src_literal_elems.empty()
                         ? src_literal_elems[src_field_index]
                         : MakeElementAccessInst<SourceAccessInstT>(
                               context, loc_id, src_id, src_elem_type, context,
                               src_field_index);

  // If we're performing a conversion rather than an initialization, we won't
  // have or need a target.
  ConversionTarget target = {.kind = kind, .type_id = target_elem_type};
  if (!target.is_initializer()) {
    return Convert(context, loc_id, src_elem_id, target);
  }

  // Compute the location of the target element and initialize it.
  PendingBlock::DiscardUnusedInstsScope scope(target_block);
  target.init_block = target_block;
  target.init_id = MakeElementAccessInst<TargetAccessInstT>(
      context, loc_id, target_id, target_elem_type, *target_block,
      target_field_index);
  return Convert(context, loc_id, src_elem_id, target, vtable_class_type);
}

// Performs a conversion from a tuple to an array type. This function only
// converts the type, and does not perform a final conversion to the requested
// expression category.
static auto ConvertTupleToArray(Context& context, SemIR::TupleType tuple_type,
                                SemIR::ArrayType array_type,
                                SemIR::InstId value_id, ConversionTarget target)
    -> SemIR::InstId {
  auto& sem_ir = context.sem_ir();
  auto tuple_elem_types = sem_ir.inst_blocks().Get(tuple_type.type_elements_id);

  auto value = sem_ir.insts().Get(value_id);
  SemIR::LocId value_loc_id(value_id);

  // If we're initializing from a tuple literal, we will use its elements
  // directly. Otherwise, materialize a temporary if needed and index into the
  // result.
  llvm::ArrayRef<SemIR::InstId> literal_elems;
  if (auto tuple_literal = value.TryAs<SemIR::TupleLiteral>()) {
    literal_elems = sem_ir.inst_blocks().Get(tuple_literal->elements_id);
  } else {
    value_id = MaterializeIfInitializing(context, value_id);
  }

  // Check that the tuple is the right size.
  std::optional<uint64_t> array_bound =
      sem_ir.GetArrayBoundValue(array_type.bound_id);
  if (!array_bound) {
    // TODO: Should this fall back to using `ImplicitAs`?
    if (target.diagnose) {
      CARBON_DIAGNOSTIC(ArrayInitDependentBound, Error,
                        "cannot initialize array with dependent bound from a "
                        "list of initializers");
      context.emitter().Emit(value_loc_id, ArrayInitDependentBound);
    }
    return SemIR::ErrorInst::InstId;
  }
  if (tuple_elem_types.size() != array_bound) {
    if (target.diagnose) {
      CARBON_DIAGNOSTIC(ArrayInitFromLiteralArgCountMismatch, Error,
                        "cannot initialize array of {0} element{0:s} from {1} "
                        "initializer{1:s}",
                        Diagnostics::IntAsSelect, Diagnostics::IntAsSelect);
      CARBON_DIAGNOSTIC(
          ArrayInitFromExprArgCountMismatch, Error,
          "cannot initialize array of {0} element{0:s} from tuple "
          "with {1} element{1:s}",
          Diagnostics::IntAsSelect, Diagnostics::IntAsSelect);
      context.emitter().Emit(value_loc_id,
                             literal_elems.empty()
                                 ? ArrayInitFromExprArgCountMismatch
                                 : ArrayInitFromLiteralArgCountMismatch,
                             *array_bound, tuple_elem_types.size());
    }
    return SemIR::ErrorInst::InstId;
  }

  PendingBlock target_block_storage(&context);
  PendingBlock* target_block =
      target.init_block ? target.init_block : &target_block_storage;

  // Arrays are always initialized in-place. Allocate a temporary as the
  // destination for the array initialization if we weren't given one.
  SemIR::InstId return_slot_arg_id = target.init_id;
  if (!target.init_id.has_value()) {
    return_slot_arg_id =
        target_block->AddInstWithCleanup<SemIR::TemporaryStorage>(
            value_loc_id, {.type_id = target.type_id});
  }

  // Initialize each element of the array from the corresponding element of the
  // tuple.
  // TODO: Annotate diagnostics coming from here with the array element index,
  // if initializing from a tuple literal.
  llvm::SmallVector<SemIR::InstId> inits;
  inits.reserve(*array_bound + 1);
  for (auto [i, src_type_inst_id] : llvm::enumerate(
           context.types().GetBlockAsTypeInstIds(tuple_elem_types))) {
    // TODO: This call recurses back into conversion. Switch to an iterative
    // approach.
    auto init_id =
        ConvertAggregateElement<SemIR::TupleAccess, SemIR::ArrayIndex>(
            context, value_loc_id, value_id, src_type_inst_id, literal_elems,
            ConversionTarget::FullInitializer, return_slot_arg_id,
            array_type.element_type_inst_id, target_block, i, i);
    if (init_id == SemIR::ErrorInst::InstId) {
      return SemIR::ErrorInst::InstId;
    }
    inits.push_back(init_id);
  }

  // Flush the temporary here if we didn't insert it earlier, so we can add a
  // reference to the return slot.
  target_block->InsertHere();
  return AddInst<SemIR::ArrayInit>(context, value_loc_id,
                                   {.type_id = target.type_id,
                                    .inits_id = sem_ir.inst_blocks().Add(inits),
                                    .dest_id = return_slot_arg_id});
}

// Performs a conversion from a tuple to a tuple type. This function only
// converts the type, and does not perform a final conversion to the requested
// expression category.
static auto ConvertTupleToTuple(Context& context, SemIR::TupleType src_type,
                                SemIR::TupleType dest_type,
                                SemIR::InstId value_id, ConversionTarget target)
    -> SemIR::InstId {
  auto& sem_ir = context.sem_ir();
  auto src_elem_types = sem_ir.inst_blocks().Get(src_type.type_elements_id);
  auto dest_elem_types = sem_ir.inst_blocks().Get(dest_type.type_elements_id);

  auto value = sem_ir.insts().Get(value_id);
  SemIR::LocId value_loc_id(value_id);

  // If we're initializing from a tuple literal, we will use its elements
  // directly. Otherwise, materialize a temporary if needed and index into the
  // result.
  llvm::ArrayRef<SemIR::InstId> literal_elems;
  auto literal_elems_id = SemIR::InstBlockId::None;
  if (auto tuple_literal = value.TryAs<SemIR::TupleLiteral>()) {
    literal_elems_id = tuple_literal->elements_id;
    literal_elems = sem_ir.inst_blocks().Get(literal_elems_id);
  } else {
    value_id = MaterializeIfInitializing(context, value_id);
  }

  // Check that the tuples are the same size.
  if (src_elem_types.size() != dest_elem_types.size()) {
    if (target.diagnose) {
      CARBON_DIAGNOSTIC(
          TupleInitElementCountMismatch, Error,
          "cannot initialize tuple of {0} element{0:s} from tuple "
          "with {1} element{1:s}",
          Diagnostics::IntAsSelect, Diagnostics::IntAsSelect);
      context.emitter().Emit(value_loc_id, TupleInitElementCountMismatch,
                             dest_elem_types.size(), src_elem_types.size());
    }
    return SemIR::ErrorInst::InstId;
  }

  // If we're forming an initializer, then we want an initializer for each
  // element. Otherwise, we want a value representation for each element.
  // Perform a final destination store if we're performing an in-place
  // initialization.
  bool is_init = target.is_initializer();
  ConversionTarget::Kind inner_kind =
      !is_init ? ConversionTarget::Value
      : SemIR::InitRepr::ForType(sem_ir, target.type_id).kind ==
              SemIR::InitRepr::InPlace
          ? ConversionTarget::FullInitializer
          : ConversionTarget::Initializer;

  // Initialize each element of the destination from the corresponding element
  // of the source.
  // TODO: Annotate diagnostics coming from here with the element index.
  auto new_block =
      literal_elems_id.has_value()
          ? SemIR::CopyOnWriteInstBlock(&sem_ir, literal_elems_id)
          : SemIR::CopyOnWriteInstBlock(
                &sem_ir, SemIR::CopyOnWriteInstBlock::UninitializedBlock{
                             src_elem_types.size()});
  for (auto [i, src_type_inst_id, dest_type_inst_id] : llvm::enumerate(
           context.types().GetBlockAsTypeInstIds(src_elem_types),
           context.types().GetBlockAsTypeInstIds(dest_elem_types))) {
    // TODO: This call recurses back into conversion. Switch to an iterative
    // approach.
    auto init_id =
        ConvertAggregateElement<SemIR::TupleAccess, SemIR::TupleAccess>(
            context, value_loc_id, value_id, src_type_inst_id, literal_elems,
            inner_kind, target.init_id, dest_type_inst_id, target.init_block, i,
            i);
    if (init_id == SemIR::ErrorInst::InstId) {
      return SemIR::ErrorInst::InstId;
    }
    new_block.Set(i, init_id);
  }

  if (is_init) {
    target.init_block->InsertHere();
    return AddInst<SemIR::TupleInit>(context, value_loc_id,
                                     {.type_id = target.type_id,
                                      .elements_id = new_block.id(),
                                      .dest_id = target.init_id});
  } else {
    return AddInst<SemIR::TupleValue>(
        context, value_loc_id,
        {.type_id = target.type_id, .elements_id = new_block.id()});
  }
}

// Common implementation for ConvertStructToStruct and ConvertStructToClass.
template <typename TargetAccessInstT>
static auto ConvertStructToStructOrClass(
    Context& context, SemIR::StructType src_type, SemIR::StructType dest_type,
    SemIR::InstId value_id, ConversionTarget target,
    SemIR::ClassType* vtable_class_type = nullptr) -> SemIR::InstId {
  static_assert(std::is_same_v<SemIR::ClassElementAccess, TargetAccessInstT> ||
                std::is_same_v<SemIR::StructAccess, TargetAccessInstT>);
  constexpr bool ToClass =
      std::is_same_v<SemIR::ClassElementAccess, TargetAccessInstT>;

  auto& sem_ir = context.sem_ir();
  auto src_elem_fields = sem_ir.struct_type_fields().Get(src_type.fields_id);
  auto dest_elem_fields = sem_ir.struct_type_fields().Get(dest_type.fields_id);
  bool dest_has_vptr = !dest_elem_fields.empty() &&
                       dest_elem_fields.front().name_id == SemIR::NameId::Vptr;
  int dest_vptr_offset = (dest_has_vptr ? 1 : 0);
  auto dest_elem_fields_size = dest_elem_fields.size() - dest_vptr_offset;

  auto value = sem_ir.insts().Get(value_id);
  SemIR::LocId value_loc_id(value_id);

  // If we're initializing from a struct literal, we will use its elements
  // directly. Otherwise, materialize a temporary if needed and index into the
  // result.
  llvm::ArrayRef<SemIR::InstId> literal_elems;
  auto literal_elems_id = SemIR::InstBlockId::None;
  if (auto struct_literal = value.TryAs<SemIR::StructLiteral>()) {
    literal_elems_id = struct_literal->elements_id;
    literal_elems = sem_ir.inst_blocks().Get(literal_elems_id);
  } else {
    value_id = MaterializeIfInitializing(context, value_id);
  }

  // Check that the structs are the same size.
  // TODO: If not, include the name of the first source field that doesn't
  // exist in the destination or vice versa in the diagnostic.
  if (src_elem_fields.size() != dest_elem_fields_size) {
    if (target.diagnose) {
      CARBON_DIAGNOSTIC(
          StructInitElementCountMismatch, Error,
          "cannot initialize {0:class|struct} with {1} field{1:s} from struct "
          "with {2} field{2:s}",
          Diagnostics::BoolAsSelect, Diagnostics::IntAsSelect,
          Diagnostics::IntAsSelect);
      context.emitter().Emit(value_loc_id, StructInitElementCountMismatch,
                             ToClass, dest_elem_fields_size,
                             src_elem_fields.size());
    }
    return SemIR::ErrorInst::InstId;
  }

  // Prepare to look up fields in the source by index.
  Map<SemIR::NameId, int32_t> src_field_indexes;
  if (src_type.fields_id != dest_type.fields_id) {
    for (auto [i, field] : llvm::enumerate(src_elem_fields)) {
      auto result = src_field_indexes.Insert(field.name_id, i);
      CARBON_CHECK(result.is_inserted(), "Duplicate field in source structure");
    }
  }

  // If we're forming an initializer, then we want an initializer for each
  // element. Otherwise, we want a value representation for each element.
  // Perform a final destination store if we're performing an in-place
  // initialization.
  bool is_init = target.is_initializer();
  ConversionTarget::Kind inner_kind =
      !is_init ? ConversionTarget::Value
      : SemIR::InitRepr::ForType(sem_ir, target.type_id).kind ==
              SemIR::InitRepr::InPlace
          ? ConversionTarget::FullInitializer
          : ConversionTarget::Initializer;

  // Initialize each element of the destination from the corresponding element
  // of the source.
  // TODO: Annotate diagnostics coming from here with the element index.
  auto new_block =
      literal_elems_id.has_value() && !dest_has_vptr
          ? SemIR::CopyOnWriteInstBlock(&sem_ir, literal_elems_id)
          : SemIR::CopyOnWriteInstBlock(
                &sem_ir, SemIR::CopyOnWriteInstBlock::UninitializedBlock{
                             dest_elem_fields.size()});
  for (auto [i, dest_field] : llvm::enumerate(dest_elem_fields)) {
    if (dest_field.name_id == SemIR::NameId::Vptr) {
      if constexpr (!ToClass) {
        CARBON_FATAL("Only classes should have vptrs.");
      }
      target.init_block->InsertHere();
      auto vptr_type_id =
          context.types().GetTypeIdForTypeInstId(dest_field.type_inst_id);
      auto dest_id =
          AddInst<SemIR::ClassElementAccess>(context, value_loc_id,
                                             {.type_id = vptr_type_id,
                                              .base_id = target.init_id,
                                              .index = SemIR::ElementIndex(i)});
      auto vtable_decl_id =
          context.classes().Get(vtable_class_type->class_id).vtable_decl_id;
      LoadImportRef(context, vtable_decl_id);
      auto canonical_vtable_decl_id =
          context.constant_values().GetConstantInstId(vtable_decl_id);
      auto vtable_ptr_id = AddInst<SemIR::VtablePtr>(
          context, value_loc_id,
          {.type_id = GetPointerType(context, SemIR::VtableType::TypeInstId),
           .vtable_id = context.insts()
                            .GetAs<SemIR::VtableDecl>(canonical_vtable_decl_id)
                            .vtable_id,
           .specific_id = vtable_class_type->specific_id});
      auto init_id = AddInst<SemIR::InitializeFrom>(context, value_loc_id,
                                                    {.type_id = vptr_type_id,
                                                     .src_id = vtable_ptr_id,
                                                     .dest_id = dest_id});
      new_block.Set(i, init_id);
      continue;
    }

    // Find the matching source field.
    auto src_field_index = i;
    if (src_type.fields_id != dest_type.fields_id) {
      if (auto lookup = src_field_indexes.Lookup(dest_field.name_id)) {
        src_field_index = lookup.value();
      } else {
        if (target.diagnose) {
          if (literal_elems_id.has_value()) {
            CARBON_DIAGNOSTIC(
                StructInitMissingFieldInLiteral, Error,
                "missing value for field `{0}` in struct initialization",
                SemIR::NameId);
            context.emitter().Emit(value_loc_id,
                                   StructInitMissingFieldInLiteral,
                                   dest_field.name_id);
          } else {
            CARBON_DIAGNOSTIC(StructInitMissingFieldInConversion, Error,
                              "cannot convert from struct type {0} to {1}: "
                              "missing field `{2}` in source type",
                              TypeOfInstId, SemIR::TypeId, SemIR::NameId);
            context.emitter().Emit(value_loc_id,
                                   StructInitMissingFieldInConversion, value_id,
                                   target.type_id, dest_field.name_id);
          }
        }
        return SemIR::ErrorInst::InstId;
      }
    }
    auto src_field = src_elem_fields[src_field_index];

    // TODO: This call recurses back into conversion. Switch to an iterative
    // approach.
    auto init_id =
        ConvertAggregateElement<SemIR::StructAccess, TargetAccessInstT>(
            context, value_loc_id, value_id, src_field.type_inst_id,
            literal_elems, inner_kind, target.init_id, dest_field.type_inst_id,
            target.init_block, src_field_index,
            src_field_index + dest_vptr_offset, vtable_class_type);
    if (init_id == SemIR::ErrorInst::InstId) {
      return SemIR::ErrorInst::InstId;
    }
    new_block.Set(i, init_id);
  }

  if (ToClass) {
    target.init_block->InsertHere();
    CARBON_CHECK(is_init,
                 "Converting directly to a class value is not supported");
    return AddInst<SemIR::ClassInit>(context, value_loc_id,
                                     {.type_id = target.type_id,
                                      .elements_id = new_block.id(),
                                      .dest_id = target.init_id});
  } else if (is_init) {
    target.init_block->InsertHere();
    return AddInst<SemIR::StructInit>(context, value_loc_id,
                                      {.type_id = target.type_id,
                                       .elements_id = new_block.id(),
                                       .dest_id = target.init_id});
  } else {
    return AddInst<SemIR::StructValue>(
        context, value_loc_id,
        {.type_id = target.type_id, .elements_id = new_block.id()});
  }
}

// Performs a conversion from a struct to a struct type. This function only
// converts the type, and does not perform a final conversion to the requested
// expression category.
static auto ConvertStructToStruct(Context& context, SemIR::StructType src_type,
                                  SemIR::StructType dest_type,
                                  SemIR::InstId value_id,
                                  ConversionTarget target) -> SemIR::InstId {
  return ConvertStructToStructOrClass<SemIR::StructAccess>(
      context, src_type, dest_type, value_id, target);
}

// Performs a conversion from a struct to a class type. This function only
// converts the type, and does not perform a final conversion to the requested
// expression category.
static auto ConvertStructToClass(Context& context, SemIR::StructType src_type,
                                 SemIR::ClassType dest_type,
                                 SemIR::InstId value_id,
                                 ConversionTarget target,
                                 SemIR::ClassType* vtable_class_type)
    -> SemIR::InstId {
  PendingBlock target_block(&context);
  auto& dest_class_info = context.classes().Get(dest_type.class_id);
  CARBON_CHECK(dest_class_info.inheritance_kind != SemIR::Class::Abstract);
  auto object_repr_id =
      dest_class_info.GetObjectRepr(context.sem_ir(), dest_type.specific_id);
  if (object_repr_id == SemIR::ErrorInst::TypeId) {
    return SemIR::ErrorInst::InstId;
  }
  if (context.types().Is<SemIR::CustomLayoutType>(object_repr_id)) {
    // Builtin conversion does not apply.
    return value_id;
  }
  auto dest_struct_type =
      context.types().GetAs<SemIR::StructType>(object_repr_id);

  // If we're trying to create a class value, form a temporary for the value to
  // point to.
  bool need_temporary = !target.is_initializer();
  if (need_temporary) {
    target.kind = ConversionTarget::Initializer;
    target.init_block = &target_block;
    target.init_id = target_block.AddInstWithCleanup<SemIR::TemporaryStorage>(
        SemIR::LocId(value_id), {.type_id = target.type_id});
  }

  auto result_id = ConvertStructToStructOrClass<SemIR::ClassElementAccess>(
      context, src_type, dest_struct_type, value_id, target,
      vtable_class_type ? vtable_class_type : &dest_type);

  if (need_temporary) {
    target_block.InsertHere();
    result_id = AddInst<SemIR::Temporary>(context, SemIR::LocId(value_id),
                                          {.type_id = target.type_id,
                                           .storage_id = target.init_id,
                                           .init_id = result_id});
  }
  return result_id;
}

// An inheritance path is a sequence of `BaseDecl`s and corresponding base types
// in order from derived to base.
using InheritancePath =
    llvm::SmallVector<std::pair<SemIR::InstId, SemIR::TypeId>>;

// Computes the inheritance path from class `derived_id` to class `base_id`.
// Returns nullopt if `derived_id` is not a class derived from `base_id`.
static auto ComputeInheritancePath(Context& context, SemIR::LocId loc_id,
                                   SemIR::TypeId derived_id,
                                   SemIR::TypeId base_id)
    -> std::optional<InheritancePath> {
  // We intend for NRVO to be applied to `result`. All `return` statements in
  // this function should `return result;`.
  std::optional<InheritancePath> result(std::in_place);
  if (!TryToCompleteType(context, derived_id, loc_id)) {
    // TODO: Should we give an error here? If we don't, and there is an
    // inheritance path when the class is defined, we may have a coherence
    // problem.
    result = std::nullopt;
    return result;
  }
  while (derived_id != base_id) {
    auto derived_class_type =
        context.types().TryGetAs<SemIR::ClassType>(derived_id);
    if (!derived_class_type) {
      result = std::nullopt;
      break;
    }
    auto& derived_class = context.classes().Get(derived_class_type->class_id);
    auto base_type_id = derived_class.GetBaseType(
        context.sem_ir(), derived_class_type->specific_id);
    if (!base_type_id.has_value()) {
      result = std::nullopt;
      break;
    }
    result->push_back({derived_class.base_id, base_type_id});
    derived_id = base_type_id;
  }
  return result;
}

// Performs a conversion from a derived class value or reference to a base class
// value or reference.
static auto ConvertDerivedToBase(Context& context, SemIR::LocId loc_id,
                                 SemIR::InstId value_id,
                                 const InheritancePath& path) -> SemIR::InstId {
  // Materialize a temporary if necessary.
  value_id = ConvertToValueOrRefExpr(context, value_id);

  // Preserve type qualifiers.
  auto quals = context.types()
                   .GetUnqualifiedTypeAndQualifiers(
                       context.insts().Get(value_id).type_id())
                   .second;

  // Add a series of `.base` accesses.
  for (auto [base_id, base_type_id] : path) {
    auto base_decl = context.insts().GetAs<SemIR::BaseDecl>(base_id);
    value_id = AddInst<SemIR::ClassElementAccess>(
        context, loc_id,
        {.type_id = GetQualifiedType(context, base_type_id, quals),
         .base_id = value_id,
         .index = base_decl.index});
  }
  return value_id;
}

// Performs a conversion from a derived class pointer to a base class pointer.
static auto ConvertDerivedPointerToBasePointer(
    Context& context, SemIR::LocId loc_id, SemIR::PointerType src_ptr_type,
    SemIR::TypeId dest_ptr_type_id, SemIR::InstId ptr_id,
    const InheritancePath& path) -> SemIR::InstId {
  auto pointee_type_id =
      context.types().GetTypeIdForTypeInstId(src_ptr_type.pointee_id);

  // Form `*p`.
  ptr_id = ConvertToValueExpr(context, ptr_id);
  auto ref_id = AddInst<SemIR::Deref>(
      context, loc_id, {.type_id = pointee_type_id, .pointer_id = ptr_id});

  // Convert as a reference expression.
  ref_id = ConvertDerivedToBase(context, loc_id, ref_id, path);

  // Take the address.
  return AddInst<SemIR::AddrOf>(
      context, loc_id, {.type_id = dest_ptr_type_id, .lvalue_id = ref_id});
}

// Returns whether `category` is a valid expression category to produce as a
// result of a conversion with kind `target_kind`, or at most needs a temporary
// to be materialized.
static auto IsValidExprCategoryForConversionTarget(
    SemIR::ExprCategory category, ConversionTarget::Kind target_kind) -> bool {
  switch (target_kind) {
    case ConversionTarget::Value:
      return category == SemIR::ExprCategory::Value;
    case ConversionTarget::ValueOrRef:
    case ConversionTarget::Discarded:
      return category == SemIR::ExprCategory::Value ||
             category == SemIR::ExprCategory::DurableRef ||
             category == SemIR::ExprCategory::EphemeralRef ||
             category == SemIR::ExprCategory::Initializing;
    case ConversionTarget::DurableRef:
      return category == SemIR::ExprCategory::DurableRef;
    case ConversionTarget::CppThunkRef:
      return category == SemIR::ExprCategory::EphemeralRef;
    case ConversionTarget::ExplicitAs:
      return true;
    case ConversionTarget::Initializer:
    case ConversionTarget::FullInitializer:
      return category == SemIR::ExprCategory::Initializing;
  }
}

// Determines whether the initialization representation of the type is a copy of
// the value representation.
static auto InitReprIsCopyOfValueRepr(const SemIR::File& sem_ir,
                                      SemIR::TypeId type_id) -> bool {
  // The initializing representation is a copy of the value representation if
  // they're both copies of the object representation.
  return SemIR::InitRepr::ForType(sem_ir, type_id).IsCopyOfObjectRepr() &&
         SemIR::ValueRepr::ForType(sem_ir, type_id)
             .IsCopyOfObjectRepr(sem_ir, type_id);
}

// Determines whether we can pull a value directly out of an initializing
// expression of type `type_id` to initialize a target of type `type_id` and
// kind `target_kind`.
static auto CanUseValueOfInitializer(const SemIR::File& sem_ir,
                                     SemIR::TypeId type_id,
                                     ConversionTarget::Kind target_kind)
    -> bool {
  if (!IsValidExprCategoryForConversionTarget(SemIR::ExprCategory::Value,
                                              target_kind)) {
    // We don't want a value expression.
    return false;
  }

  // We can pull a value out of an initializing expression if it holds one.
  return InitReprIsCopyOfValueRepr(sem_ir, type_id);
}

static auto DiagnoseConversionFailureToConstraintValue(
    Context& context, SemIR::LocId loc_id, SemIR::InstId expr_id,
    SemIR::TypeId target_type_id) -> void {
  CARBON_DCHECK(target_type_id == SemIR::TypeType::TypeId ||
                context.types().Is<SemIR::FacetType>(target_type_id));

  auto type_of_expr_id = context.insts().Get(expr_id).type_id();
  CARBON_CHECK(context.types().IsFacetType(type_of_expr_id));
  // If the source type is/has a facet value, then we can include its
  // FacetType in the diagnostic to help explain what interfaces the
  // source type implements.
  auto facet_value_inst_id = SemIR::InstId::None;
  if (auto facet_access_type =
          context.insts().TryGetAs<SemIR::FacetAccessType>(expr_id)) {
    facet_value_inst_id = facet_access_type->facet_value_inst_id;
  } else if (context.types().Is<SemIR::FacetType>(type_of_expr_id)) {
    facet_value_inst_id = expr_id;
  }

  if (facet_value_inst_id.has_value()) {
    CARBON_DIAGNOSTIC(ConversionFailureFacetToFacet, Error,
                      "cannot convert type {0} that implements {1} into type "
                      "implementing {2}",
                      InstIdAsType, TypeOfInstId, SemIR::TypeId);
    context.emitter().Emit(loc_id, ConversionFailureFacetToFacet, expr_id,
                           facet_value_inst_id, target_type_id);
  } else {
    CARBON_DIAGNOSTIC(ConversionFailureTypeToFacet, Error,
                      "cannot convert type {0} into type implementing {1}",
                      InstIdAsType, SemIR::TypeId);
    context.emitter().Emit(loc_id, ConversionFailureTypeToFacet, expr_id,
                           target_type_id);
  }
}

static auto PerformBuiltinConversion(
    Context& context, SemIR::LocId loc_id, SemIR::InstId value_id,
    ConversionTarget target, SemIR::ClassType* vtable_class_type = nullptr)
    -> SemIR::InstId {
  auto& sem_ir = context.sem_ir();
  auto value = sem_ir.insts().Get(value_id);
  auto value_type_id = value.type_id();
  auto target_type_inst = sem_ir.types().GetAsInst(target.type_id);

  // Various forms of implicit conversion are supported as builtin conversions,
  // either in addition to or instead of `impl`s of `ImplicitAs` in the Carbon
  // prelude. There are a few reasons we need to perform some of these
  // conversions as builtins:
  //
  // 1) Conversions from struct and tuple *literals* have special rules that
  //    cannot be implemented by invoking `ImplicitAs`. Specifically, we must
  //    recurse into the elements of the literal before performing
  //    initialization in order to avoid unnecessary conversions between
  //    expression categories that would be performed by `ImplicitAs.Convert`.
  // 2) (Not implemented yet) Conversion of a facet to a facet type depends on
  //    the value of the facet, not only its type, and therefore cannot be
  //    modeled by `ImplicitAs`.
  // 3) Some of these conversions are used while checking the library
  //    definition of `ImplicitAs` itself or implementations of it.
  //
  // We also expect to see better performance by avoiding an `impl` lookup for
  // common conversions.
  //
  // TODO: We should provide a debugging flag to turn off as many of these
  // builtin conversions as we can so that we can test that they do the same
  // thing as the library implementations.
  //
  // The builtin conversions that correspond to `impl`s in the library all
  // correspond to `final impl`s, so we don't need to worry about `ImplicitAs`
  // being specialized in any of these cases.

  // If the value is already of the right kind and expression category, there's
  // nothing to do. Performing a conversion would decompose and rebuild tuples
  // and structs, so it's important that we bail out early in this case.
  if (value_type_id == target.type_id) {
    auto value_cat = SemIR::GetExprCategory(sem_ir, value_id);
    if (IsValidExprCategoryForConversionTarget(value_cat, target.kind)) {
      return value_id;
    }

    // If the source is an initializing expression, we may be able to pull a
    // value right out of it.
    if (value_cat == SemIR::ExprCategory::Initializing &&
        CanUseValueOfInitializer(sem_ir, value_type_id, target.kind)) {
      return AddInst<SemIR::ValueOfInitializer>(
          context, loc_id, {.type_id = value_type_id, .init_id = value_id});
    }

    // PerformBuiltinConversion converts each part of a tuple or struct, even
    // when the types are the same. This is not done for classes since they have
    // to define their conversions as part of their api.
    //
    // If a class adapts a tuple or struct, we convert each of its parts when
    // there's no other conversion going on (the source and target types are the
    // same). To do so, we have to insert a conversion of the value up to the
    // foundation and back down, and a conversion of the initializing object if
    // there is one.
    //
    // Implementation note: We do the conversion through a call to
    // PerformBuiltinConversion() call rather than a Convert() call to avoid
    // extraneous `converted` semir instructions on the adapted types, and as a
    // shortcut to doing the explicit calls to walk the parts of the
    // tuple/struct which happens inside PerformBuiltinConversion().
    if (auto foundation_type_id =
            context.types().GetTransitiveAdaptedType(value_type_id);
        foundation_type_id != value_type_id &&
        (context.types().Is<SemIR::TupleType>(foundation_type_id) ||
         context.types().Is<SemIR::StructType>(foundation_type_id))) {
      auto foundation_value_id = AddInst<SemIR::AsCompatible>(
          context, loc_id,
          {.type_id = foundation_type_id, .source_id = value_id});

      auto foundation_init_id = target.init_id;
      if (foundation_init_id != SemIR::InstId::None) {
        foundation_init_id = target.init_block->AddInst<SemIR::AsCompatible>(
            loc_id,
            {.type_id = foundation_type_id, .source_id = target.init_id});
      }

      {
        // While the types are the same, the conversion can still fail if it
        // performs a copy while converting the value to another category, and
        // the type (or some part of it) is not copyable.
        Diagnostics::AnnotationScope annotate_diagnostics(
            &context.emitter(), [&](auto& builder) {
              CARBON_DIAGNOSTIC(InCopy, Note, "in copy of {0}", TypeOfInstId);
              builder.Note(value_id, InCopy, value_id);
            });

        foundation_value_id =
            PerformBuiltinConversion(context, loc_id, foundation_value_id,
                                     {.kind = target.kind,
                                      .type_id = foundation_type_id,
                                      .init_id = foundation_init_id,
                                      .init_block = target.init_block,
                                      .diagnose = target.diagnose});
        if (foundation_value_id == SemIR::ErrorInst::InstId) {
          return SemIR::ErrorInst::InstId;
        }
      }

      return AddInst<SemIR::AsCompatible>(
          context, loc_id,
          {.type_id = target.type_id, .source_id = foundation_value_id});
    }
  }

  // T explicitly converts to U if T is compatible with U.
  if (target.kind == ConversionTarget::Kind::ExplicitAs &&
      target.type_id != value_type_id) {
    auto target_foundation_id =
        context.types().GetTransitiveAdaptedType(target.type_id);
    auto value_foundation_id =
        context.types().GetTransitiveAdaptedType(value_type_id);
    if (target_foundation_id == value_foundation_id) {
      // For a struct or tuple literal, perform a category conversion if
      // necessary.
      if (SemIR::GetExprCategory(context.sem_ir(), value_id) ==
          SemIR::ExprCategory::Mixed) {
        value_id = PerformBuiltinConversion(context, loc_id, value_id,
                                            {.kind = ConversionTarget::Value,
                                             .type_id = value_type_id,
                                             .diagnose = target.diagnose});
      }
      return AddInst<SemIR::AsCompatible>(
          context, loc_id, {.type_id = target.type_id, .source_id = value_id});
    }
  }

  // A tuple (T1, T2, ..., Tn) converts to (U1, U2, ..., Un) if each Ti
  // converts to Ui.
  if (auto target_tuple_type = target_type_inst.TryAs<SemIR::TupleType>()) {
    if (auto src_tuple_type =
            sem_ir.types().TryGetAs<SemIR::TupleType>(value_type_id)) {
      return ConvertTupleToTuple(context, *src_tuple_type, *target_tuple_type,
                                 value_id, target);
    }
  }

  // A struct {.f_1: T_1, .f_2: T_2, ..., .f_n: T_n} converts to
  // {.f_p(1): U_p(1), .f_p(2): U_p(2), ..., .f_p(n): U_p(n)} if
  // (p(1), ..., p(n)) is a permutation of (1, ..., n) and each Ti converts
  // to Ui.
  if (auto target_struct_type = target_type_inst.TryAs<SemIR::StructType>()) {
    if (auto src_struct_type =
            sem_ir.types().TryGetAs<SemIR::StructType>(value_type_id)) {
      return ConvertStructToStruct(context, *src_struct_type,
                                   *target_struct_type, value_id, target);
    }
  }

  // No other conversions apply when the source and destination types are the
  // same.
  if (value_type_id == target.type_id) {
    return value_id;
  }

  // A tuple (T1, T2, ..., Tn) converts to array(T, n) if each Ti converts to T.
  if (auto target_array_type = target_type_inst.TryAs<SemIR::ArrayType>()) {
    if (auto src_tuple_type =
            sem_ir.types().TryGetAs<SemIR::TupleType>(value_type_id)) {
      return ConvertTupleToArray(context, *src_tuple_type, *target_array_type,
                                 value_id, target);
    }
  }

  // A struct {.f_1: T_1, .f_2: T_2, ..., .f_n: T_n} converts to a class type
  // if it converts to the struct type that is the class's representation type
  // (a struct with the same fields as the class, plus a base field where
  // relevant).
  if (auto target_class_type = target_type_inst.TryAs<SemIR::ClassType>()) {
    if (auto src_struct_type =
            sem_ir.types().TryGetAs<SemIR::StructType>(value_type_id)) {
      if (!context.classes()
               .Get(target_class_type->class_id)
               .adapt_id.has_value()) {
        return ConvertStructToClass(context, *src_struct_type,
                                    *target_class_type, value_id, target,
                                    vtable_class_type);
      }
    }

    // An expression of type T converts to U if T is a class derived from U.
    if (auto path = ComputeInheritancePath(context, loc_id, value_type_id,
                                           target.type_id);
        path && !path->empty()) {
      return ConvertDerivedToBase(context, loc_id, value_id, *path);
    }
  }

  // A pointer T* converts to [const] U* if T is the same as U, or is a class
  // derived from U.
  if (auto target_pointer_type = target_type_inst.TryAs<SemIR::PointerType>()) {
    if (auto src_pointer_type =
            sem_ir.types().TryGetAs<SemIR::PointerType>(value_type_id)) {
      auto [unqual_target_pointee_type_id, target_quals] =
          sem_ir.types().GetUnqualifiedTypeAndQualifiers(
              context.types().GetTypeIdForTypeInstId(
                  target_pointer_type->pointee_id));
      auto [unqual_src_pointee_type_id, src_quals] =
          sem_ir.types().GetUnqualifiedTypeAndQualifiers(
              context.types().GetTypeIdForTypeInstId(
                  src_pointer_type->pointee_id));

      // If the qualifiers are incompatible, we can't perform a conversion.
      if ((src_quals & ~target_quals) != SemIR::TypeQualifiers::None) {
        // TODO: Consider producing a custom diagnostic here for a cast that
        // discards constness. We should allow this with `unsafe as`.
        return value_id;
      }

      if (unqual_target_pointee_type_id != unqual_src_pointee_type_id) {
        // If there's an inheritance path from target to source, this is a
        // derived to base conversion.
        if (auto path = ComputeInheritancePath(context, loc_id,
                                               unqual_src_pointee_type_id,
                                               unqual_target_pointee_type_id);
            path && !path->empty()) {
          value_id = ConvertDerivedPointerToBasePointer(
              context, loc_id, *src_pointer_type, target.type_id, value_id,
              *path);
        } else {
          // No conversion was possible.
          return value_id;
        }
      }

      // Perform a compatible conversion to add any new qualifiers.
      if (src_quals != target_quals) {
        return AddInst<SemIR::AsCompatible>(
            context, loc_id,
            {.type_id = target.type_id, .source_id = value_id});
      }
      return value_id;
    }
  }

  if (target.type_id == SemIR::TypeType::TypeId ||
      sem_ir.types().Is<SemIR::FacetType>(target.type_id)) {
    auto type_value_id = SemIR::InstId::None;

    // A tuple of types converts to type `type`.
    // TODO: This should apply even for non-literal tuples.
    if (auto tuple_literal = value.TryAs<SemIR::TupleLiteral>()) {
      llvm::SmallVector<SemIR::InstId> type_inst_ids;
      for (auto tuple_inst_id :
           sem_ir.inst_blocks().Get(tuple_literal->elements_id)) {
        // TODO: This call recurses back into conversion. Switch to an
        // iterative approach.
        type_inst_ids.push_back(
            ExprAsType(context, loc_id, tuple_inst_id, target.diagnose)
                .inst_id);
      }
      // TODO: Should we add this as an instruction? It will contain references
      // to local InstIds.
      auto tuple_type_id = GetTupleType(context, type_inst_ids);
      type_value_id = sem_ir.types().GetInstId(tuple_type_id);
    }

    // `{}` converts to `{} as type`.
    // TODO: This conversion should also be performed for a non-literal value
    // of type `{}`.
    if (auto struct_literal = value.TryAs<SemIR::StructLiteral>();
        struct_literal &&
        struct_literal->elements_id == SemIR::InstBlockId::Empty) {
      type_value_id = sem_ir.types().GetInstId(value_type_id);
    }

    if (type_value_id != SemIR::InstId::None) {
      if (sem_ir.types().Is<SemIR::FacetType>(target.type_id)) {
        // Use the converted `TypeType` value for converting to a facet.
        value_id = type_value_id;
        value_type_id = SemIR::TypeType::TypeId;
      } else {
        // We wanted a `TypeType`, and we've done that.
        return type_value_id;
      }
    }
  }

  // FacetType converts to Type by wrapping the facet value in
  // FacetAccessType.
  if (target.type_id == SemIR::TypeType::TypeId &&
      sem_ir.types().Is<SemIR::FacetType>(value_type_id)) {
    return AddInst<SemIR::FacetAccessType>(
        context, loc_id,
        {.type_id = target.type_id, .facet_value_inst_id = value_id});
  }

  // Type values can convert to facet values, and facet values can convert to
  // other facet values, as long as they satisfy the required interfaces of the
  // target `FacetType`.
  if (target.type_id != value_type_id &&
      sem_ir.types().Is<SemIR::FacetType>(target.type_id) &&
      (sem_ir.types().Is<SemIR::TypeType>(value_type_id) ||
       sem_ir.types().Is<SemIR::FacetType>(value_type_id))) {
    // The value is a type or facet value, so it has a constant value. We get
    // that to unwrap things like NameRef and get to the underlying type or
    // facet value instruction so that we can use `TryGetAs`.
    auto const_value_id = sem_ir.constant_values().GetConstantInstId(value_id);
    // TODO: Runtime facet values should be allowed to convert based on their
    // FacetTypes, but we assume constant values for impl lookup at the moment.
    if (!const_value_id.has_value()) {
      context.TODO(loc_id, "conversion of runtime facet value");
      const_value_id = SemIR::ErrorInst::InstId;
    }

    if (auto facet_access_type_inst =
            sem_ir.insts().TryGetAs<SemIR::FacetAccessType>(const_value_id)) {
      // Conversion from a `FacetAccessType` to a `FacetValue` of the target
      // `FacetType` if the instruction in the `FacetAccessType` is of a
      // `FacetType` that satisfies the requirements of the target `FacetType`.
      // If the `FacetType` exactly matches the target `FacetType` then we can
      // shortcut and use that value, and avoid impl lookup.
      auto facet_value_inst_id = facet_access_type_inst->facet_value_inst_id;
      if (sem_ir.insts().Get(facet_value_inst_id).type_id() == target.type_id) {
        return facet_value_inst_id;
      }
    }

    // Conversion from a facet value (which has type `FacetType`) or a type
    // value (which has type `TypeType`) to a facet value. We can do this if the
    // type satisfies the requirements of the target `FacetType`, as determined
    // by finding impl witnesses for the target FacetType.
    auto lookup_result = LookupImplWitness(
        context, loc_id, sem_ir.constant_values().Get(const_value_id),
        sem_ir.types().GetConstantId(target.type_id));
    if (lookup_result.has_value()) {
      if (lookup_result.has_error_value()) {
        return SemIR::ErrorInst::InstId;
      } else {
        // We bind the input value to the target `FacetType` with a
        // `FacetValue`, which requires an instruction of type `TypeType`. So if
        // we are converting from a facet value, we get its `type` via an extra
        // `FacetAccessType` instruction.
        auto type_inst_id = SemIR::TypeInstId::None;
        if (sem_ir.types().Is<SemIR::FacetType>(value_type_id)) {
          type_inst_id = AddTypeInst<SemIR::FacetAccessType>(
              context, loc_id,
              {.type_id = SemIR::TypeType::TypeId,
               .facet_value_inst_id = const_value_id});
        } else {
          type_inst_id = context.types().GetAsTypeInstId(const_value_id);
        }
        // Note that `FacetValue`'s type is the same `FacetType` that was used
        // to construct the set of witnesses, ie. the query to
        // `LookupImplWitness()`. This ensures that the witnesses are in the
        // same order as the `required_interfaces()` in the `FacetValue`'s type.
        return AddInst<SemIR::FacetValue>(
            context, loc_id,
            {.type_id = target.type_id,
             .type_inst_id = type_inst_id,
             .witnesses_block_id = lookup_result.inst_block_id()});
      }
    } else {
      // If impl lookup fails, don't keep looking for another way to convert.
      // See https://github.com/carbon-language/carbon-lang/issues/5122.
      // TODO: Pass this function into `LookupImplWitness` so it can construct
      // the error add notes explaining failure.
      if (target.diagnose) {
        DiagnoseConversionFailureToConstraintValue(context, loc_id, value_id,
                                                   target.type_id);
      }
      return SemIR::ErrorInst::InstId;
    }
  }

  // No builtin conversion applies.
  return value_id;
}

// Given a value expression, form a corresponding initializer that copies from
// that value, if it is possible to do so.
static auto PerformCopy(Context& context, SemIR::InstId expr_id, bool diagnose)
    -> SemIR::InstId {
  auto expr = context.insts().Get(expr_id);
  auto type_id = expr.type_id();
  if (type_id == SemIR::ErrorInst::TypeId) {
    return SemIR::ErrorInst::InstId;
  }

  if (InitReprIsCopyOfValueRepr(context.sem_ir(), type_id)) {
    // For simple by-value types, no explicit action is required. Initializing
    // from a value expression is treated as copying the value.
    return expr_id;
  }

  // TODO: We don't yet have rules for whether and when a class type is
  // copyable, or how to perform the copy.
  if (diagnose) {
    CARBON_DIAGNOSTIC(CopyOfUncopyableType, Error,
                      "cannot copy value of type {0}", TypeOfInstId);
    context.emitter().Emit(expr_id, CopyOfUncopyableType, expr_id);
  }
  return SemIR::ErrorInst::InstId;
}

// Convert a value expression so that it can be used to initialize a C++ thunk
// parameter.
static auto ConvertValueForCppThunkRef(Context& context, SemIR::InstId expr_id,
                                       bool diagnose) -> SemIR::InstId {
  auto expr = context.insts().Get(expr_id);

  // If the expression has a pointer value representation, extract that and use
  // it directly.
  if (SemIR::ValueRepr::ForType(context.sem_ir(), expr.type_id()).kind ==
      SemIR::ValueRepr::Pointer) {
    return AddInst<SemIR::ValueAsRef>(
        context, SemIR::LocId(expr_id),
        {.type_id = expr.type_id(), .value_id = expr_id});
  }

  // Otherwise, we need a temporary to pass as the thunk argument. Create a copy
  // and initialize a temporary from it.
  expr_id = PerformCopy(context, expr_id, diagnose);
  if (SemIR::GetExprCategory(context.sem_ir(), expr_id) ==
      SemIR::ExprCategory::Value) {
    // If we still have a value expression, then it's a value expression
    // whose value is being used directly to initialize the object. Copy
    // it into a temporary to form an ephemeral reference.
    expr_id = CopyValueToTemporary(context, expr_id);
  }
  return expr_id;
}

auto PerformAction(Context& context, SemIR::LocId loc_id,
                   SemIR::ConvertToValueAction action) -> SemIR::InstId {
  return Convert(context, loc_id, action.inst_id,
                 {.kind = ConversionTarget::Value,
                  .type_id = context.types().GetTypeIdForTypeInstId(
                      action.target_type_inst_id)});
}

auto Convert(Context& context, SemIR::LocId loc_id, SemIR::InstId expr_id,
             ConversionTarget target, SemIR::ClassType* vtable_class_type)
    -> SemIR::InstId {
  auto& sem_ir = context.sem_ir();
  auto orig_expr_id = expr_id;

  // Start by making sure both sides are non-errors. If any part is an error,
  // the result is an error and we shouldn't diagnose.
  if (sem_ir.insts().Get(expr_id).type_id() == SemIR::ErrorInst::TypeId ||
      target.type_id == SemIR::ErrorInst::TypeId) {
    return SemIR::ErrorInst::InstId;
  }

  if (SemIR::GetExprCategory(sem_ir, expr_id) == SemIR::ExprCategory::NotExpr) {
    // TODO: We currently encounter this for use of namespaces and functions.
    // We should provide a better diagnostic for inappropriate use of
    // namespace names, and allow use of functions as values.
    if (target.diagnose) {
      CARBON_DIAGNOSTIC(UseOfNonExprAsValue, Error,
                        "expression cannot be used as a value");
      context.emitter().Emit(expr_id, UseOfNonExprAsValue);
    }
    return SemIR::ErrorInst::InstId;
  }

  // We can only perform initialization for complete, non-abstract types. Note
  // that `RequireConcreteType` returns true for facet types, since their
  // representation is fixed. This allows us to support using the `Self` of an
  // interface inside its definition.
  if (!RequireConcreteType(
          context, target.type_id, loc_id,
          [&] {
            CARBON_CHECK(!target.is_initializer(),
                         "Initialization of incomplete types is expected to be "
                         "caught elsewhere.");
            if (!target.diagnose) {
              return context.emitter().BuildSuppressed();
            }
            CARBON_DIAGNOSTIC(IncompleteTypeInValueConversion, Error,
                              "forming value of incomplete type {0}",
                              SemIR::TypeId);
            CARBON_DIAGNOSTIC(IncompleteTypeInConversion, Error,
                              "invalid use of incomplete type {0}",
                              SemIR::TypeId);
            return context.emitter().Build(
                loc_id,
                target.kind == ConversionTarget::Value
                    ? IncompleteTypeInValueConversion
                    : IncompleteTypeInConversion,
                target.type_id);
          },
          [&] {
            if (!target.diagnose || !target.is_initializer()) {
              return context.emitter().BuildSuppressed();
            }
            CARBON_DIAGNOSTIC(AbstractTypeInInit, Error,
                              "initialization of abstract type {0}",
                              SemIR::TypeId);
            return context.emitter().Build(loc_id, AbstractTypeInInit,
                                           target.type_id);
          })) {
    return SemIR::ErrorInst::InstId;
  }

  // The source type doesn't need to be complete, but its completeness can
  // affect the result. For example, we don't know what type it adapts or
  // derives from unless it's complete.
  // TODO: Is there a risk of coherence problems if the source type is
  // incomplete, but a conversion would have been possible or would have behaved
  // differently if it were complete?
  TryToCompleteType(context, context.insts().Get(expr_id).type_id(), loc_id);

  // Check whether any builtin conversion applies.
  expr_id = PerformBuiltinConversion(context, loc_id, expr_id, target,
                                     vtable_class_type);
  if (expr_id == SemIR::ErrorInst::InstId) {
    return expr_id;
  }

  // Defer the action if it's dependent. We do this now rather than before
  // attempting any conversion so that we can still perform builtin conversions
  // on dependent arguments. This matters for things like converting a
  // `template T:! SomeInterface` to `type`, where it's important to form a
  // `FacetAccessType` when checking the template. But when running the action
  // later, we need to try builtin conversions again, because one may apply that
  // didn't apply in the template definition.
  // TODO: Support this for targets other than `Value`.
  if (sem_ir.insts().Get(expr_id).type_id() != target.type_id &&
      target.kind == ConversionTarget::Value &&
      (OperandIsDependent(context, expr_id) ||
       OperandIsDependent(context, target.type_id))) {
    auto target_type_inst_id = context.types().GetInstId(target.type_id);
    return AddDependentActionSplice(
        context, loc_id,
        SemIR::ConvertToValueAction{.type_id = SemIR::InstType::TypeId,
                                    .inst_id = expr_id,
                                    .target_type_inst_id = target_type_inst_id},
        target_type_inst_id);
  }

  // If this is not a builtin conversion, try an `ImplicitAs` conversion.
  if (sem_ir.insts().Get(expr_id).type_id() != target.type_id) {
    SemIR::InstId interface_args[] = {
        context.types().GetInstId(target.type_id)};
    Operator op = {
        .interface_name = target.kind == ConversionTarget::ExplicitAs
                              ? llvm::StringLiteral("As")
                              : llvm::StringLiteral("ImplicitAs"),
        .interface_args_ref = interface_args,
        .op_name = "Convert",
    };
    expr_id = BuildUnaryOperator(context, loc_id, op, expr_id, [&] {
      if (!target.diagnose) {
        return context.emitter().BuildSuppressed();
      }
      if (target.type_id == SemIR::TypeType::TypeId ||
          sem_ir.types().Is<SemIR::FacetType>(target.type_id)) {
        CARBON_DIAGNOSTIC(
            ConversionFailureNonTypeToFacet, Error,
            "cannot{0:| implicitly} convert non-type value of type {1} "
            "{2:to|into type implementing} {3}{0: with `as`|}",
            Diagnostics::BoolAsSelect, TypeOfInstId, Diagnostics::BoolAsSelect,
            SemIR::TypeId);
        return context.emitter().Build(
            loc_id, ConversionFailureNonTypeToFacet,
            target.kind == ConversionTarget::ExplicitAs, expr_id,
            target.type_id == SemIR::TypeType::TypeId, target.type_id);
      } else {
        CARBON_DIAGNOSTIC(ConversionFailure, Error,
                          "cannot{0:| implicitly} convert expression of type "
                          "{1} to {2}{0: with `as`|}",
                          Diagnostics::BoolAsSelect, TypeOfInstId,
                          SemIR::TypeId);
        return context.emitter().Build(
            loc_id, ConversionFailure,
            target.kind == ConversionTarget::ExplicitAs, expr_id,
            target.type_id);
      }
    });

    // Pull a value directly out of the initializer if possible and wanted.
    if (expr_id != SemIR::ErrorInst::InstId &&
        CanUseValueOfInitializer(sem_ir, target.type_id, target.kind)) {
      expr_id = AddInst<SemIR::ValueOfInitializer>(
          context, loc_id, {.type_id = target.type_id, .init_id = expr_id});
    }
  }

  // Track that we performed a type conversion, if we did so.
  if (orig_expr_id != expr_id) {
    expr_id = AddInst<SemIR::Converted>(context, loc_id,
                                        {.type_id = target.type_id,
                                         .original_id = orig_expr_id,
                                         .result_id = expr_id});
  }

  // For `as`, don't perform any value category conversions. In particular, an
  // identity conversion shouldn't change the expression category.
  if (target.kind == ConversionTarget::ExplicitAs) {
    return expr_id;
  }

  // Now perform any necessary value category conversions.
  switch (SemIR::GetExprCategory(sem_ir, expr_id)) {
    case SemIR::ExprCategory::NotExpr:
    case SemIR::ExprCategory::Mixed:
      CARBON_FATAL("Unexpected expression {0} after builtin conversions",
                   sem_ir.insts().Get(expr_id));

    case SemIR::ExprCategory::Error:
      return SemIR::ErrorInst::InstId;

    case SemIR::ExprCategory::Initializing:
      if (target.is_initializer()) {
        if (orig_expr_id == expr_id) {
          // Don't fill in the return slot if we created the expression through
          // a conversion. In that case, we will have created it with the
          // target already set.
          // TODO: Find a better way to track whether we need to do this.
          MarkInitializerFor(sem_ir, expr_id, target.init_id,
                             *target.init_block);
        }
        break;
      }

      // Commit to using a temporary for this initializing expression.
      // TODO: Don't create a temporary if the initializing representation
      // is already a value representation.
      // TODO: If the target is DurableRef, materialize a VarStorage instead of
      // a TemporaryStorage to lifetime-extend.
      expr_id = FinalizeTemporary(context, expr_id,
                                  target.kind == ConversionTarget::Discarded);
      // We now have an ephemeral reference.
      [[fallthrough]];

    case SemIR::ExprCategory::DurableRef:
      if (target.kind == ConversionTarget::DurableRef) {
        break;
      }
      [[fallthrough]];

    case SemIR::ExprCategory::EphemeralRef:
      // If a reference expression is an acceptable result, we're done.
      if (target.kind == ConversionTarget::ValueOrRef ||
          target.kind == ConversionTarget::Discarded ||
          target.kind == ConversionTarget::CppThunkRef) {
        break;
      }

      // If we have a reference and don't want one, form a value binding.
      // TODO: Support types with custom value representations.
      expr_id = AddInst<SemIR::BindValue>(
          context, SemIR::LocId(expr_id),
          {.type_id = target.type_id, .value_id = expr_id});
      // We now have a value expression.
      [[fallthrough]];

    case SemIR::ExprCategory::Value:
      if (target.kind == ConversionTarget::DurableRef) {
        if (target.diagnose) {
          CARBON_DIAGNOSTIC(ConversionFailureNonRefToRef, Error,
                            "cannot bind durable reference to non-reference "
                            "value of type {0}",
                            SemIR::TypeId);
          context.emitter().Emit(loc_id, ConversionFailureNonRefToRef,
                                 target.type_id);
        }
        return SemIR::ErrorInst::InstId;
      }

      // When initializing from a value, perform a copy.
      if (target.is_initializer()) {
        expr_id = PerformCopy(context, expr_id, target.diagnose);
      }

      // When initializing a C++ thunk parameter, form a reference, creating a
      // temporary if needed.
      if (target.kind == ConversionTarget::CppThunkRef) {
        expr_id = ConvertValueForCppThunkRef(context, expr_id, target.diagnose);
      }

      break;
  }

  // Perform a final destination store, if necessary.
  if (target.kind == ConversionTarget::FullInitializer) {
    if (auto init_rep = SemIR::InitRepr::ForType(sem_ir, target.type_id);
        init_rep.kind == SemIR::InitRepr::ByCopy) {
      target.init_block->InsertHere();
      expr_id = AddInst<SemIR::InitializeFrom>(context, loc_id,
                                               {.type_id = target.type_id,
                                                .src_id = expr_id,
                                                .dest_id = target.init_id});
    }
  }

  return expr_id;
}

auto Initialize(Context& context, SemIR::LocId loc_id, SemIR::InstId target_id,
                SemIR::InstId value_id) -> SemIR::InstId {
  PendingBlock target_block(&context);
  return Convert(context, loc_id, value_id,
                 {.kind = ConversionTarget::Initializer,
                  .type_id = context.insts().Get(target_id).type_id(),
                  .init_id = target_id,
                  .init_block = &target_block});
}

auto ConvertToValueExpr(Context& context, SemIR::InstId expr_id)
    -> SemIR::InstId {
  return Convert(context, SemIR::LocId(expr_id), expr_id,
                 {.kind = ConversionTarget::Value,
                  .type_id = context.insts().Get(expr_id).type_id()});
}

auto ConvertToValueOrRefExpr(Context& context, SemIR::InstId expr_id)
    -> SemIR::InstId {
  return Convert(context, SemIR::LocId(expr_id), expr_id,
                 {.kind = ConversionTarget::ValueOrRef,
                  .type_id = context.insts().Get(expr_id).type_id()});
}

auto ConvertToValueOfType(Context& context, SemIR::LocId loc_id,
                          SemIR::InstId expr_id, SemIR::TypeId type_id)
    -> SemIR::InstId {
  return Convert(context, loc_id, expr_id,
                 {.kind = ConversionTarget::Value, .type_id = type_id});
}

auto ConvertToValueOrRefOfType(Context& context, SemIR::LocId loc_id,
                               SemIR::InstId expr_id, SemIR::TypeId type_id)
    -> SemIR::InstId {
  return Convert(context, loc_id, expr_id,
                 {.kind = ConversionTarget::ValueOrRef, .type_id = type_id});
}

// Like ConvertToValueOfType but failure to convert does not result in
// diagnostics. An ErrorInst instruction is still returned on failure.
auto TryConvertToValueOfType(Context& context, SemIR::LocId loc_id,
                             SemIR::InstId expr_id, SemIR::TypeId type_id)
    -> SemIR::InstId {
  return Convert(
      context, loc_id, expr_id,
      {.kind = ConversionTarget::Value, .type_id = type_id, .diagnose = false});
}

auto ConvertToBoolValue(Context& context, SemIR::LocId loc_id,
                        SemIR::InstId value_id) -> SemIR::InstId {
  return ConvertToValueOfType(
      context, loc_id, value_id,
      GetSingletonType(context, SemIR::BoolType::TypeInstId));
}

auto ConvertForExplicitAs(Context& context, Parse::NodeId as_node,
                          SemIR::InstId value_id, SemIR::TypeId type_id)
    -> SemIR::InstId {
  return Convert(context, as_node, value_id,
                 {.kind = ConversionTarget::ExplicitAs, .type_id = type_id});
}

// TODO: Consider moving this to pattern_match.h.
auto ConvertCallArgs(Context& context, SemIR::LocId call_loc_id,
                     SemIR::InstId self_id,
                     llvm::ArrayRef<SemIR::InstId> arg_refs,
                     SemIR::InstId return_slot_arg_id,
                     const SemIR::Function& callee,
                     SemIR::SpecificId callee_specific_id)
    -> SemIR::InstBlockId {
  auto param_patterns =
      context.inst_blocks().GetOrEmpty(callee.param_patterns_id);
  auto return_slot_pattern_id = callee.return_slot_pattern_id;

  // The caller should have ensured this callee has the right arity.
  CARBON_CHECK(arg_refs.size() == param_patterns.size());

  if (callee.self_param_id.has_value() && !self_id.has_value()) {
    CARBON_DIAGNOSTIC(MissingObjectInMethodCall, Error,
                      "missing object argument in method call");
    CARBON_DIAGNOSTIC(InCallToFunction, Note, "calling function declared here");
    context.emitter()
        .Build(call_loc_id, MissingObjectInMethodCall)
        .Note(callee.latest_decl_id(), InCallToFunction)
        .Emit();
    self_id = SemIR::ErrorInst::InstId;
  }

  return CallerPatternMatch(context, callee_specific_id, callee.self_param_id,
                            callee.param_patterns_id, return_slot_pattern_id,
                            self_id, arg_refs, return_slot_arg_id);
}

auto ExprAsType(Context& context, SemIR::LocId loc_id, SemIR::InstId value_id,
                bool diagnose) -> TypeExpr {
  auto type_inst_id =
      ConvertToValueOfType(context, loc_id, value_id, SemIR::TypeType::TypeId);
  if (type_inst_id == SemIR::ErrorInst::InstId) {
    return {.inst_id = SemIR::ErrorInst::TypeInstId,
            .type_id = SemIR::ErrorInst::TypeId};
  }

  auto type_const_id = context.constant_values().Get(type_inst_id);
  if (!type_const_id.is_constant()) {
    if (diagnose) {
      CARBON_DIAGNOSTIC(TypeExprEvaluationFailure, Error,
                        "cannot evaluate type expression");
      context.emitter().Emit(loc_id, TypeExprEvaluationFailure);
    }
    return {.inst_id = SemIR::ErrorInst::TypeInstId,
            .type_id = SemIR::ErrorInst::TypeId};
  }

  return {.inst_id = context.types().GetAsTypeInstId(type_inst_id),
          .type_id = context.types().GetTypeIdForTypeConstantId(type_const_id)};
}

auto DiscardExpr(Context& context, SemIR::InstId expr_id) -> void {
  // If we discard an initializing expression, convert it to a value or
  // reference so that it has something to initialize.
  auto expr = context.insts().Get(expr_id);
  Convert(context, SemIR::LocId(expr_id), expr_id,
          {.kind = ConversionTarget::Discarded, .type_id = expr.type_id()});

  // TODO: This will eventually need to do some "do not discard" analysis.
}

}  // namespace Carbon::Check

// NOLINTEND(misc-no-recursion)
