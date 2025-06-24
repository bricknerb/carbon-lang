// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "toolchain/lower/function_context.h"
#include "toolchain/sem_ir/builtin_function_kind.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Lower {

// Returns whether this instruction names a namespace.
static auto IsNamespace(FunctionContext& context, SemIR::InstId inst_id)
    -> bool {
  // Note, we don't use context.GetTypeOfInst here. An instruction can't change
  // from being a non-namespace in a generic to being a namespace in a specific,
  // because namespace names are not first-class.
  auto type_inst_id = context.sem_ir().types().GetInstId(
      context.sem_ir().insts().Get(inst_id).type_id());
  return type_inst_id == SemIR::NamespaceType::TypeInstId;
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::AddrOf inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.lvalue_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::ArrayIndex inst) -> void {
  auto* array_value = context.GetValue(inst.array_id);
  auto* llvm_type = context.GetTypeOfInst(inst.array_id);

  // The index in an `ArrayIndex` can be of any integer type, including
  // IntLiteral. If it is an IntLiteral, its value representation is empty, so
  // create a ConstantInt from its SemIR value directly.
  llvm::Value* index;
  auto index_type = context.GetTypeIdOfInst(inst.index_id);
  if (index_type.file->types().GetInstId(index_type.type_id) ==
      SemIR::IntLiteralType::TypeInstId) {
    auto value = context.sem_ir().insts().GetAs<SemIR::IntValue>(
        context.sem_ir().constant_values().GetConstantInstId(inst.index_id));
    const auto& apint_value = context.sem_ir().ints().Get(value.int_id);
    context.AddIntToCurrentFingerprint(apint_value.getSExtValue());
    index = llvm::ConstantInt::get(context.llvm_context(), apint_value);
  } else {
    context.AddIntToCurrentFingerprint(-1);
    index = context.GetValue(inst.index_id);
  }

  llvm::Value* indexes[2] = {
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvm_context()), 0),
      index};
  context.SetLocal(inst_id,
                   context.builder().CreateInBoundsGEP(llvm_type, array_value,
                                                       indexes, "array.index"));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::ArrayInit inst) -> void {
  // The result of initialization is the return slot of the initializer.
  context.SetLocal(inst_id, context.GetValue(inst.dest_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::AsCompatible inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.source_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::Assign inst) -> void {
  context.FinishInit(context.GetTypeIdOfInst(inst.lhs_id), inst.lhs_id,
                     inst.rhs_id);
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::BindAlias inst) -> void {
  if (IsNamespace(context, inst_id)) {
    return;
  }

  context.SetLocal(inst_id, context.GetValue(inst.value_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::ExportDecl inst) -> void {
  if (IsNamespace(context, inst_id)) {
    return;
  }

  context.SetLocal(inst_id, context.GetValue(inst.value_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::BindName inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.value_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::BindSymbolicName inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.value_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::BlockArg inst) -> void {
  context.SetLocal(
      inst_id,
      context.GetBlockArg(inst.block_id, context.GetTypeIdOfInst(inst_id)));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::BoundMethod inst) -> void {
  // Propagate just the function; the object is separately provided to the
  // enclosing call as an implicit argument.
  context.SetLocal(inst_id, context.GetValue(inst.function_decl_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::Branch inst) -> void {
  // Opportunistically avoid creating a BasicBlock that contains just a branch.
  // TODO: Don't do this if it would remove a loop preheader block.
  llvm::BasicBlock* block = context.builder().GetInsertBlock();
  if (block->empty() && context.TryToReuseBlock(inst.target_id, block)) {
    // Reuse this block as the branch target.
  } else {
    context.builder().CreateBr(context.GetBlock(inst.target_id));
  }

  context.builder().ClearInsertionPoint();
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::BranchIf inst) -> void {
  llvm::Value* cond = context.GetValue(inst.cond_id);
  llvm::BasicBlock* then_block = context.GetBlock(inst.target_id);
  llvm::BasicBlock* else_block = context.MakeSyntheticBlock();
  context.builder().CreateCondBr(cond, then_block, else_block);
  context.builder().SetInsertPoint(else_block);
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::BranchWithArg inst) -> void {
  llvm::Value* arg = context.GetValue(inst.arg_id);
  auto arg_type = context.GetTypeIdOfInst(inst.arg_id);

  // Opportunistically avoid creating a BasicBlock that contains just a branch.
  // We only do this for a block that we know will only have a single
  // predecessor, so that we can correctly populate the predecessors of the
  // PHINode.
  llvm::BasicBlock* block = context.builder().GetInsertBlock();
  llvm::BasicBlock* phi_predecessor = block;
  if (block->empty() && context.IsCurrentSyntheticBlock(block) &&
      context.TryToReuseBlock(inst.target_id, block)) {
    // Reuse this block as the branch target.
    phi_predecessor = block->getSinglePredecessor();
    CARBON_CHECK(phi_predecessor,
                 "Synthetic block did not have a single predecessor");
  } else {
    context.builder().CreateBr(context.GetBlock(inst.target_id));
  }

  context.GetBlockArg(inst.target_id, arg_type)
      ->addIncoming(arg, phi_predecessor);
  context.builder().ClearInsertionPoint();
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::Converted inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.result_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::Deref inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.pointer_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::FacetAccessType /*inst*/) -> void {
  context.SetLocal(inst_id, context.GetTypeAsValue());
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::InitializeFrom inst) -> void {
  context.FinishInit(context.GetTypeIdOfInst(inst.dest_id), inst.dest_id,
                     inst.src_id);
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::NameBindingDecl /*inst*/) -> void {
  // A NameBindingDecl is lowered by pattern matching.
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::NameRef inst) -> void {
  if (IsNamespace(context, inst_id)) {
    return;
  }

  auto inner_inst_id = inst.value_id;

  if (auto bind_name =
          context.sem_ir().insts().TryGetAs<SemIR::BindName>(inner_inst_id)) {
    inner_inst_id = bind_name->value_id;
  }

  context.SetLocal(inst_id, context.GetValue(inner_inst_id));
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::OutParam /*inst*/) -> void {
  // Parameters are lowered by `BuildFunctionDefinition`.
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::RefParam /*inst*/) -> void {
  // Parameters are lowered by `BuildFunctionDefinition`.
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::ValueParam /*inst*/) -> void {
  // Parameters are lowered by `BuildFunctionDefinition`.
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::ReturnSlot inst) -> void {
  if (context.GetInitRepr(context.GetTypeIdOfInst(inst_id)).kind ==
      SemIR::InitRepr::InPlace) {
    context.SetLocal(inst_id, context.GetValue(inst.storage_id));
  }
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::Return /*inst*/) -> void {
  context.builder().CreateRetVoid();
}

auto HandleInst(FunctionContext& context, SemIR::InstId /*inst_id*/,
                SemIR::ReturnExpr inst) -> void {
  auto result_type = context.GetTypeIdOfInst(inst.expr_id);
  switch (context.GetInitRepr(result_type).kind) {
    case SemIR::InitRepr::None:
      // Nothing to return.
      context.builder().CreateRetVoid();
      return;
    case SemIR::InitRepr::InPlace:
      context.FinishInit(result_type, inst.dest_id, inst.expr_id);
      context.builder().CreateRetVoid();
      return;
    case SemIR::InitRepr::ByCopy:
      // The expression produces the value representation for the type.
      context.builder().CreateRet(context.GetValue(inst.expr_id));
      return;
    case SemIR::InitRepr::Incomplete:
      CARBON_FATAL("Lowering return of incomplete type {0}",
                   result_type.file->types().GetAsInst(result_type.type_id));
  }
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::SpecificFunction /*inst*/) -> void {
  // Nothing to do. This value should never be consumed.
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::SpecificImplFunction /*inst*/) -> void {
  // Nothing to do. This value should never be consumed.
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::SpliceBlock inst) -> void {
  context.LowerBlockContents(inst.block_id);
  context.SetLocal(inst_id, context.GetValue(inst.result_id));
}

auto HandleInst(FunctionContext& /*context*/, SemIR::InstId /*inst_id*/,
                SemIR::SpliceInst /*inst*/) -> void {
  // TODO: Get the constant value of the spliced instruction from the current
  // specific, and lower the instruction in that constant value.
  CARBON_FATAL("Template lowering not implemented yet");
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::UnaryOperatorNot inst) -> void {
  context.SetLocal(
      inst_id, context.builder().CreateNot(context.GetValue(inst.operand_id)));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::VarStorage /* inst */) -> void {
  auto* type = context.GetTypeOfInst(inst_id);

  // Position the first alloca right before the start of the executable code in
  // the function.
  llvm::AllocaInst* alloca;
  {
    llvm::IRBuilderBase::InsertPointGuard guard(context.builder());

    auto debug_loc = context.builder().getCurrentDebugLocation();
    if (auto* after_allocas = context.GetInstructionAfterAllocas()) {
      context.builder().SetInsertPoint(after_allocas);
    } else {
      context.builder().SetInsertPointPastAllocas(&context.llvm_function());
    }

    // IRBuilder tramples over our debug location when setting the insert point,
    // so undo that.
    context.builder().SetCurrentDebugLocation(debug_loc);

    // Create an alloca for this variable in the entry block.
    alloca = context.builder().CreateAlloca(type);
  }

  // Create a lifetime start intrinsic here to indicate where its scope really
  // begins.
  auto size = context.llvm_module().getDataLayout().getTypeAllocSize(type);
  context.builder().CreateLifetimeStart(
      alloca,
      llvm::ConstantInt::get(context.llvm_context(), llvm::APInt(64, size)));

  // If we just created the first alloca, there is now definitely at least one
  // instruction after it -- there is a lifetime start instruction if nothing
  // else. Use that instruction as our insert point for all future allocas.
  if (!context.GetInstructionAfterAllocas()) {
    auto loc = alloca->getIterator();
    ++loc;
    context.SetInstructionAfterAllocas(&*loc);
  }

  // TODO: Create a matching `@llvm.lifetime.end` intrinsic call when the
  // variable goes out of scope.

  context.SetLocal(inst_id, alloca);
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::VtablePtr inst) -> void {
  context.SetLocal(inst_id, context.GetValue(inst.vtable_id));
}

}  // namespace Carbon::Lower
