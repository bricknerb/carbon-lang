// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/lower/function_context.h"
#include "toolchain/sem_ir/expr_info.h"
#include "toolchain/sem_ir/file.h"

namespace Carbon::Lower {

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::BindValue inst) -> void {
  auto inst_type = context.GetTypeIdOfInst(inst_id);
  switch (context.GetValueRepr(inst_type).repr.kind) {
    case SemIR::ValueRepr::Unknown:
      CARBON_FATAL(
          "Value binding for type with incomplete value representation");
    case SemIR::ValueRepr::None:
      // Nothing should use this value, but StubRef needs a value to
      // propagate.
      // TODO: Remove this now the StubRefs are gone.
      context.SetLocal(inst_id,
                       llvm::PoisonValue::get(context.GetType(inst_type)));
      break;
    case SemIR::ValueRepr::Copy: {
      auto* type = context.GetType(inst_type);
      context.SetLocal(inst_id, context.builder().CreateLoad(
                                    type, context.GetValue(inst.value_id)));
    } break;
    case SemIR::ValueRepr::Pointer:
      context.SetLocal(inst_id, context.GetValue(inst.value_id));
      break;
    case SemIR::ValueRepr::Custom:
      CARBON_FATAL("TODO: Add support for BindValue with custom value rep");
  }
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::Temporary inst) -> void {
  context.FinishInit(context.GetTypeIdOfInst(inst_id), inst.storage_id,
                     inst.init_id);
  context.SetLocal(inst_id, context.GetValue(inst.storage_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::TemporaryStorage /*inst*/) -> void {
  context.SetLocal(
      inst_id, context.CreateAlloca(context.GetTypeOfInst(inst_id), "temp"));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::ValueAsRef inst) -> void {
  CARBON_CHECK(SemIR::GetExprCategory(context.sem_ir(), inst.value_id) ==
               SemIR::ExprCategory::Value);
  auto inst_type = context.GetTypeIdOfInst(inst_id);
  auto value_repr = context.GetValueRepr(inst_type);
  CARBON_CHECK(value_repr.repr.kind == SemIR::ValueRepr::Pointer);
  context.SetLocal(inst_id, context.GetValue(inst.value_id));
}

auto HandleInst(FunctionContext& context, SemIR::InstId inst_id,
                SemIR::ValueOfInitializer inst) -> void {
  CARBON_CHECK(SemIR::GetExprCategory(context.sem_ir(), inst.init_id) ==
               SemIR::ExprCategory::Initializing);
  auto inst_type = context.GetTypeIdOfInst(inst_id);
  auto value_repr = context.GetValueRepr(inst_type);
  auto init_repr = context.GetInitRepr(inst_type);
  CARBON_CHECK(value_repr.repr.kind == SemIR::ValueRepr::Copy);
  CARBON_CHECK(init_repr.kind == SemIR::InitRepr::ByCopy);
  context.SetLocal(inst_id, context.GetValue(inst.init_id));
}

}  // namespace Carbon::Lower
