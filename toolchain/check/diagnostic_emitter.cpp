// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/diagnostic_emitter.h"

#include <algorithm>
#include <optional>
#include <string>

#include "common/raw_string_ostream.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/sem_ir/absolute_node_id.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/stringify.h"

namespace Carbon::Check {

auto DiagnosticEmitter::ConvertLoc(LocIdForDiagnostics loc_id,
                                   ContextFnT context_fn) const
    -> Diagnostics::ConvertedLoc {
  auto [imports, converted] = loc_converter_.ConvertWithImports(
      loc_id.loc_id(), loc_id.is_token_only());
  for (const auto& import : imports) {
    CARBON_DIAGNOSTIC(InImport, LocationInfo, "in import");
    context_fn(import.loc, InImport);
  }

  // Use the token when possible, but -1 is the default value.
  auto last_offset = -1;
  if (last_token_.has_value()) {
    last_offset = sem_ir_->parse_tree().tokens().GetByteOffset(last_token_);
  }

  // When the diagnostic is in the same file, we use the last possible offset;
  // otherwise, we ignore the offset because it's probably in that file.
  if (converted.loc.filename == sem_ir_->filename()) {
    converted.last_byte_offset =
        std::max(converted.last_byte_offset, last_offset);
  } else {
    converted.last_byte_offset = last_offset;
  }

  return converted;
}

auto DiagnosticEmitter::ConvertArg(llvm::Any arg) const -> llvm::Any {
  if (auto* library_name_id = llvm::any_cast<SemIR::LibraryNameId>(&arg)) {
    std::string library_name;
    if (*library_name_id == SemIR::LibraryNameId::Default) {
      library_name = "default library";
    } else if (!library_name_id->has_value()) {
      library_name = "library <none>";
    } else {
      RawStringOstream stream;
      stream << "library \""
             << sem_ir_->string_literal_values().Get(
                    library_name_id->AsStringLiteralValueId())
             << "\"";
      library_name = stream.TakeStr();
    }
    return library_name;
  }
  if (auto* name_id = llvm::any_cast<SemIR::NameId>(&arg)) {
    return sem_ir_->names().GetFormatted(*name_id).str();
  }
  if (auto* type_of_expr = llvm::any_cast<TypeOfInstId>(&arg)) {
    if (!type_of_expr->inst_id.has_value()) {
      return "<none>";
    }
    // TODO: Where possible, produce a better description of the type based on
    // the expression.
    return "`" +
           StringifyConstantInst(
               *sem_ir_,
               sem_ir_->types().GetInstId(
                   sem_ir_->insts().Get(type_of_expr->inst_id).type_id())) +
           "`";
  }
  if (auto* expr = llvm::any_cast<InstIdAsConstant>(&arg)) {
    return "`" + StringifyConstantInst(*sem_ir_, expr->inst_id) + "`";
  }
  if (auto* type_expr = llvm::any_cast<InstIdAsRawType>(&arg)) {
    return StringifyConstantInst(*sem_ir_, type_expr->inst_id);
  }
  if (auto* type = llvm::any_cast<TypeIdAsRawType>(&arg)) {
    return StringifyConstantInst(*sem_ir_,
                                 sem_ir_->types().GetInstId(type->type_id));
  }
  if (auto* type_id = llvm::any_cast<SemIR::TypeId>(&arg)) {
    return "`" +
           StringifyConstantInst(*sem_ir_,
                                 sem_ir_->types().GetInstId(*type_id)) +
           "`";
  }
  if (auto* specific_id = llvm::any_cast<SemIR::SpecificId>(&arg)) {
    return "`" + StringifySpecific(*sem_ir_, *specific_id) + "`";
  }
  if (auto* typed_int = llvm::any_cast<TypedInt>(&arg)) {
    return llvm::APSInt(typed_int->value,
                        !sem_ir_->types().IsSignedInt(typed_int->type));
  }
  if (auto* specific_interface_id =
          llvm::any_cast<SemIR::SpecificInterfaceId>(&arg)) {
    auto specific_interface =
        sem_ir_->specific_interfaces().Get(*specific_interface_id);
    return "`" + StringifySpecificInterface(*sem_ir_, specific_interface) + "`";
  }
  if (auto* specific_interface_raw =
          llvm::any_cast<SpecificInterfaceIdAsRawType>(&arg)) {
    auto specific_interface = sem_ir_->specific_interfaces().Get(
        specific_interface_raw->specific_interface_id);
    return StringifySpecificInterface(*sem_ir_, specific_interface);
  }
  return DiagnosticEmitterBase::ConvertArg(arg);
}

}  // namespace Carbon::Check
