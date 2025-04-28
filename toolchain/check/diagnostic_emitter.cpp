// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/diagnostic_emitter.h"

#include <algorithm>
#include <optional>
#include <string>

#include "common/raw_string_ostream.h"
#include "toolchain/sem_ir/absolute_node_id.h"
#include "toolchain/sem_ir/stringify.h"

namespace Carbon::Check {

auto DiagnosticEmitter::ConvertLoc(SemIR::LocId loc_id,
                                   ContextFnT context_fn) const
    -> Diagnostics::ConvertedLoc {
  // TODO: Instead of special casing Clang location here, support it within
  // `GetAbsoluteNodeId()`. See discussion in
  // https://github.com/carbon-language/carbon-lang/pull/5262/files/20a3f9dcfab5c6f6c5089554fd5e22d5f1ca75a3#r2040308805.
  auto converted_clang_loc = TryConvertClangDiagnosticLoc(loc_id);
  if (converted_clang_loc) {
    return *converted_clang_loc;
  }

  auto converted = ConvertLocImpl(loc_id, context_fn);

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

auto DiagnosticEmitter::ConvertLocImpl(SemIR::LocId loc_id,
                                       ContextFnT context_fn) const
    -> Diagnostics::ConvertedLoc {
  llvm::SmallVector<SemIR::AbsoluteNodeId> absolute_node_ids =
      SemIR::GetAbsoluteNodeId(sem_ir_, loc_id);
  bool token_only =
      loc_id.kind() != SemIR::LocId::Kind::InstId && loc_id.is_token_only();

  auto final_node_id = absolute_node_ids.pop_back_val();
  for (const auto& absolute_node_id : absolute_node_ids) {
    if (!absolute_node_id.node_id.has_value()) {
      // TODO: Add an "In implicit import of prelude." note for the case where
      // we don't have a location.
      continue;
    }
    // TODO: Include the name of the imported library in the diagnostic.
    auto diag_loc = ConvertLocInFile(absolute_node_id, token_only, context_fn);
    CARBON_DIAGNOSTIC(InImport, LocationInfo, "in import");
    context_fn(diag_loc.loc, InImport);
  }

  return ConvertLocInFile(final_node_id, token_only, context_fn);
}

auto DiagnosticEmitter::TryConvertClangDiagnosticLoc(SemIR::LocId loc_id) const
    -> std::optional<Diagnostics::ConvertedLoc> {
  if (loc_id.kind() != SemIR::LocId::Kind::ImportIRInstId) {
    return std::nullopt;
  }

  SemIR::ImportIRInst import_ir_inst =
      sem_ir_->import_ir_insts().Get(loc_id.import_ir_inst_id());

  if (import_ir_inst.ir_id() != SemIR::ImportIRId::Cpp) {
    return std::nullopt;
  }

  clang::SourceLocation clang_loc =
      sem_ir_->clang_source_locs().Get(import_ir_inst.clang_source_loc_id());

  CARBON_CHECK(sem_ir_->cpp_ast());
  clang::PresumedLoc presumed_loc =
      sem_ir_->cpp_ast()->getSourceManager().getPresumedLoc(clang_loc);

  return Diagnostics::ConvertedLoc{
      .loc = {.filename = presumed_loc.getFilename(),
              .line_number = static_cast<int32_t>(presumed_loc.getLine())},
      // TODO: Set `last_byte_offset` based on the `import Cpp` location.
      .last_byte_offset = 0};
}

auto DiagnosticEmitter::ConvertLocInFile(SemIR::AbsoluteNodeId absolute_node_id,
                                         bool token_only,
                                         ContextFnT /*context_fn*/) const
    -> Diagnostics::ConvertedLoc {
  const auto& tree_and_subtrees =
      tree_and_subtrees_getters_[absolute_node_id.check_ir_id.index]();
  return tree_and_subtrees.NodeToDiagnosticLoc(absolute_node_id.node_id,
                                               token_only);
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
  return DiagnosticEmitterBase::ConvertArg(arg);
}

}  // namespace Carbon::Check
