// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/sem_ir_diagnostic_converter.h"

namespace Carbon::Check {

auto SemIRDiagnosticConverter::ConvertLoc(SemIRLoc loc,
                                          ContextFnT context_fn) const
    -> DiagnosticLoc {
  // Cursors for the current IR and instruction in that IR.
  const auto* cursor_ir = sem_ir_;
  auto cursor_inst_id = SemIR::InstId::Invalid;

  // Notes an import on the diagnostic and updates cursors to point at the
  // imported IR.
  auto follow_import_ref = [&](SemIR::ImportIRInstId import_ir_inst_id) {
    auto import_ir_inst = cursor_ir->import_ir_insts().Get(import_ir_inst_id);
    const auto& import_ir = cursor_ir->import_irs().Get(import_ir_inst.ir_id);
    auto context_loc = ConvertLocInFile(cursor_ir, import_ir.node_id,
                                        loc.token_only, context_fn);
    CARBON_DIAGNOSTIC(InImport, Note, "In import.");
    context_fn(context_loc, InImport);
    cursor_ir = import_ir.sem_ir;
    cursor_inst_id = import_ir_inst.inst_id;
  };

  // If the location is is an import, follows it and returns nullopt.
  // Otherwise, it's a parse node, so return the final location.
  auto handle_loc = [&](SemIR::LocId loc_id) -> std::optional<DiagnosticLoc> {
    if (loc_id.is_import_ir_inst_id()) {
      follow_import_ref(loc_id.import_ir_inst_id());
      return std::nullopt;
    } else {
      // Parse nodes always refer to the current IR.
      return ConvertLocInFile(cursor_ir, loc_id.node_id(), loc.token_only,
                              context_fn);
    }
  };

  // Handle the base location.
  if (loc.is_inst_id) {
    cursor_inst_id = loc.inst_id;
  } else {
    if (auto diag_loc = handle_loc(loc.loc_id)) {
      return *diag_loc;
    }
    CARBON_CHECK(cursor_inst_id.is_valid()) << "Should have been set";
  }

  while (true) {
    if (cursor_inst_id.is_valid()) {
      auto cursor_inst = cursor_ir->insts().Get(cursor_inst_id);
      if (auto bind_ref = cursor_inst.TryAs<SemIR::ExportDecl>();
          bind_ref && bind_ref->value_id.is_valid()) {
        cursor_inst_id = bind_ref->value_id;
        continue;
      }

      // If the parse node is valid, use it for the location.
      if (auto loc_id = cursor_ir->insts().GetLocId(cursor_inst_id);
          loc_id.is_valid()) {
        if (auto diag_loc = handle_loc(loc_id)) {
          return *diag_loc;
        }
        continue;
      }

      // If a namespace has an instruction for an import, switch to looking at
      // it.
      if (auto ns = cursor_inst.TryAs<SemIR::Namespace>()) {
        if (ns->import_id.is_valid()) {
          cursor_inst_id = ns->import_id;
          continue;
        }
      }
    }

    // Invalid parse node but not an import; just nothing to point at.
    return ConvertLocInFile(cursor_ir, Parse::NodeId::Invalid, loc.token_only,
                            context_fn);
  }
}

auto SemIRDiagnosticConverter::ConvertArg(llvm::Any arg) const -> llvm::Any {
  if (auto* name_id = llvm::any_cast<SemIR::NameId>(&arg)) {
    return sem_ir_->names().GetFormatted(*name_id).str();
  }
  if (auto* type_id = llvm::any_cast<SemIR::TypeId>(&arg)) {
    return sem_ir_->StringifyType(*type_id);
  }
  if (auto* typed_int = llvm::any_cast<TypedInt>(&arg)) {
    return llvm::APSInt(typed_int->value,
                        !sem_ir_->types().IsSignedInt(typed_int->type));
  }
  return DiagnosticConverter<SemIRLoc>::ConvertArg(arg);
}

auto SemIRDiagnosticConverter::ConvertLocInFile(const SemIR::File* sem_ir,
                                                Parse::NodeId node_id,
                                                bool token_only,
                                                ContextFnT context_fn) const
    -> DiagnosticLoc {
  return node_converters_[sem_ir->check_ir_id().index]->ConvertLoc(
      Parse::NodeLoc(node_id, token_only), context_fn);
}

}  // namespace Carbon::Check