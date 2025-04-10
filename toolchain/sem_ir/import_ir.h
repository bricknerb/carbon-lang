// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEM_IR_IMPORT_IR_H_
#define CARBON_TOOLCHAIN_SEM_IR_IMPORT_IR_H_

#include "llvm/ADT/FoldingSet.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"

namespace Carbon::SemIR {

// A reference to an imported IR.
struct ImportIR : public Printable<ImportIR> {
  auto Print(llvm::raw_ostream& out) const -> void {
    out << "{decl_id: " << decl_id
        << ", is_export: " << (is_export ? "true" : "false") << "}";
  }

  // The `import` declaration.
  InstId decl_id;
  // True if this is part of an `export import`.
  bool is_export;
  // The imported IR.
  const File* sem_ir;
};

static_assert(sizeof(ImportIR) == 8 + sizeof(uintptr_t), "Unexpected size");

// A reference to an instruction in an imported IR. Used for diagnostics with
// LocId. For `Cpp` import, points to a Clang source location.
struct ImportIRInst : public Printable<ImportIRInst> {
  auto Print(llvm::raw_ostream& out) const -> void {
    out << "{ir_id: " << ir_id << ", ";
    if (ir_id == ImportIRId::Cpp) {
      out << "clang_source_location_id: " << clang_source_location_id;
    } else {
      out << "inst_id: " << inst_id;
    }
    out << "}";
  }

  friend auto operator==(const ImportIRInst& lhs, const ImportIRInst& rhs)
      -> bool {
    return lhs.ir_id == rhs.ir_id &&
           (lhs.ir_id == ImportIRId::Cpp
                ? lhs.clang_source_location_id == rhs.clang_source_location_id
                : lhs.inst_id == rhs.inst_id);
  }

  ImportIRId ir_id;
  union {
    // Set iff `ir_id != ImportIRId::Cpp`.
    InstId inst_id;

    // Set iff `ir_id == ImportIRId::Cpp`.
    ClangSourceLocationId clang_source_location_id;
  };
};

}  // namespace Carbon::SemIR

#endif  // CARBON_TOOLCHAIN_SEM_IR_IMPORT_IR_H_
