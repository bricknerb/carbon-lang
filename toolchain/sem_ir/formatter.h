// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEM_IR_FORMATTER_H_
#define CARBON_TOOLCHAIN_SEM_IR_FORMATTER_H_

#include "llvm/Support/raw_ostream.h"
#include "toolchain/lex/tokenized_buffer.h"
#include "toolchain/parse/tree.h"
#include "toolchain/sem_ir/file.h"
#include "toolchain/sem_ir/inst_namer.h"

namespace Carbon::SemIR {

// Formatter for printing textual Semantics IR.
class Formatter {
 public:
  // A callback that indicates whether a specific entity, identified by its
  // declaration, should be included in the output.
  using ShouldFormatEntityFn =
      llvm::function_ref<auto(InstId decl_inst_id)->bool>;

  explicit Formatter(
      const File* sem_ir,
      ShouldFormatEntityFn should_format_entity = [](InstId) { return true; });
  ~Formatter();

  // Prints the full IR.
  auto Print(llvm::raw_ostream& out) -> void;

 private:
  const File* sem_ir_;
  ShouldFormatEntityFn should_format_entity_;
  // Caches naming between Print calls.
  InstNamer inst_namer_;
};

}  // namespace Carbon::SemIR

#endif  // CARBON_TOOLCHAIN_SEM_IR_FORMATTER_H_
