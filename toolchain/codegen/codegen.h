// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_CODEGEN_CODEGEN_H_
#define CARBON_TOOLCHAIN_CODEGEN_CODEGEN_H_

#include "llvm/IR/Module.h"
#include "llvm/Target/TargetMachine.h"
#include "toolchain/diagnostics/diagnostic_consumer.h"
#include "toolchain/diagnostics/file_diagnostics.h"

namespace Carbon {

class CodeGen {
 public:
  // `module` and `errors` must not be null. `consumer` may be null, in which
  // case diagnostics go to stderr.
  static auto Make(llvm::Module* module, llvm::StringRef target_triple_str,
                   Diagnostics::Consumer* consumer = nullptr)
      -> std::optional<CodeGen>;

  // Generates the object code file.
  // Returns false in case of failure, and any information about the failure is
  // printed to the error stream.
  //
  // Note this requires a `pwrite` stream to allow patching the output.
  auto EmitObject(llvm::raw_pwrite_stream& out) -> bool;

  // Prints the assembly to stdout.
  // Returns false in case of failure, and any information about the failure is
  // printed to the error stream.
  //
  // Note this requires a `pwrite` stream to allow patching the output.
  auto EmitAssembly(llvm::raw_pwrite_stream& out) -> bool;

 private:
  // `module` and `consumer` must not be null.
  explicit CodeGen(llvm::Module* module, Diagnostics::Consumer* consumer)
      : module_(module), emitter_(consumer) {}

  // Using the llvm pass emits either assembly or object code to dest.
  // Returns false in case of failure, and any information about the failure is
  // printed to the error stream.
  auto EmitCode(llvm::raw_pwrite_stream& out, llvm::CodeGenFileType file_type)
      -> bool;

  llvm::Module* module_;

  // The emitter for diagnostics.
  Diagnostics::FileEmitter emitter_;

  std::unique_ptr<llvm::TargetMachine> target_machine_;
};

}  // namespace Carbon

#endif  // CARBON_TOOLCHAIN_CODEGEN_CODEGEN_H_
