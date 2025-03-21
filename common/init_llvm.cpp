// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "common/init_llvm.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

namespace Carbon {

InitLLVM::InitLLVM(int& argc, char**& argv)
    : init_llvm_(argc, argv),
      // LLVM assumes that argc and argv won't change, and registers them with
      // an `llvm::PrettyStackTraceProgram` that will crash if an argv element
      // gets nulled out, which for example `testing::InitGoogleTest` does. So
      // make a copy of the argv that LLVM produces in order to support
      // mutation.
      args_(argv, argv + argc) {
  // `argv[argc]` is expected to be a null pointer (may reallocate `args_`).
  args_.push_back(nullptr);

  // Return our mutable copy of argv for the program to use.
  argc = args_.size() - 1;
  argv = args_.data();

  llvm::setBugReportMsg(
      "Please report issues to "
      "https://github.com/carbon-language/carbon-lang/issues and include the "
      "crash backtrace.\n");

  // Initialize LLVM targets if //common:all_llvm_targets was linked in.
  if (InitializeTargets) {
    InitializeTargets();
  }

  // Printing to stderr should flush stdout. This is most noticeable when stderr
  // is piped to stdout.
  llvm::errs().tie(&llvm::outs());
}

InitLLVM::InitializeTargetsFn* InitLLVM::InitializeTargets = nullptr;

}  // namespace Carbon
