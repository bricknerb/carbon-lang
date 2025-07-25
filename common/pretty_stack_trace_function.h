// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_COMMON_PRETTY_STACK_TRACE_FUNCTION_H_
#define CARBON_COMMON_PRETTY_STACK_TRACE_FUNCTION_H_

#include <functional>

#include "llvm/Support/PrettyStackTrace.h"

namespace Carbon {

// Calls `fn` as part of LLVM's pretty stack trace support. Implementations
// should typically have a terminating `\n`.
class PrettyStackTraceFunction : public llvm::PrettyStackTraceEntry {
 public:
  explicit PrettyStackTraceFunction(
      std::function<auto(llvm::raw_ostream&)->void> fn)
      : fn_(std::move(fn)) {}
  ~PrettyStackTraceFunction() override = default;

  auto print(llvm::raw_ostream& output) const -> void override { fn_(output); }

 private:
  const std::function<auto(llvm::raw_ostream&)->void> fn_;
};

}  // namespace Carbon

#endif  // CARBON_COMMON_PRETTY_STACK_TRACE_FUNCTION_H_
