// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEM_IR_CLANG_SOURCE_LOCATION_H_
#define CARBON_TOOLCHAIN_SEM_IR_CLANG_SOURCE_LOCATION_H_

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceLocation.h"

namespace Carbon::SemIR {

// Contains the Clang source location and a pointer to the diagnostics engine so
// it can be transformed to a path and line number.
struct ClangSourceLocation {
  clang::SourceLocation source_location;
};

}  // namespace Carbon::SemIR

#endif  // CARBON_TOOLCHAIN_SEM_IR_CLANG_SOURCE_LOCATION_H_
