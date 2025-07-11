// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_COMMON_EXE_PATH_H_
#define CARBON_COMMON_EXE_PATH_H_

#include "llvm/ADT/StringRef.h"

namespace Carbon {

// Computes the executable path for the given `argv[0]` value from `main`.
// `argv0` is required to be null-terminated.
//
// A simplistic approach -- if the provided string isn't already a valid path,
// we look it up in the PATH environment variable. Doesn't resolve any symlinks
// and if it fails, returns the main executable path.
auto FindExecutablePath(const char* argv0) -> std::string;

}  // namespace Carbon

#endif  // CARBON_COMMON_EXE_PATH_H_
