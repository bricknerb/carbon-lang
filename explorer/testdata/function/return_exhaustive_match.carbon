// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  match (0) {
    case _: auto => { return 1; }
  }
  // We don't need a `return` here because the match is exhaustive.
}

// CHECK:STDOUT: result: 1
