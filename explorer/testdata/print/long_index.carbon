// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  Print("Printing {0000}", 1);
  return 0;
}

// CHECK:STDOUT: Printing 1
// CHECK:STDOUT: result: 0
