// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// Test global variable initialization and read.

var zero: i32 = 0;

fn Main() -> i32 {
  return zero;
}

// CHECK:STDOUT: result: 0
