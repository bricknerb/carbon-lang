// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

var call_count: i32 = 0;
fn ReturnFalse() -> bool {
  call_count = call_count + 1;
  return false;
}

fn Main() -> i32 {
  var result: bool = ReturnFalse() or ReturnFalse();
  return if not result then call_count else -1;
}

// CHECK:STDOUT: result: 2
