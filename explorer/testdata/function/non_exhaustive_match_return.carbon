// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var n: i32 = 0;
  match (n) {
    case 1 => { return 0; }
  }
  return 1;
}

// CHECK:STDOUT: result: 1
