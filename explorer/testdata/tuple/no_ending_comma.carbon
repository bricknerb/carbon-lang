// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var x: i32 = (1);
  var t2: (i32, i32) = (5, 2);
  t2[0] = 3;
  return t2[0] - t2[1] - x;
}

// CHECK:STDOUT: result: 0
