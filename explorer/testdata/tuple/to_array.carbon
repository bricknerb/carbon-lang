// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var t: auto = (1, 2);
  var a: [i32; 2] = t;
  return a[0] + a[1];
}

// CHECK:STDOUT: result: 3
