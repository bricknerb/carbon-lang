// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var p: i32* = heap.New(0);
  var y: i32 = *p;
  heap.Delete(p);
  return y;
}

// CHECK:STDOUT: result: 0
