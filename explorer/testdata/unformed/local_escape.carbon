// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn AssignIntTo(x: i32, destination: i32*) {
  *destination = x;
}

fn Main() -> i32 {
  var y: i32;
  // `y` is unformed here.
  AssignIntTo(42, &y);
  // `y` is fully formed and usable.
  return y;
}

// CHECK:STDOUT: result: 42
