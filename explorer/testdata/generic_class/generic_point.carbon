// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class Point(T:! type) {
  fn Origin(zero: T) -> Point(T) {
    return {.x = zero, .y = zero};
  }

  fn GetX[self: Point(T)]() -> T {
    return self.x;
  }

  var x: T;
  var y: T;
}

fn Main() -> i32 {
  var p: Point(i32) = Point(i32).Origin(0);
  return p.GetX();
}

// CHECK:STDOUT: result: 0
