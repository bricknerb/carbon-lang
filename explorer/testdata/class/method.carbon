// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class Point {
  fn Origin() -> Point {
    return {.x = 0, .y = 0};
  }

  fn GetX[self: Point]() -> i32 {
    return self.x;
  }

  var x: i32;
  var y: i32;
}

fn Main() -> i32 {
  var p: Point = Point.Origin();
  return p.GetX();
}

// CHECK:STDOUT: result: 0
