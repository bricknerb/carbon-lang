// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

__mixin Operations {
  fn F[self: Self](x: Self) -> Self{
     return x;
  }
}

class Point {
  fn Origin() -> Point {
    return {.x = 0, .y = 0};
  }

  var x: i32;
  var y: i32;
  __mix Operations;
}

class Complex {
  fn Zero() -> Complex {
    return {.r = 0, .i = 0};
  }

  var r: i32;
  var i: i32;
}

fn Main() -> i32 {
  var p: Point = Point.Origin();
  var c: Complex = {.r = 42, .i = 1};
  // CHECK:STDERR: COMPILATION ERROR: fail_self_substitution.carbon:[[@LINE+1]]: mismatch in non-deduced types, `class Complex` is not implicitly convertible to `class Point`
  var p1: Point = p.F(c);

  return p1.x - 42;
}
