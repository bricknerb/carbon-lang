// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

choice A {
  Value(i32)
}

impl i32 as ImplicitAs(A) {
  fn Convert[self: Self]() -> A { return A.Value(self + 1); }
}

fn Main() -> i32 {
  let A.Value(n: i32) = 2;
  return n;
}

// CHECK:STDOUT: result: 3
