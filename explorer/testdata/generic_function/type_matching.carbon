// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn fst[T:! type](x: T, y: T) -> T {
  return x;
}

fn Main() -> i32 {
  return fst(0, 1);
}

// CHECK:STDOUT: result: 0
