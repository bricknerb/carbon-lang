// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn snd[T:! type](x: i32, y: T) -> T {
  return y;
}

fn Main() -> i32 {
  return snd(0, 1);
}

// CHECK:STDOUT: result: 1
