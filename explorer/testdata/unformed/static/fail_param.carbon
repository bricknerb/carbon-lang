// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn AddInt(a: i32, b: i32) -> auto {
  return a + b;
}

fn Main() -> i32 {
  var x: i32;
  // CHECK:STDERR: COMPILATION ERROR: fail_param.carbon:[[@LINE+1]]: use of uninitialized variable x
  return AddInt(x, 2);
}
