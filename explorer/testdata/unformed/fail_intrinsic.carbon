// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var v: i32;
  // CHECK:STDERR: COMPILATION ERROR: fail_intrinsic.carbon:[[@LINE+1]]: use of uninitialized variable v
  return if __intrinsic_int_eq(v, v) then 1 else 2;
}
