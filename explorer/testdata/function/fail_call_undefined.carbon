// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn F() -> i32;

fn Main() -> i32 {
  // CHECK:STDERR: RUNTIME ERROR: fail_call_undefined.carbon:[[@LINE+1]]: attempt to call function `F` that has not been defined
  return F();
}
