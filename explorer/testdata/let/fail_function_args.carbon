// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn f((var x: i32, y: i32)) -> i32 {
  x = 0;
  // CHECK:STDERR: COMPILATION ERROR: fail_function_args.carbon:[[@LINE+1]]: Only a reference expression can be assigned to, but got `y`
  y = 0;
  return x - 1;
}

fn Main() -> i32 {
  var t: auto = (1, 2);
  return f(t);
}
