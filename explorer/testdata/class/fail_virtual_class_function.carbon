// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class C {
  virtual fn Foo() -> i32 {
    return 1;
  // CHECK:STDERR: COMPILATION ERROR: fail_virtual_class_function.carbon:[[@LINE+1]]: Error declaring `Foo`: class functions cannot be virtual.
  }
}

fn Main() -> i32 {
  let c: C = {};
  return 0;
}
