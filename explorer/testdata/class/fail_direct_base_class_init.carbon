// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

base class A {
  var a: i32;
}

class B {
  extend base: A;
  var b: i32;
}

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_direct_base_class_init.carbon:[[@LINE+1]]: type error in initializer of variable: '{.a: i32, .b: i32}' is not implicitly convertible to 'class B'
  var b: B = {.a=0, .b=1};
  return 0;
}
