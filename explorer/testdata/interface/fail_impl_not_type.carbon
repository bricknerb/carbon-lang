// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface Vector {
  fn Zero() -> i32;
}
// CHECK:STDERR: COMPILATION ERROR: fail_impl_not_type.carbon:[[@LINE+1]]: type error in type expression: 'String' is not implicitly convertible to 'type'
impl "hello" as Vector {
  fn Zero() -> i32 { return 0; }
}

fn Main() -> i32 {
  return 0;
}
