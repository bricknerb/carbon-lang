// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package EmptyIdentifier impl;

fn test() -> i32 {
  return 1;
}

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_var_type_is_call.carbon:[[@LINE+1]]: type error in type of name binding: 'i32' is not implicitly convertible to 'type'
  var x: test() = 1;
  return 0;
}
