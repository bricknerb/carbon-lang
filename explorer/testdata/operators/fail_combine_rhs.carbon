// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class C {}

// CHECK:STDERR: COMPILATION ERROR: fail_combine_rhs.carbon:[[@LINE+1]]: expected a constraint in second operand of `&`, found class C
fn F[T:! type & C]();

fn Main() -> i32 {
  return 0;
}
