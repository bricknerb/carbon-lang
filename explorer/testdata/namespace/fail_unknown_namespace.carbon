// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// CHECK:STDERR: COMPILATION ERROR: fail_unknown_namespace.carbon:[[@LINE+1]]: name 'N' has not been declared in this scope
fn N.F() {}

fn Main() -> i32 {
  return 0;
}
