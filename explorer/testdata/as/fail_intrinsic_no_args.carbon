// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_intrinsic_no_args.carbon:[[@LINE+1]]: __intrinsic_implicit_as takes 1 argument
  __intrinsic_implicit_as();
  return 0;
}
