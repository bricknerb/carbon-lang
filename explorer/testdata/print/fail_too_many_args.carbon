// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_too_many_args.carbon:[[@LINE+1]]: Print takes 1 or 2 arguments, received 5
  Print("too", "many", "args", "to", "print");
  return 0;
}
