// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

choice Ints {
  None,
  // CHECK:STDERR: COMPILATION ERROR: fail_choice_duplicate.carbon:[[@LINE+1]]: Duplicate name `None` in choice type
  None,
}

fn Main() -> i32 {
  return 0;
}
