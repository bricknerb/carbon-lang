// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var x: i32 = 0;
  // CHECK:STDERR: COMPILATION ERROR: fail_block_duplicate.carbon:[[@LINE+1]]: Duplicate name `x` also found at fail_block_duplicate.carbon:[[@LINE-1]]
  var x: i32 = 0;
  return 0;
}
