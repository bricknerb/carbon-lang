// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface HasType {
  let T:! type;
}

// CHECK:STDERR: COMPILATION ERROR: fail_overspecified_impl.carbon:[[@LINE+3]]: multiple different rewrites for `(i32).(HasType.T)`:
// CHECK:STDERR:   i32
// CHECK:STDERR:   {.a: i32}
impl i32 as HasType where .T = i32 and .T = {.a: i32} {}

fn Main() -> i32 { return 0; }
