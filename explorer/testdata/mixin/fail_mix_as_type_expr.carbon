// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

__mixin Operations {}

fn Main() -> i32 {
   // CHECK:STDERR: COMPILATION ERROR: fail_mix_as_type_expr.carbon:[[@LINE+1]]: invalid use of mixin Operations
   var a: Operations;
   return 0;
}
