// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// CHECK:STDERR: COMPILATION ERROR: fail_alias_expression.carbon:[[@LINE+1]]: invalid target for alias declaration
alias A = 5;

fn Main() -> i32 { return 0; }
