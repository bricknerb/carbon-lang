// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

namespace N;

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_unknown_member.carbon:[[@LINE+1]]: name 'value' has not been declared in namespace N
  return N.value;
}
