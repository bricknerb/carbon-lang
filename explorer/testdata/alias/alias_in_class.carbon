// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class A {
  class B {}
  alias C = B;
}

fn Main() -> i32 {
  return 0;
}

// CHECK:STDOUT: result: 0
