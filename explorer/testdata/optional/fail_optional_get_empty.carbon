// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE
// CHECK:STDERR: RUNTIME ERROR: prelude.carbon:{{\d+}}: "Attempted to unwrap empty Optional"

package ExplorerTest api;

fn Main() -> i32{
  var x: Optional(i32) = Optional(i32).CreateEmpty();
  return x.Get();
}
