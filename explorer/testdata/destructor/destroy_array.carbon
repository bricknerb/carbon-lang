// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;


class A{
    destructor[self: Self]{
        Print("DESTRUCTOR A {0}",self.n);
    }
    var n: i32;
}

fn Main() -> i32 {
  var a: [A; 2] = ({.n = 6},{.n = 5});
  var b: [[A; 2]; 2] = (({.n = 4},{.n = 3}), ({.n = 2},{.n = 1}));
  return 1;
}

// CHECK:STDOUT: DESTRUCTOR A 1
// CHECK:STDOUT: DESTRUCTOR A 2
// CHECK:STDOUT: DESTRUCTOR A 3
// CHECK:STDOUT: DESTRUCTOR A 4
// CHECK:STDOUT: DESTRUCTOR A 5
// CHECK:STDOUT: DESTRUCTOR A 6
// CHECK:STDOUT: result: 1
