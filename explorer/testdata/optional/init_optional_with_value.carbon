// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32{
  var x: Optional(String) = Optional(String).Create( "Hallo Welt" );
  if(x.HasValue()){
    Print(x.Get());
    return 0;
  }
  return -1;
}

// CHECK:STDOUT: Hallo Welt
// CHECK:STDOUT: result: 0
