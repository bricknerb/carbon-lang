// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;


choice MyOptionalElement(ZZ:! type, YY:! type) {
  None(YY),
  Element(ZZ)
}


fn Main() -> i32 {
  var f: MyOptionalElement(String,i32);
  f = MyOptionalElement(String,i32).None(22);
  match(f) {
    case MyOptionalElement(String,i32).None(var x: i32) => {
      return x;
    }
  }
  return 0;
}

// CHECK:STDOUT: result: 22
