// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var t: auto = 5;
  match (t) {
    case 3 => {
      return -1;
    }
    case 4 => {
      return -1;
    }
    default => {
      return 0;
    }
  }
}

// CHECK:STDOUT: result: 0
