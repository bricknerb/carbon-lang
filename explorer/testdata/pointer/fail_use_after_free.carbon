// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var p: i32* = heap.New(5);
  heap.Delete(p);
  // CHECK:STDERR: RUNTIME ERROR: fail_use_after_free.carbon:[[@LINE+1]]: undefined behavior: access to dead or discarded value 5
  return *p;
}
