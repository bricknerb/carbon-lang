// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

abstract class B {
}

// CHECK:STDERR: COMPILATION ERROR: fail_instantiate_global_abstract.carbon:[[@LINE+1]]: Cannot instantiate abstract class B
var b: B = {};

fn Main() -> i32 {
    return 0;
}
