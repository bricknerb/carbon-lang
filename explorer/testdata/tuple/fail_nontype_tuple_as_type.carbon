// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// CHECK:STDERR: COMPILATION ERROR: fail_nontype_tuple_as_type.carbon:[[@LINE+1]]: type error in type of name binding: '(i32, i32)' is not implicitly convertible to 'type'
fn F[T:! (i32, i32)](x: T);

fn Main() -> i32;
