// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface I {
  fn F() -> i32;
  fn M[self: Self]() -> i32;
}
class A {
  var n: i32;
  extend impl as I {
    fn F() -> i32 { return 1; }
    fn M[self: Self]() -> i32 { return 2; }
  }
  fn G[self: Self]() -> i32 { return 3; }
}
impl i32 as I {
  fn F() -> i32 { return 4; }
  fn M[self: Self]() -> i32 { return 5; }
}

alias IF = I.F;
alias IM = I.M;
alias AIF = A.(IF);
alias AIM = A.(IM);
alias AG = A.G;
alias i32IF = i32.(IF);
alias i32IM = i32.(IM);

fn Main() -> i32 {
  var a: A = {.n = 0};
  if (A.(IF)() != 1) {
    return 1;
  }
  if (a.(IF)() != 1) {
    return 2;
  }
  if (a.(IM)() != 2) {
    return 3;
  }
  if (a.(A.(IM))() != 2) {
    return 4;
  }
  if (AIF() != 1) {
    return 5;
  }
  if (a.(AIM)() != 2) {
    return 6;
  }
  if (a.(AG)() != 3) {
    return 7;
  }
  if (i32.(IF)() != 4) {
    return 8;
  }
  if (0.(IF)() != 4) {
    return 9;
  }
  if (0.(IM)() != 5) {
    return 10;
  }
  if (0.(i32.(IM))() != 5) {
    return 11;
  }
  if (i32IF() != 4) {
    return 12;
  }
  if (0.(i32IM)() != 5) {
    return 13;
  }
  return 0;
}

// CHECK:STDOUT: result: 0
