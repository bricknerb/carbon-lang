// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

choice Opt {
  None(),
  Some(i32)
}

fn A(a: Opt, b: Opt) -> i32 {
  match ((a, b)) {
    case (Opt.Some(n: i32), _: auto) => { return n; }
    case (_: auto, Opt.Some(n: i32)) => { return n; }
    case (Opt.None(), Opt.None()) => { return 0; }
  }
}

fn B(a: Opt, b: Opt) -> i32 {
  match ((a, b)) {
    case (Opt.None(), _: auto) => { return 0; }
    case (Opt.Some(n: i32), Opt.None()) => { return n; }
    case (Opt.Some(n: i32), Opt.Some(m: i32)) => { return n + m; }
  }
}

fn Main() -> i32 {
  return A(Opt.None(), Opt.Some(1)) + B(Opt.Some(2), Opt.None());
}

// CHECK:STDOUT: result: 3
