// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/convert.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/builtins/int/make_type_signed.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/builtins/int/make_type_signed.carbon

// --- types.carbon

library "[[@TEST_NAME]]";

fn IntLiteral() -> type = "int_literal.make_type";
fn Int(n: IntLiteral()) -> type = "int.make_type_signed";

// --- use_types.carbon

library "[[@TEST_NAME]]";

import library "types";

fn F(n: Int(64)) ->
    //@dump-sem-ir-begin
    Int(64)
    //@dump-sem-ir-end
    {
  return n;
}

fn G(n: Int(13)) ->
    //@dump-sem-ir-begin
    Int(13)
    //@dump-sem-ir-end
    {
  return n;
}

fn Symbolic(N:! IntLiteral(), x: Int(N)) -> Int(N) {
  return x;
}

// --- import_types.carbon

library "[[@TEST_NAME]]";

import library "types";
import library "use_types";

fn UseF(n: Int(64)) -> Int(64) {
  return F(n);
}

fn UseG(n: Int(13)) -> Int(13) {
  return G(n);
}

fn UseSymbolic(n: Int(24)) -> Int(24) {
  return Symbolic(24, n);
}

// --- fail_zero_size.carbon

library "[[@TEST_NAME]]";

import library "types";

// CHECK:STDERR: fail_zero_size.carbon:[[@LINE+4]]:8: error: integer type width of 0 is not positive [IntWidthNotPositive]
// CHECK:STDERR: var n: Int(0);
// CHECK:STDERR:        ^~~~~~
// CHECK:STDERR:
var n: Int(0);

// --- fail_negative_size.carbon

library "[[@TEST_NAME]]";

import library "types";

fn Negate(a: IntLiteral()) -> IntLiteral() = "int.snegate";

// CHECK:STDERR: fail_negative_size.carbon:[[@LINE+4]]:8: error: integer type width of -1 is not positive [IntWidthNotPositive]
// CHECK:STDERR: var n: Int(Negate(1));
// CHECK:STDERR:        ^~~~~~~~~~~~~~
// CHECK:STDERR:
var n: Int(Negate(1));

// --- fail_oversized.carbon

library "[[@TEST_NAME]]";

import library "types";

// CHECK:STDERR: fail_oversized.carbon:[[@LINE+4]]:8: error: integer type width of 1000000000 is greater than the maximum supported width of 8388608 [IntWidthTooLarge]
// CHECK:STDERR: var m: Int(1000000000);
// CHECK:STDERR:        ^~~~~~~~~~~~~~~
// CHECK:STDERR:
var m: Int(1000000000);

// CHECK:STDOUT: --- use_types.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_64: Core.IntLiteral = int_value 64 [concrete]
// CHECK:STDOUT:   %i64.builtin: type = int_type signed, %int_64 [concrete]
// CHECK:STDOUT:   %int_13: Core.IntLiteral = int_value 13 [concrete]
// CHECK:STDOUT:   %i13.builtin: type = int_type signed, %int_13 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F(%n.param: %i64.builtin) -> %i64.builtin {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @G(%n.param: %i13.builtin) -> %i13.builtin {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
