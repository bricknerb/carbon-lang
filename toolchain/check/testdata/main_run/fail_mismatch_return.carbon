// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/primitives.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/main_run/fail_mismatch_return.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/main_run/fail_mismatch_return.carbon

// CHECK:STDERR: fail_mismatch_return.carbon:[[@LINE+4]]:1: error: invalid signature for `Main.Run` function; expected `fn ()` or `fn () -> i32` [InvalidMainRunSignature]
// CHECK:STDERR: fn Run() -> f64 {
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~
// CHECK:STDERR:
fn Run() -> f64 {
  return 0.0;
}
