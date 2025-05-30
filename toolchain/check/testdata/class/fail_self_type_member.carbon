// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/fail_self_type_member.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/fail_self_type_member.carbon

class Class {
  var b: bool;
}

fn F() -> bool {
  var c1: Class = {.b = true};
  // CHECK:STDERR: fail_self_type_member.carbon:[[@LINE+8]]:17: error: expected identifier after `.` [ExpectedIdentifierAfterPeriodOrArrow]
  // CHECK:STDERR:   var c2: Class.Self = c1;
  // CHECK:STDERR:                 ^~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_self_type_member.carbon:[[@LINE+4]]:17: error: semantics TODO: `handle invalid parse trees in `check`` [SemanticsTodo]
  // CHECK:STDERR:   var c2: Class.Self = c1;
  // CHECK:STDERR:                 ^~~~
  // CHECK:STDERR:
  var c2: Class.Self = c1;
  return c2.b;
}

// CHECK:STDOUT: --- fail_self_type_member.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Class: type = class_type @Class [concrete]
// CHECK:STDOUT:   %Bool.type: type = fn_type @Bool [concrete]
// CHECK:STDOUT:   %Bool: %Bool.type = struct_value () [concrete]
// CHECK:STDOUT:   %Class.elem: type = unbound_element_type %Class, bool [concrete]
// CHECK:STDOUT:   %struct_type.b: type = struct_type {.b: bool} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.b [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Class {
// CHECK:STDOUT:   %bool.make_type: init type = call constants.%Bool() [concrete = bool]
// CHECK:STDOUT:   %.loc15_10.1: type = value_of_initializer %bool.make_type [concrete = bool]
// CHECK:STDOUT:   %.loc15_10.2: type = converted %bool.make_type, %.loc15_10.1 [concrete = bool]
// CHECK:STDOUT:   %.loc15_8: %Class.elem = field_decl b, element0 [concrete]
// CHECK:STDOUT:   %struct_type.b: type = struct_type {.b: bool} [concrete = constants.%struct_type.b]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.b [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Class
// CHECK:STDOUT:   .b = %.loc15_8
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() -> bool;
// CHECK:STDOUT:
