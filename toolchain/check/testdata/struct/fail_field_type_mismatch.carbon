// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/int.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/struct/fail_field_type_mismatch.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/struct/fail_field_type_mismatch.carbon

// CHECK:STDERR: fail_field_type_mismatch.carbon:[[@LINE+4]]:20: error: missing value for field `a` in struct initialization [StructInitMissingFieldInLiteral]
// CHECK:STDERR: var x: {.a: i32} = {.b = 1.0};
// CHECK:STDERR:                    ^~~~~~~~~~
// CHECK:STDERR:
var x: {.a: i32} = {.b = 1.0};

// CHECK:STDOUT: --- fail_field_type_mismatch.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %struct_type.a: type = struct_type {.a: %i32} [concrete]
// CHECK:STDOUT:   %pattern_type.268: type = pattern_type %struct_type.a [concrete]
// CHECK:STDOUT:   %float: f64 = float_literal 1 [concrete]
// CHECK:STDOUT:   %struct_type.b: type = struct_type {.b: f64} [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/parts/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .x = %x
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.268 = binding_pattern x [concrete]
// CHECK:STDOUT:     %x.var_patt: %pattern_type.268 = var_pattern %x.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %x.var: ref %struct_type.a = var %x.var_patt [concrete]
// CHECK:STDOUT:   %.loc19: type = splice_block %struct_type.a [concrete = constants.%struct_type.a] {
// CHECK:STDOUT:     %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %struct_type.a: type = struct_type {.a: %i32} [concrete = constants.%struct_type.a]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %x: ref %struct_type.a = bind_name x, %x.var [concrete = %x.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %float: f64 = float_literal 1 [concrete = constants.%float]
// CHECK:STDOUT:   %.loc19: %struct_type.b = struct_literal (%float)
// CHECK:STDOUT:   assign file.%x.var, <error>
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
