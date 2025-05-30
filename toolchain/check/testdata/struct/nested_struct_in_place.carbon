// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/struct/nested_struct_in_place.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/struct/nested_struct_in_place.carbon

fn F() -> (i32, i32, i32);

fn G() {
  var v: {.a: (i32, i32, i32), .b: (i32, i32, i32)} = {.a = F(), .b = F()};
}

// CHECK:STDOUT: --- nested_struct_in_place.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %tuple.type.ff9: type = tuple_type (type, type, type) [concrete]
// CHECK:STDOUT:   %tuple.type.189: type = tuple_type (%i32, %i32, %i32) [concrete]
// CHECK:STDOUT:   %pattern_type.b5a: type = pattern_type %tuple.type.189 [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %G.type: type = fn_type @G [concrete]
// CHECK:STDOUT:   %G: %G.type = struct_value () [concrete]
// CHECK:STDOUT:   %struct_type.a.b.2f9: type = struct_type {.a: %tuple.type.189, .b: %tuple.type.189} [concrete]
// CHECK:STDOUT:   %pattern_type.3c2: type = pattern_type %struct_type.a.b.2f9 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/types/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:     .G = %G.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {
// CHECK:STDOUT:     %return.patt: %pattern_type.b5a = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.b5a = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc14_12: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc14_12: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc14_17: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc14_17: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc14_22: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc14_22: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc14_25.1: %tuple.type.ff9 = tuple_literal (%i32.loc14_12, %i32.loc14_17, %i32.loc14_22)
// CHECK:STDOUT:     %.loc14_25.2: type = converted %.loc14_25.1, constants.%tuple.type.189 [concrete = constants.%tuple.type.189]
// CHECK:STDOUT:     %return.param: ref %tuple.type.189 = out_param call_param0
// CHECK:STDOUT:     %return: ref %tuple.type.189 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %G.decl: %G.type = fn_decl @G [concrete = constants.%G] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() -> %return.param: %tuple.type.189;
// CHECK:STDOUT:
// CHECK:STDOUT: fn @G() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: %pattern_type.3c2 = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: %pattern_type.3c2 = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref %struct_type.a.b.2f9 = var %v.var_patt
// CHECK:STDOUT:   %F.ref.loc17_61: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %.loc17_74.1: ref %tuple.type.189 = struct_access %v.var, element0
// CHECK:STDOUT:   %F.call.loc17_63: init %tuple.type.189 = call %F.ref.loc17_61() to %.loc17_74.1
// CHECK:STDOUT:   %F.ref.loc17_71: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %.loc17_74.2: ref %tuple.type.189 = struct_access %v.var, element1
// CHECK:STDOUT:   %F.call.loc17_73: init %tuple.type.189 = call %F.ref.loc17_71() to %.loc17_74.2
// CHECK:STDOUT:   %.loc17_74.3: %struct_type.a.b.2f9 = struct_literal (%F.call.loc17_63, %F.call.loc17_73)
// CHECK:STDOUT:   %.loc17_74.4: init %struct_type.a.b.2f9 = struct_init (%F.call.loc17_63, %F.call.loc17_73) to %v.var
// CHECK:STDOUT:   %.loc17_3: init %struct_type.a.b.2f9 = converted %.loc17_74.3, %.loc17_74.4
// CHECK:STDOUT:   assign %v.var, %.loc17_3
// CHECK:STDOUT:   %.loc17_51: type = splice_block %struct_type.a.b [concrete = constants.%struct_type.a.b.2f9] {
// CHECK:STDOUT:     %int_32.loc17_16: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc17_16: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc17_21: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc17_21: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc17_26: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc17_26: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc17_29.1: %tuple.type.ff9 = tuple_literal (%i32.loc17_16, %i32.loc17_21, %i32.loc17_26)
// CHECK:STDOUT:     %.loc17_29.2: type = converted %.loc17_29.1, constants.%tuple.type.189 [concrete = constants.%tuple.type.189]
// CHECK:STDOUT:     %int_32.loc17_37: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc17_37: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc17_42: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc17_42: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc17_47: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc17_47: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc17_50.1: %tuple.type.ff9 = tuple_literal (%i32.loc17_37, %i32.loc17_42, %i32.loc17_47)
// CHECK:STDOUT:     %.loc17_50.2: type = converted %.loc17_50.1, constants.%tuple.type.189 [concrete = constants.%tuple.type.189]
// CHECK:STDOUT:     %struct_type.a.b: type = struct_type {.a: %tuple.type.189, .b: %tuple.type.189} [concrete = constants.%struct_type.a.b.2f9]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: ref %struct_type.a.b.2f9 = bind_name v, %v.var
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
