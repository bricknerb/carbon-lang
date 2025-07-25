// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/none.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/function/builtin/import.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/function/builtin/import.carbon

// --- core.carbon

package Core library "[[@TEST_NAME]]";


fn IntLiteral() -> type = "int_literal.make_type";
fn Int(N: IntLiteral()) -> type = "int.make_type_signed";

fn AsI32(a: IntLiteral()) -> i32 = "int.convert_checked";
fn AsIntLiteral(a: i32) -> IntLiteral() = "int.convert_checked";

fn TestAdd(a: i32, b: i32) -> i32 = "int.sadd";

// --- use.carbon

import Core library "core";

var arr: array(i32, Core.AsIntLiteral(Core.TestAdd(Core.AsI32(1), Core.AsI32(2)))) = (Core.AsI32(1), Core.AsI32(2), Core.AsI32(3));

// CHECK:STDOUT: --- core.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %IntLiteral.type: type = fn_type @IntLiteral [concrete]
// CHECK:STDOUT:   %IntLiteral: %IntLiteral.type = struct_value () [concrete]
// CHECK:STDOUT:   %pattern_type.dc0: type = pattern_type Core.IntLiteral [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32.builtin: type = int_type signed, %int_32 [concrete]
// CHECK:STDOUT:   %pattern_type.956: type = pattern_type %i32.builtin [concrete]
// CHECK:STDOUT:   %AsI32.type: type = fn_type @AsI32 [concrete]
// CHECK:STDOUT:   %AsI32: %AsI32.type = struct_value () [concrete]
// CHECK:STDOUT:   %AsIntLiteral.type: type = fn_type @AsIntLiteral [concrete]
// CHECK:STDOUT:   %AsIntLiteral: %AsIntLiteral.type = struct_value () [concrete]
// CHECK:STDOUT:   %TestAdd.type: type = fn_type @TestAdd [concrete]
// CHECK:STDOUT:   %TestAdd: %TestAdd.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .IntLiteral = %IntLiteral.decl
// CHECK:STDOUT:     .Int = %Int.decl
// CHECK:STDOUT:     .AsI32 = %AsI32.decl
// CHECK:STDOUT:     .AsIntLiteral = %AsIntLiteral.decl
// CHECK:STDOUT:     .TestAdd = %TestAdd.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %IntLiteral.decl: %IntLiteral.type = fn_decl @IntLiteral [concrete = constants.%IntLiteral] {
// CHECK:STDOUT:     %return.patt: %pattern_type.98f = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.98f = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %return.param: ref type = out_param call_param0
// CHECK:STDOUT:     %return: ref type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Int.decl: %Int.type = fn_decl @Int [concrete = constants.%Int] {
// CHECK:STDOUT:     %N.patt: %pattern_type.dc0 = binding_pattern N [concrete]
// CHECK:STDOUT:     %N.param_patt: %pattern_type.dc0 = value_param_pattern %N.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.98f = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.98f = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %N.param: Core.IntLiteral = value_param call_param0
// CHECK:STDOUT:     %.loc6_22.1: type = splice_block %.loc6_22.3 [concrete = Core.IntLiteral] {
// CHECK:STDOUT:       %IntLiteral.ref: %IntLiteral.type = name_ref IntLiteral, file.%IntLiteral.decl [concrete = constants.%IntLiteral]
// CHECK:STDOUT:       %IntLiteral.call: init type = call %IntLiteral.ref() [concrete = Core.IntLiteral]
// CHECK:STDOUT:       %.loc6_22.2: type = value_of_initializer %IntLiteral.call [concrete = Core.IntLiteral]
// CHECK:STDOUT:       %.loc6_22.3: type = converted %IntLiteral.call, %.loc6_22.2 [concrete = Core.IntLiteral]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %N: Core.IntLiteral = bind_name N, %N.param
// CHECK:STDOUT:     %return.param: ref type = out_param call_param1
// CHECK:STDOUT:     %return: ref type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %AsI32.decl: %AsI32.type = fn_decl @AsI32 [concrete = constants.%AsI32] {
// CHECK:STDOUT:     %a.patt: %pattern_type.dc0 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.dc0 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.956 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.956 = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %Int.call: init type = call constants.%Int(%int_32) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %.loc8_30.1: type = value_of_initializer %Int.call [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %.loc8_30.2: type = converted %Int.call, %.loc8_30.1 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %a.param: Core.IntLiteral = value_param call_param0
// CHECK:STDOUT:     %.loc8_24.1: type = splice_block %.loc8_24.3 [concrete = Core.IntLiteral] {
// CHECK:STDOUT:       %IntLiteral.ref: %IntLiteral.type = name_ref IntLiteral, file.%IntLiteral.decl [concrete = constants.%IntLiteral]
// CHECK:STDOUT:       %IntLiteral.call: init type = call %IntLiteral.ref() [concrete = Core.IntLiteral]
// CHECK:STDOUT:       %.loc8_24.2: type = value_of_initializer %IntLiteral.call [concrete = Core.IntLiteral]
// CHECK:STDOUT:       %.loc8_24.3: type = converted %IntLiteral.call, %.loc8_24.2 [concrete = Core.IntLiteral]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %a: Core.IntLiteral = bind_name a, %a.param
// CHECK:STDOUT:     %return.param: ref %i32.builtin = out_param call_param1
// CHECK:STDOUT:     %return: ref %i32.builtin = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %AsIntLiteral.decl: %AsIntLiteral.type = fn_decl @AsIntLiteral [concrete = constants.%AsIntLiteral] {
// CHECK:STDOUT:     %a.patt: %pattern_type.956 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.956 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.dc0 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.dc0 = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %IntLiteral.ref: %IntLiteral.type = name_ref IntLiteral, file.%IntLiteral.decl [concrete = constants.%IntLiteral]
// CHECK:STDOUT:     %IntLiteral.call: init type = call %IntLiteral.ref() [concrete = Core.IntLiteral]
// CHECK:STDOUT:     %.loc9_39.1: type = value_of_initializer %IntLiteral.call [concrete = Core.IntLiteral]
// CHECK:STDOUT:     %.loc9_39.2: type = converted %IntLiteral.call, %.loc9_39.1 [concrete = Core.IntLiteral]
// CHECK:STDOUT:     %a.param: %i32.builtin = value_param call_param0
// CHECK:STDOUT:     %.loc9_20.1: type = splice_block %.loc9_20.3 [concrete = constants.%i32.builtin] {
// CHECK:STDOUT:       %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %Int.call: init type = call constants.%Int(%int_32) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:       %.loc9_20.2: type = value_of_initializer %Int.call [concrete = constants.%i32.builtin]
// CHECK:STDOUT:       %.loc9_20.3: type = converted %Int.call, %.loc9_20.2 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %a: %i32.builtin = bind_name a, %a.param
// CHECK:STDOUT:     %return.param: ref Core.IntLiteral = out_param call_param1
// CHECK:STDOUT:     %return: ref Core.IntLiteral = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %TestAdd.decl: %TestAdd.type = fn_decl @TestAdd [concrete = constants.%TestAdd] {
// CHECK:STDOUT:     %a.patt: %pattern_type.956 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.956 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %b.patt: %pattern_type.956 = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.param_patt: %pattern_type.956 = value_param_pattern %b.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.956 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.956 = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc11_31: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %Int.call.loc11_31: init type = call constants.%Int(%int_32.loc11_31) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %.loc11_31.1: type = value_of_initializer %Int.call.loc11_31 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %.loc11_31.2: type = converted %Int.call.loc11_31, %.loc11_31.1 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %a.param: %i32.builtin = value_param call_param0
// CHECK:STDOUT:     %.loc11_15.1: type = splice_block %.loc11_15.3 [concrete = constants.%i32.builtin] {
// CHECK:STDOUT:       %int_32.loc11_15: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %Int.call.loc11_15: init type = call constants.%Int(%int_32.loc11_15) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:       %.loc11_15.2: type = value_of_initializer %Int.call.loc11_15 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:       %.loc11_15.3: type = converted %Int.call.loc11_15, %.loc11_15.2 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %a: %i32.builtin = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %i32.builtin = value_param call_param1
// CHECK:STDOUT:     %.loc11_23.1: type = splice_block %.loc11_23.3 [concrete = constants.%i32.builtin] {
// CHECK:STDOUT:       %int_32.loc11_23: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %Int.call.loc11_23: init type = call constants.%Int(%int_32.loc11_23) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:       %.loc11_23.2: type = value_of_initializer %Int.call.loc11_23 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:       %.loc11_23.3: type = converted %Int.call.loc11_23, %.loc11_23.2 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %b: %i32.builtin = bind_name b, %b.param
// CHECK:STDOUT:     %return.param: ref %i32.builtin = out_param call_param2
// CHECK:STDOUT:     %return: ref %i32.builtin = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @IntLiteral() -> type = "int_literal.make_type";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Int(%N.param: Core.IntLiteral) -> type = "int.make_type_signed";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @AsI32(%a.param: Core.IntLiteral) -> %i32.builtin = "int.convert_checked";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @AsIntLiteral(%a.param: %i32.builtin) -> Core.IntLiteral = "int.convert_checked";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestAdd(%a.param: %i32.builtin, %b.param: %i32.builtin) -> %i32.builtin = "int.sadd";
// CHECK:STDOUT:
// CHECK:STDOUT: --- use.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32.builtin: type = int_type signed, %int_32 [concrete]
// CHECK:STDOUT:   %AsIntLiteral.type: type = fn_type @AsIntLiteral [concrete]
// CHECK:STDOUT:   %AsIntLiteral: %AsIntLiteral.type = struct_value () [concrete]
// CHECK:STDOUT:   %TestAdd.type: type = fn_type @TestAdd [concrete]
// CHECK:STDOUT:   %TestAdd: %TestAdd.type = struct_value () [concrete]
// CHECK:STDOUT:   %AsI32.type: type = fn_type @AsI32 [concrete]
// CHECK:STDOUT:   %AsI32: %AsI32.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %int_1.f38: %i32.builtin = int_value 1 [concrete]
// CHECK:STDOUT:   %int_2.ecc: Core.IntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %int_2.5a1: %i32.builtin = int_value 2 [concrete]
// CHECK:STDOUT:   %int_3.a0f: %i32.builtin = int_value 3 [concrete]
// CHECK:STDOUT:   %int_3.1ba: Core.IntLiteral = int_value 3 [concrete]
// CHECK:STDOUT:   %array_type: type = array_type %int_3.1ba, %i32.builtin [concrete]
// CHECK:STDOUT:   %pattern_type.9e2: type = pattern_type %array_type [concrete]
// CHECK:STDOUT:   %tuple.type: type = tuple_type (%i32.builtin, %i32.builtin, %i32.builtin) [concrete]
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete]
// CHECK:STDOUT:   %array: %array_type = tuple_value (%int_1.f38, %int_2.5a1, %int_3.a0f) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     .AsIntLiteral = %Core.AsIntLiteral
// CHECK:STDOUT:     .TestAdd = %Core.TestAdd
// CHECK:STDOUT:     .AsI32 = %Core.AsI32
// CHECK:STDOUT:     import Core//core
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//core, Int, loaded [concrete = constants.%Int]
// CHECK:STDOUT:   %Core.AsIntLiteral: %AsIntLiteral.type = import_ref Core//core, AsIntLiteral, loaded [concrete = constants.%AsIntLiteral]
// CHECK:STDOUT:   %Core.TestAdd: %TestAdd.type = import_ref Core//core, TestAdd, loaded [concrete = constants.%TestAdd]
// CHECK:STDOUT:   %Core.AsI32: %AsI32.type = import_ref Core//core, AsI32, loaded [concrete = constants.%AsI32]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .arr = %arr
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %arr.patt: %pattern_type.9e2 = binding_pattern arr [concrete]
// CHECK:STDOUT:     %arr.var_patt: %pattern_type.9e2 = var_pattern %arr.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %arr.var: ref %array_type = var %arr.var_patt [concrete]
// CHECK:STDOUT:   %.loc4_82: type = splice_block %array_type [concrete = constants.%array_type] {
// CHECK:STDOUT:     %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %Int.call: init type = call constants.%Int(%int_32) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %Core.ref.loc4_21: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %AsIntLiteral.ref: %AsIntLiteral.type = name_ref AsIntLiteral, imports.%Core.AsIntLiteral [concrete = constants.%AsIntLiteral]
// CHECK:STDOUT:     %Core.ref.loc4_39: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %TestAdd.ref: %TestAdd.type = name_ref TestAdd, imports.%Core.TestAdd [concrete = constants.%TestAdd]
// CHECK:STDOUT:     %Core.ref.loc4_52: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %AsI32.ref.loc4_56: %AsI32.type = name_ref AsI32, imports.%Core.AsI32 [concrete = constants.%AsI32]
// CHECK:STDOUT:     %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:     %AsI32.call.loc4_64: init %i32.builtin = call %AsI32.ref.loc4_56(%int_1) [concrete = constants.%int_1.f38]
// CHECK:STDOUT:     %Core.ref.loc4_67: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %AsI32.ref.loc4_71: %AsI32.type = name_ref AsI32, imports.%Core.AsI32 [concrete = constants.%AsI32]
// CHECK:STDOUT:     %int_2: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:     %AsI32.call.loc4_79: init %i32.builtin = call %AsI32.ref.loc4_71(%int_2) [concrete = constants.%int_2.5a1]
// CHECK:STDOUT:     %.loc4_64.1: %i32.builtin = value_of_initializer %AsI32.call.loc4_64 [concrete = constants.%int_1.f38]
// CHECK:STDOUT:     %.loc4_64.2: %i32.builtin = converted %AsI32.call.loc4_64, %.loc4_64.1 [concrete = constants.%int_1.f38]
// CHECK:STDOUT:     %.loc4_79.1: %i32.builtin = value_of_initializer %AsI32.call.loc4_79 [concrete = constants.%int_2.5a1]
// CHECK:STDOUT:     %.loc4_79.2: %i32.builtin = converted %AsI32.call.loc4_79, %.loc4_79.1 [concrete = constants.%int_2.5a1]
// CHECK:STDOUT:     %TestAdd.call: init %i32.builtin = call %TestAdd.ref(%.loc4_64.2, %.loc4_79.2) [concrete = constants.%int_3.a0f]
// CHECK:STDOUT:     %.loc4_80.1: %i32.builtin = value_of_initializer %TestAdd.call [concrete = constants.%int_3.a0f]
// CHECK:STDOUT:     %.loc4_80.2: %i32.builtin = converted %TestAdd.call, %.loc4_80.1 [concrete = constants.%int_3.a0f]
// CHECK:STDOUT:     %AsIntLiteral.call: init Core.IntLiteral = call %AsIntLiteral.ref(%.loc4_80.2) [concrete = constants.%int_3.1ba]
// CHECK:STDOUT:     %.loc4_16.1: type = value_of_initializer %Int.call [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %.loc4_16.2: type = converted %Int.call, %.loc4_16.1 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:     %.loc4_81.1: Core.IntLiteral = value_of_initializer %AsIntLiteral.call [concrete = constants.%int_3.1ba]
// CHECK:STDOUT:     %.loc4_81.2: Core.IntLiteral = converted %AsIntLiteral.call, %.loc4_81.1 [concrete = constants.%int_3.1ba]
// CHECK:STDOUT:     %array_type: type = array_type %.loc4_81.2, %.loc4_16.2 [concrete = constants.%array_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %arr: ref %array_type = bind_name arr, %arr.var [concrete = %arr.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Int = "int.make_type_signed" [from "core.carbon"];
// CHECK:STDOUT:
// CHECK:STDOUT: fn @AsIntLiteral = "int.convert_checked" [from "core.carbon"];
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestAdd = "int.sadd" [from "core.carbon"];
// CHECK:STDOUT:
// CHECK:STDOUT: fn @AsI32 = "int.convert_checked" [from "core.carbon"];
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Core.ref.loc4_87: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:   %AsI32.ref.loc4_91: %AsI32.type = name_ref AsI32, imports.%Core.AsI32 [concrete = constants.%AsI32]
// CHECK:STDOUT:   %int_1.loc4_98: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %AsI32.call.loc4_99: init %i32.builtin = call %AsI32.ref.loc4_91(%int_1.loc4_98) [concrete = constants.%int_1.f38]
// CHECK:STDOUT:   %Core.ref.loc4_102: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:   %AsI32.ref.loc4_106: %AsI32.type = name_ref AsI32, imports.%Core.AsI32 [concrete = constants.%AsI32]
// CHECK:STDOUT:   %int_2.loc4_113: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %AsI32.call.loc4_114: init %i32.builtin = call %AsI32.ref.loc4_106(%int_2.loc4_113) [concrete = constants.%int_2.5a1]
// CHECK:STDOUT:   %Core.ref.loc4_117: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:   %AsI32.ref.loc4_121: %AsI32.type = name_ref AsI32, imports.%Core.AsI32 [concrete = constants.%AsI32]
// CHECK:STDOUT:   %int_3: Core.IntLiteral = int_value 3 [concrete = constants.%int_3.1ba]
// CHECK:STDOUT:   %AsI32.call.loc4_129: init %i32.builtin = call %AsI32.ref.loc4_121(%int_3) [concrete = constants.%int_3.a0f]
// CHECK:STDOUT:   %.loc4_130.1: %tuple.type = tuple_literal (%AsI32.call.loc4_99, %AsI32.call.loc4_114, %AsI32.call.loc4_129)
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete = constants.%int_0]
// CHECK:STDOUT:   %.loc4_130.2: ref %i32.builtin = array_index file.%arr.var, %int_0
// CHECK:STDOUT:   %.loc4_130.3: init %i32.builtin = initialize_from %AsI32.call.loc4_99 to %.loc4_130.2 [concrete = constants.%int_1.f38]
// CHECK:STDOUT:   %int_1.loc4_130: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %.loc4_130.4: ref %i32.builtin = array_index file.%arr.var, %int_1.loc4_130
// CHECK:STDOUT:   %.loc4_130.5: init %i32.builtin = initialize_from %AsI32.call.loc4_114 to %.loc4_130.4 [concrete = constants.%int_2.5a1]
// CHECK:STDOUT:   %int_2.loc4_130: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %.loc4_130.6: ref %i32.builtin = array_index file.%arr.var, %int_2.loc4_130
// CHECK:STDOUT:   %.loc4_130.7: init %i32.builtin = initialize_from %AsI32.call.loc4_129 to %.loc4_130.6 [concrete = constants.%int_3.a0f]
// CHECK:STDOUT:   %.loc4_130.8: init %array_type = array_init (%.loc4_130.3, %.loc4_130.5, %.loc4_130.7) to file.%arr.var [concrete = constants.%array]
// CHECK:STDOUT:   %.loc4_1: init %array_type = converted %.loc4_130.1, %.loc4_130.8 [concrete = constants.%array]
// CHECK:STDOUT:   assign file.%arr.var, %.loc4_1
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
