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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/function/builtin/adapted_type.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/function/builtin/adapted_type.carbon

// --- adapt.carbon

library "[[@TEST_NAME]]";

fn IntLiteral() -> type = "int_literal.make_type";

class MyIntLiteral {
  adapt IntLiteral();
}

fn Int(N: MyIntLiteral) -> type = "int.make_type_signed";

class MyInt32 {
  adapt Int(32 as MyIntLiteral);

  fn Make(a: MyIntLiteral) -> MyInt32;
}

fn MyInt32.Make(a: MyIntLiteral) -> MyInt32 = "int.convert_checked";

fn MyAdd(a: MyInt32, b: MyInt32) -> MyInt32 = "int.sadd";

var v: MyInt32 = MyAdd(MyInt32.Make(1 as MyIntLiteral), MyInt32.Make(2 as MyIntLiteral));

// --- fail_bad_adapt.carbon

library "[[@TEST_NAME]]";

class MyIntLiteral {
  adapt {};
}

// CHECK:STDERR: fail_bad_adapt.carbon:[[@LINE+4]]:1: error: invalid signature for builtin function "int.make_type_signed" [InvalidBuiltinSignature]
// CHECK:STDERR: fn Int(N: MyIntLiteral) -> type = "int.make_type_signed";
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR:
fn Int(N: MyIntLiteral) -> type = "int.make_type_signed";

// CHECK:STDOUT: --- adapt.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %IntLiteral.type: type = fn_type @IntLiteral [concrete]
// CHECK:STDOUT:   %IntLiteral: %IntLiteral.type = struct_value () [concrete]
// CHECK:STDOUT:   %MyIntLiteral: type = class_type @MyIntLiteral [concrete]
// CHECK:STDOUT:   %complete_type.972: <witness> = complete_type_witness Core.IntLiteral [concrete]
// CHECK:STDOUT:   %pattern_type.e22: type = pattern_type %MyIntLiteral [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %MyInt32: type = class_type @MyInt32 [concrete]
// CHECK:STDOUT:   %int_32.be0: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %int_32.4da: %MyIntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32.builtin: type = int_type signed, %int_32.4da [concrete]
// CHECK:STDOUT:   %pattern_type.81e: type = pattern_type %MyInt32 [concrete]
// CHECK:STDOUT:   %MyInt32.Make.type: type = fn_type @MyInt32.Make [concrete]
// CHECK:STDOUT:   %MyInt32.Make: %MyInt32.Make.type = struct_value () [concrete]
// CHECK:STDOUT:   %complete_type.ec2: <witness> = complete_type_witness %i32.builtin [concrete]
// CHECK:STDOUT:   %MyAdd.type: type = fn_type @MyAdd [concrete]
// CHECK:STDOUT:   %MyAdd: %MyAdd.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %int_1.383: %MyIntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %int_1.d74: %MyInt32 = int_value 1 [concrete]
// CHECK:STDOUT:   %int_2.ecc: Core.IntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %int_2.a27: %MyIntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %int_2.eda: %MyInt32 = int_value 2 [concrete]
// CHECK:STDOUT:   %int_3: %MyInt32 = int_value 3 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .IntLiteral = %IntLiteral.decl
// CHECK:STDOUT:     .MyIntLiteral = %MyIntLiteral.decl
// CHECK:STDOUT:     .Int = %Int.decl
// CHECK:STDOUT:     .MyInt32 = %MyInt32.decl
// CHECK:STDOUT:     .MyAdd = %MyAdd.decl
// CHECK:STDOUT:     .v = %v
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %IntLiteral.decl: %IntLiteral.type = fn_decl @IntLiteral [concrete = constants.%IntLiteral] {
// CHECK:STDOUT:     %return.patt: %pattern_type.98f = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.98f = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %return.param: ref type = out_param call_param0
// CHECK:STDOUT:     %return: ref type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %MyIntLiteral.decl: type = class_decl @MyIntLiteral [concrete = constants.%MyIntLiteral] {} {}
// CHECK:STDOUT:   %Int.decl: %Int.type = fn_decl @Int [concrete = constants.%Int] {
// CHECK:STDOUT:     %N.patt: %pattern_type.e22 = binding_pattern N [concrete]
// CHECK:STDOUT:     %N.param_patt: %pattern_type.e22 = value_param_pattern %N.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.98f = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.98f = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %N.param: %MyIntLiteral = value_param call_param0
// CHECK:STDOUT:     %MyIntLiteral.ref: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:     %N: %MyIntLiteral = bind_name N, %N.param
// CHECK:STDOUT:     %return.param: ref type = out_param call_param1
// CHECK:STDOUT:     %return: ref type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %MyInt32.decl: type = class_decl @MyInt32 [concrete = constants.%MyInt32] {} {}
// CHECK:STDOUT:   %MyInt32.Make.decl: %MyInt32.Make.type = fn_decl @MyInt32.Make [concrete = constants.%MyInt32.Make] {
// CHECK:STDOUT:     %a.patt: %pattern_type.e22 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.e22 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.81e = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.81e = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %MyInt32.ref.loc18: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:     %a.param.loc18: %MyIntLiteral = value_param call_param0
// CHECK:STDOUT:     %MyIntLiteral.ref.loc18: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:     %a.loc18: %MyIntLiteral = bind_name a, %a.param.loc18
// CHECK:STDOUT:     %return.param.loc18: ref %MyInt32 = out_param call_param1
// CHECK:STDOUT:     %return.loc18: ref %MyInt32 = return_slot %return.param.loc18
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %MyAdd.decl: %MyAdd.type = fn_decl @MyAdd [concrete = constants.%MyAdd] {
// CHECK:STDOUT:     %a.patt: %pattern_type.81e = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.81e = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %b.patt: %pattern_type.81e = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.param_patt: %pattern_type.81e = value_param_pattern %b.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.81e = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.81e = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %MyInt32.ref.loc20_37: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:     %a.param: %MyInt32 = value_param call_param0
// CHECK:STDOUT:     %MyInt32.ref.loc20_13: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:     %a: %MyInt32 = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %MyInt32 = value_param call_param1
// CHECK:STDOUT:     %MyInt32.ref.loc20_25: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:     %b: %MyInt32 = bind_name b, %b.param
// CHECK:STDOUT:     %return.param: ref %MyInt32 = out_param call_param2
// CHECK:STDOUT:     %return: ref %MyInt32 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: %pattern_type.81e = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: %pattern_type.81e = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref %MyInt32 = var %v.var_patt [concrete]
// CHECK:STDOUT:   %MyInt32.ref: type = name_ref MyInt32, %MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:   %v: ref %MyInt32 = bind_name v, %v.var [concrete = %v.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @MyIntLiteral {
// CHECK:STDOUT:   %IntLiteral.ref: %IntLiteral.type = name_ref IntLiteral, file.%IntLiteral.decl [concrete = constants.%IntLiteral]
// CHECK:STDOUT:   %IntLiteral.call: init type = call %IntLiteral.ref() [concrete = Core.IntLiteral]
// CHECK:STDOUT:   %.loc7_21.1: type = value_of_initializer %IntLiteral.call [concrete = Core.IntLiteral]
// CHECK:STDOUT:   %.loc7_21.2: type = converted %IntLiteral.call, %.loc7_21.1 [concrete = Core.IntLiteral]
// CHECK:STDOUT:   adapt_decl %.loc7_21.2 [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness Core.IntLiteral [concrete = constants.%complete_type.972]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%MyIntLiteral
// CHECK:STDOUT:   .IntLiteral = <poisoned>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @MyInt32 {
// CHECK:STDOUT:   %Int.ref: %Int.type = name_ref Int, file.%Int.decl [concrete = constants.%Int]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32.be0]
// CHECK:STDOUT:   %MyIntLiteral.ref: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:   %.loc13_16.1: %MyIntLiteral = as_compatible %int_32 [concrete = constants.%int_32.4da]
// CHECK:STDOUT:   %.loc13_16.2: %MyIntLiteral = converted %int_32, %.loc13_16.1 [concrete = constants.%int_32.4da]
// CHECK:STDOUT:   %Int.call: init type = call %Int.ref(%.loc13_16.2) [concrete = constants.%i32.builtin]
// CHECK:STDOUT:   %.loc13_32.1: type = value_of_initializer %Int.call [concrete = constants.%i32.builtin]
// CHECK:STDOUT:   %.loc13_32.2: type = converted %Int.call, %.loc13_32.1 [concrete = constants.%i32.builtin]
// CHECK:STDOUT:   adapt_decl %.loc13_32.2 [concrete]
// CHECK:STDOUT:   %MyInt32.Make.decl: %MyInt32.Make.type = fn_decl @MyInt32.Make [concrete = constants.%MyInt32.Make] {
// CHECK:STDOUT:     %a.patt: %pattern_type.e22 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.e22 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.81e = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.81e = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %MyInt32.ref.loc15: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:     %a.param.loc15: %MyIntLiteral = value_param call_param0
// CHECK:STDOUT:     %MyIntLiteral.ref.loc15: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:     %a.loc15: %MyIntLiteral = bind_name a, %a.param.loc15
// CHECK:STDOUT:     %return.param.loc15: ref %MyInt32 = out_param call_param1
// CHECK:STDOUT:     %return.loc15: ref %MyInt32 = return_slot %return.param.loc15
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness constants.%i32.builtin [concrete = constants.%complete_type.ec2]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%MyInt32
// CHECK:STDOUT:   .Int = <poisoned>
// CHECK:STDOUT:   .MyIntLiteral = <poisoned>
// CHECK:STDOUT:   .MyInt32 = <poisoned>
// CHECK:STDOUT:   .Make = %MyInt32.Make.decl
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @IntLiteral() -> type = "int_literal.make_type";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Int(%N.param: %MyIntLiteral) -> type = "int.make_type_signed";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @MyInt32.Make(%a.param.loc18: %MyIntLiteral) -> %MyInt32 = "int.convert_checked";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @MyAdd(%a.param: %MyInt32, %b.param: %MyInt32) -> %MyInt32 = "int.sadd";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %MyAdd.ref: %MyAdd.type = name_ref MyAdd, file.%MyAdd.decl [concrete = constants.%MyAdd]
// CHECK:STDOUT:   %MyInt32.ref.loc22_24: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:   %Make.ref.loc22_31: %MyInt32.Make.type = name_ref Make, @MyInt32.%MyInt32.Make.decl [concrete = constants.%MyInt32.Make]
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %MyIntLiteral.ref.loc22_42: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:   %.loc22_39.1: %MyIntLiteral = as_compatible %int_1 [concrete = constants.%int_1.383]
// CHECK:STDOUT:   %.loc22_39.2: %MyIntLiteral = converted %int_1, %.loc22_39.1 [concrete = constants.%int_1.383]
// CHECK:STDOUT:   %MyInt32.Make.call.loc22_54: init %MyInt32 = call %Make.ref.loc22_31(%.loc22_39.2) [concrete = constants.%int_1.d74]
// CHECK:STDOUT:   %MyInt32.ref.loc22_57: type = name_ref MyInt32, file.%MyInt32.decl [concrete = constants.%MyInt32]
// CHECK:STDOUT:   %Make.ref.loc22_64: %MyInt32.Make.type = name_ref Make, @MyInt32.%MyInt32.Make.decl [concrete = constants.%MyInt32.Make]
// CHECK:STDOUT:   %int_2: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %MyIntLiteral.ref.loc22_75: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:   %.loc22_72.1: %MyIntLiteral = as_compatible %int_2 [concrete = constants.%int_2.a27]
// CHECK:STDOUT:   %.loc22_72.2: %MyIntLiteral = converted %int_2, %.loc22_72.1 [concrete = constants.%int_2.a27]
// CHECK:STDOUT:   %MyInt32.Make.call.loc22_87: init %MyInt32 = call %Make.ref.loc22_64(%.loc22_72.2) [concrete = constants.%int_2.eda]
// CHECK:STDOUT:   %.loc22_54.1: %MyInt32 = value_of_initializer %MyInt32.Make.call.loc22_54 [concrete = constants.%int_1.d74]
// CHECK:STDOUT:   %.loc22_54.2: %MyInt32 = converted %MyInt32.Make.call.loc22_54, %.loc22_54.1 [concrete = constants.%int_1.d74]
// CHECK:STDOUT:   %.loc22_87.1: %MyInt32 = value_of_initializer %MyInt32.Make.call.loc22_87 [concrete = constants.%int_2.eda]
// CHECK:STDOUT:   %.loc22_87.2: %MyInt32 = converted %MyInt32.Make.call.loc22_87, %.loc22_87.1 [concrete = constants.%int_2.eda]
// CHECK:STDOUT:   %MyAdd.call: init %MyInt32 = call %MyAdd.ref(%.loc22_54.2, %.loc22_87.2) [concrete = constants.%int_3]
// CHECK:STDOUT:   assign file.%v.var, %MyAdd.call
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_bad_adapt.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %MyIntLiteral: type = class_type @MyIntLiteral [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %pattern_type.e22: type = pattern_type %MyIntLiteral [concrete]
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .MyIntLiteral = %MyIntLiteral.decl
// CHECK:STDOUT:     .Int = %Int.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %MyIntLiteral.decl: type = class_decl @MyIntLiteral [concrete = constants.%MyIntLiteral] {} {}
// CHECK:STDOUT:   %Int.decl: %Int.type = fn_decl @Int [concrete = constants.%Int] {
// CHECK:STDOUT:     %N.patt: %pattern_type.e22 = binding_pattern N [concrete]
// CHECK:STDOUT:     %N.param_patt: %pattern_type.e22 = value_param_pattern %N.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.98f = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.98f = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %N.param: %MyIntLiteral = value_param call_param0
// CHECK:STDOUT:     %MyIntLiteral.ref: type = name_ref MyIntLiteral, file.%MyIntLiteral.decl [concrete = constants.%MyIntLiteral]
// CHECK:STDOUT:     %N: %MyIntLiteral = bind_name N, %N.param
// CHECK:STDOUT:     %return.param: ref type = out_param call_param1
// CHECK:STDOUT:     %return: ref type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @MyIntLiteral {
// CHECK:STDOUT:   %.loc5_10: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc5_11: type = converted %.loc5_10, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   adapt_decl %.loc5_11 [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness constants.%empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%MyIntLiteral
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Int(%N.param: %MyIntLiteral) -> type;
// CHECK:STDOUT:
