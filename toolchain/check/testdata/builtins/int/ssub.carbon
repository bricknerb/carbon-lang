// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/builtins/int/ssub.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/builtins/int/ssub.carbon

// --- int_sub.carbon

fn Sub(a: i32, b: i32) -> i32 = "int.ssub";

var arr: [i32; Sub(3, 2)];
let arr_p: [i32; 1]* = &arr;

fn RuntimeCall(a: i32, b: i32) -> i32 {
  return Sub(a, b);
}

// --- fail_overflow.carbon

package FailOverflow;

fn Sub(a: i32, b: i32) -> i32 = "int.ssub";

let a: i32 = Sub(0, 0x7FFFFFFF);
let b: i32 = Sub(Sub(0, 0x7FFFFFFF), 1);
// CHECK:STDERR: fail_overflow.carbon:[[@LINE+3]]:14: error: integer overflow in calculation -2147483647 - 2 [CompileTimeIntegerOverflow]
// CHECK:STDERR: let c: i32 = Sub(Sub(0, 0x7FFFFFFF), 2);
// CHECK:STDERR:              ^~~~~~~~~~~~~~~~~~~~~~~~~~
let c: i32 = Sub(Sub(0, 0x7FFFFFFF), 2);

// CHECK:STDOUT: --- int_sub.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [template]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [template]
// CHECK:STDOUT:   %Sub.type.1: type = fn_type @Sub.1 [template]
// CHECK:STDOUT:   %Sub: %Sub.type.1 = struct_value () [template]
// CHECK:STDOUT:   %int_3.1: Core.IntLiteral = int_value 3 [template]
// CHECK:STDOUT:   %int_2.1: Core.IntLiteral = int_value 2 [template]
// CHECK:STDOUT:   %Convert.type.2: type = fn_type @Convert.1, @ImplicitAs(%i32) [template]
// CHECK:STDOUT:   %Convert.type.5: type = fn_type @Convert.1, @ImplicitAs(Core.IntLiteral) [template]
// CHECK:STDOUT:   %Convert.type.10: type = fn_type @Convert.2, @impl.1(%int_32) [template]
// CHECK:STDOUT:   %Convert.10: %Convert.type.10 = struct_value () [template]
// CHECK:STDOUT:   %interface.5: <witness> = interface_witness (%Convert.10) [template]
// CHECK:STDOUT:   %Convert.bound.1: <bound method> = bound_method %int_3.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.1: <specific function> = specific_function %Convert.bound.1, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_3.2: %i32 = int_value 3 [template]
// CHECK:STDOUT:   %Convert.bound.2: <bound method> = bound_method %int_2.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.2: <specific function> = specific_function %Convert.bound.2, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_2.2: %i32 = int_value 2 [template]
// CHECK:STDOUT:   %int_1.1: %i32 = int_value 1 [template]
// CHECK:STDOUT:   %Convert.type.11: type = fn_type @Convert.3, @impl.2(%int_32) [template]
// CHECK:STDOUT:   %Convert.11: %Convert.type.11 = struct_value () [template]
// CHECK:STDOUT:   %interface.6: <witness> = interface_witness (%Convert.11) [template]
// CHECK:STDOUT:   %Convert.bound.3: <bound method> = bound_method %int_1.1, %Convert.11 [template]
// CHECK:STDOUT:   %Convert.specific_fn.3: <specific function> = specific_function %Convert.bound.3, @Convert.3(%int_32) [template]
// CHECK:STDOUT:   %int_1.2: Core.IntLiteral = int_value 1 [template]
// CHECK:STDOUT:   %array_type: type = array_type %int_1.2, %i32 [template]
// CHECK:STDOUT:   %ptr: type = ptr_type %array_type [template]
// CHECK:STDOUT:   %RuntimeCall.type: type = fn_type @RuntimeCall [template]
// CHECK:STDOUT:   %RuntimeCall: %RuntimeCall.type = struct_value () [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [template] {
// CHECK:STDOUT:     .Int = %import_ref.1
// CHECK:STDOUT:     .ImplicitAs = %import_ref.5
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Sub = %Sub.decl
// CHECK:STDOUT:     .arr = %arr
// CHECK:STDOUT:     .arr_p = @__global_init.%arr_p
// CHECK:STDOUT:     .RuntimeCall = %RuntimeCall.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Sub.decl: %Sub.type.1 = fn_decl @Sub.1 [template = constants.%Sub] {
// CHECK:STDOUT:     %a.patt: %i32 = binding_pattern a
// CHECK:STDOUT:     %a.param_patt: %i32 = value_param_pattern %a.patt, runtime_param0
// CHECK:STDOUT:     %b.patt: %i32 = binding_pattern b
// CHECK:STDOUT:     %b.param_patt: %i32 = value_param_pattern %b.patt, runtime_param1
// CHECK:STDOUT:     %return.patt: %i32 = return_slot_pattern
// CHECK:STDOUT:     %return.param_patt: %i32 = out_param_pattern %return.patt, runtime_param2
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc2_11: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc2_11: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %int_32.loc2_19: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc2_19: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %int_32.loc2_27: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc2_27: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %a.param: %i32 = value_param runtime_param0
// CHECK:STDOUT:     %a: %i32 = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %i32 = value_param runtime_param1
// CHECK:STDOUT:     %b: %i32 = bind_name b, %b.param
// CHECK:STDOUT:     %return.param: ref %i32 = out_param runtime_param2
// CHECK:STDOUT:     %return: ref %i32 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_32.loc4: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:   %i32.loc4: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:   %Sub.ref: %Sub.type.1 = name_ref Sub, %Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %int_3: Core.IntLiteral = int_value 3 [template = constants.%int_3.1]
// CHECK:STDOUT:   %int_2: Core.IntLiteral = int_value 2 [template = constants.%int_2.1]
// CHECK:STDOUT:   %impl.elem0.loc4_20: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc4_20: <bound method> = bound_method %int_3, %impl.elem0.loc4_20 [template = constants.%Convert.bound.1]
// CHECK:STDOUT:   %Convert.specific_fn.loc4_20: <specific function> = specific_function %Convert.bound.loc4_20, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.1]
// CHECK:STDOUT:   %int.convert_checked.loc4_20: init %i32 = call %Convert.specific_fn.loc4_20(%int_3) [template = constants.%int_3.2]
// CHECK:STDOUT:   %.loc4_20.1: %i32 = value_of_initializer %int.convert_checked.loc4_20 [template = constants.%int_3.2]
// CHECK:STDOUT:   %.loc4_20.2: %i32 = converted %int_3, %.loc4_20.1 [template = constants.%int_3.2]
// CHECK:STDOUT:   %impl.elem0.loc4_23: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc4_23: <bound method> = bound_method %int_2, %impl.elem0.loc4_23 [template = constants.%Convert.bound.2]
// CHECK:STDOUT:   %Convert.specific_fn.loc4_23: <specific function> = specific_function %Convert.bound.loc4_23, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.2]
// CHECK:STDOUT:   %int.convert_checked.loc4_23: init %i32 = call %Convert.specific_fn.loc4_23(%int_2) [template = constants.%int_2.2]
// CHECK:STDOUT:   %.loc4_23.1: %i32 = value_of_initializer %int.convert_checked.loc4_23 [template = constants.%int_2.2]
// CHECK:STDOUT:   %.loc4_23.2: %i32 = converted %int_2, %.loc4_23.1 [template = constants.%int_2.2]
// CHECK:STDOUT:   %int.ssub: init %i32 = call %Sub.ref(%.loc4_20.2, %.loc4_23.2) [template = constants.%int_1.1]
// CHECK:STDOUT:   %impl.elem0.loc4_24: %Convert.type.5 = interface_witness_access constants.%interface.6, element0 [template = constants.%Convert.11]
// CHECK:STDOUT:   %Convert.bound.loc4_24: <bound method> = bound_method %int.ssub, %impl.elem0.loc4_24 [template = constants.%Convert.bound.3]
// CHECK:STDOUT:   %Convert.specific_fn.loc4_24: <specific function> = specific_function %Convert.bound.loc4_24, @Convert.3(constants.%int_32) [template = constants.%Convert.specific_fn.3]
// CHECK:STDOUT:   %.loc4_24.1: %i32 = value_of_initializer %int.ssub [template = constants.%int_1.1]
// CHECK:STDOUT:   %.loc4_24.2: %i32 = converted %int.ssub, %.loc4_24.1 [template = constants.%int_1.1]
// CHECK:STDOUT:   %int.convert_checked.loc4_24: init Core.IntLiteral = call %Convert.specific_fn.loc4_24(%.loc4_24.2) [template = constants.%int_1.2]
// CHECK:STDOUT:   %.loc4_24.3: Core.IntLiteral = value_of_initializer %int.convert_checked.loc4_24 [template = constants.%int_1.2]
// CHECK:STDOUT:   %.loc4_24.4: Core.IntLiteral = converted %int.ssub, %.loc4_24.3 [template = constants.%int_1.2]
// CHECK:STDOUT:   %array_type.loc4: type = array_type %.loc4_24.4, %i32 [template = constants.%array_type]
// CHECK:STDOUT:   %arr.var: ref %array_type = var arr
// CHECK:STDOUT:   %arr: ref %array_type = bind_name arr, %arr.var
// CHECK:STDOUT:   %int_32.loc5: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:   %i32.loc5: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [template = constants.%int_1.2]
// CHECK:STDOUT:   %array_type.loc5: type = array_type %int_1, %i32 [template = constants.%array_type]
// CHECK:STDOUT:   %ptr: type = ptr_type %array_type [template = constants.%ptr]
// CHECK:STDOUT:   %RuntimeCall.decl: %RuntimeCall.type = fn_decl @RuntimeCall [template = constants.%RuntimeCall] {
// CHECK:STDOUT:     %a.patt: %i32 = binding_pattern a
// CHECK:STDOUT:     %a.param_patt: %i32 = value_param_pattern %a.patt, runtime_param0
// CHECK:STDOUT:     %b.patt: %i32 = binding_pattern b
// CHECK:STDOUT:     %b.param_patt: %i32 = value_param_pattern %b.patt, runtime_param1
// CHECK:STDOUT:     %return.patt: %i32 = return_slot_pattern
// CHECK:STDOUT:     %return.param_patt: %i32 = out_param_pattern %return.patt, runtime_param2
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc7_19: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_19: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_27: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_27: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_35: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_35: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %a.param: %i32 = value_param runtime_param0
// CHECK:STDOUT:     %a: %i32 = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %i32 = value_param runtime_param1
// CHECK:STDOUT:     %b: %i32 = bind_name b, %b.param
// CHECK:STDOUT:     %return.param: ref %i32 = out_param runtime_param2
// CHECK:STDOUT:     %return: ref %i32 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Sub.1(%a.param_patt: %i32, %b.param_patt: %i32) -> %i32 = "int.ssub";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @RuntimeCall(%a.param_patt: %i32, %b.param_patt: %i32) -> %i32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Sub.ref: %Sub.type.1 = name_ref Sub, file.%Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %a.ref: %i32 = name_ref a, %a
// CHECK:STDOUT:   %b.ref: %i32 = name_ref b, %b
// CHECK:STDOUT:   %int.ssub: init %i32 = call %Sub.ref(%a.ref, %b.ref)
// CHECK:STDOUT:   %.loc8_19.1: %i32 = value_of_initializer %int.ssub
// CHECK:STDOUT:   %.loc8_19.2: %i32 = converted %int.ssub, %.loc8_19.1
// CHECK:STDOUT:   return %.loc8_19.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %arr.ref: ref %array_type = name_ref arr, file.%arr
// CHECK:STDOUT:   %addr: %ptr = addr_of %arr.ref
// CHECK:STDOUT:   %arr_p: %ptr = bind_name arr_p, %addr
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_overflow.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [template]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [template]
// CHECK:STDOUT:   %Sub.type.1: type = fn_type @Sub.1 [template]
// CHECK:STDOUT:   %Sub: %Sub.type.1 = struct_value () [template]
// CHECK:STDOUT:   %int_0.1: Core.IntLiteral = int_value 0 [template]
// CHECK:STDOUT:   %int_2147483647.1: Core.IntLiteral = int_value 2147483647 [template]
// CHECK:STDOUT:   %Convert.type.2: type = fn_type @Convert.1, @ImplicitAs(%i32) [template]
// CHECK:STDOUT:   %Convert.type.10: type = fn_type @Convert.2, @impl.1(%int_32) [template]
// CHECK:STDOUT:   %Convert.10: %Convert.type.10 = struct_value () [template]
// CHECK:STDOUT:   %interface.5: <witness> = interface_witness (%Convert.10) [template]
// CHECK:STDOUT:   %Convert.bound.1: <bound method> = bound_method %int_0.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.1: <specific function> = specific_function %Convert.bound.1, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_0.2: %i32 = int_value 0 [template]
// CHECK:STDOUT:   %Convert.bound.2: <bound method> = bound_method %int_2147483647.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.2: <specific function> = specific_function %Convert.bound.2, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_2147483647.2: %i32 = int_value 2147483647 [template]
// CHECK:STDOUT:   %int_-2147483647: %i32 = int_value -2147483647 [template]
// CHECK:STDOUT:   %int_1.1: Core.IntLiteral = int_value 1 [template]
// CHECK:STDOUT:   %Convert.bound.3: <bound method> = bound_method %int_1.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.3: <specific function> = specific_function %Convert.bound.3, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_1.2: %i32 = int_value 1 [template]
// CHECK:STDOUT:   %int_-2147483648: %i32 = int_value -2147483648 [template]
// CHECK:STDOUT:   %int_2.1: Core.IntLiteral = int_value 2 [template]
// CHECK:STDOUT:   %Convert.bound.4: <bound method> = bound_method %int_2.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.4: <specific function> = specific_function %Convert.bound.4, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_2.2: %i32 = int_value 2 [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [template] {
// CHECK:STDOUT:     .Int = %import_ref.1
// CHECK:STDOUT:     .ImplicitAs = %import_ref.5
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Sub = %Sub.decl
// CHECK:STDOUT:     .a = @__global_init.%a
// CHECK:STDOUT:     .b = @__global_init.%b
// CHECK:STDOUT:     .c = @__global_init.%c
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Sub.decl: %Sub.type.1 = fn_decl @Sub.1 [template = constants.%Sub] {
// CHECK:STDOUT:     %a.patt: %i32 = binding_pattern a
// CHECK:STDOUT:     %a.param_patt: %i32 = value_param_pattern %a.patt, runtime_param0
// CHECK:STDOUT:     %b.patt: %i32 = binding_pattern b
// CHECK:STDOUT:     %b.param_patt: %i32 = value_param_pattern %b.patt, runtime_param1
// CHECK:STDOUT:     %return.patt: %i32 = return_slot_pattern
// CHECK:STDOUT:     %return.param_patt: %i32 = out_param_pattern %return.patt, runtime_param2
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc4_11: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc4_11: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %int_32.loc4_19: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc4_19: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %int_32.loc4_27: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc4_27: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %a.param: %i32 = value_param runtime_param0
// CHECK:STDOUT:     %a: %i32 = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %i32 = value_param runtime_param1
// CHECK:STDOUT:     %b: %i32 = bind_name b, %b.param
// CHECK:STDOUT:     %return.param: ref %i32 = out_param runtime_param2
// CHECK:STDOUT:     %return: ref %i32 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_32.loc6: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:   %i32.loc6: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:   %int_32.loc7: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:   %i32.loc7: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:   %int_32.loc11: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:   %i32.loc11: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Sub.1(%a.param_patt: %i32, %b.param_patt: %i32) -> %i32 = "int.ssub";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Sub.ref.loc6: %Sub.type.1 = name_ref Sub, file.%Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %int_0.loc6: Core.IntLiteral = int_value 0 [template = constants.%int_0.1]
// CHECK:STDOUT:   %int_2147483647.loc6: Core.IntLiteral = int_value 2147483647 [template = constants.%int_2147483647.1]
// CHECK:STDOUT:   %impl.elem0.loc6_18: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc6_18: <bound method> = bound_method %int_0.loc6, %impl.elem0.loc6_18 [template = constants.%Convert.bound.1]
// CHECK:STDOUT:   %Convert.specific_fn.loc6_18: <specific function> = specific_function %Convert.bound.loc6_18, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.1]
// CHECK:STDOUT:   %int.convert_checked.loc6_18: init %i32 = call %Convert.specific_fn.loc6_18(%int_0.loc6) [template = constants.%int_0.2]
// CHECK:STDOUT:   %.loc6_18.1: %i32 = value_of_initializer %int.convert_checked.loc6_18 [template = constants.%int_0.2]
// CHECK:STDOUT:   %.loc6_18.2: %i32 = converted %int_0.loc6, %.loc6_18.1 [template = constants.%int_0.2]
// CHECK:STDOUT:   %impl.elem0.loc6_21: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc6_21: <bound method> = bound_method %int_2147483647.loc6, %impl.elem0.loc6_21 [template = constants.%Convert.bound.2]
// CHECK:STDOUT:   %Convert.specific_fn.loc6_21: <specific function> = specific_function %Convert.bound.loc6_21, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.2]
// CHECK:STDOUT:   %int.convert_checked.loc6_21: init %i32 = call %Convert.specific_fn.loc6_21(%int_2147483647.loc6) [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc6_21.1: %i32 = value_of_initializer %int.convert_checked.loc6_21 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc6_21.2: %i32 = converted %int_2147483647.loc6, %.loc6_21.1 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %int.ssub.loc6: init %i32 = call %Sub.ref.loc6(%.loc6_18.2, %.loc6_21.2) [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %.loc6_32.1: %i32 = value_of_initializer %int.ssub.loc6 [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %.loc6_32.2: %i32 = converted %int.ssub.loc6, %.loc6_32.1 [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %a: %i32 = bind_name a, %.loc6_32.2
// CHECK:STDOUT:   %Sub.ref.loc7_14: %Sub.type.1 = name_ref Sub, file.%Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %Sub.ref.loc7_18: %Sub.type.1 = name_ref Sub, file.%Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %int_0.loc7: Core.IntLiteral = int_value 0 [template = constants.%int_0.1]
// CHECK:STDOUT:   %int_2147483647.loc7: Core.IntLiteral = int_value 2147483647 [template = constants.%int_2147483647.1]
// CHECK:STDOUT:   %impl.elem0.loc7_22: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc7_22: <bound method> = bound_method %int_0.loc7, %impl.elem0.loc7_22 [template = constants.%Convert.bound.1]
// CHECK:STDOUT:   %Convert.specific_fn.loc7_22: <specific function> = specific_function %Convert.bound.loc7_22, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.1]
// CHECK:STDOUT:   %int.convert_checked.loc7_22: init %i32 = call %Convert.specific_fn.loc7_22(%int_0.loc7) [template = constants.%int_0.2]
// CHECK:STDOUT:   %.loc7_22.1: %i32 = value_of_initializer %int.convert_checked.loc7_22 [template = constants.%int_0.2]
// CHECK:STDOUT:   %.loc7_22.2: %i32 = converted %int_0.loc7, %.loc7_22.1 [template = constants.%int_0.2]
// CHECK:STDOUT:   %impl.elem0.loc7_25: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc7_25: <bound method> = bound_method %int_2147483647.loc7, %impl.elem0.loc7_25 [template = constants.%Convert.bound.2]
// CHECK:STDOUT:   %Convert.specific_fn.loc7_25: <specific function> = specific_function %Convert.bound.loc7_25, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.2]
// CHECK:STDOUT:   %int.convert_checked.loc7_25: init %i32 = call %Convert.specific_fn.loc7_25(%int_2147483647.loc7) [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc7_25.1: %i32 = value_of_initializer %int.convert_checked.loc7_25 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc7_25.2: %i32 = converted %int_2147483647.loc7, %.loc7_25.1 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %int.ssub.loc7_35: init %i32 = call %Sub.ref.loc7_18(%.loc7_22.2, %.loc7_25.2) [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [template = constants.%int_1.1]
// CHECK:STDOUT:   %.loc7_35.1: %i32 = value_of_initializer %int.ssub.loc7_35 [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %.loc7_35.2: %i32 = converted %int.ssub.loc7_35, %.loc7_35.1 [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %impl.elem0.loc7_38: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc7_38: <bound method> = bound_method %int_1, %impl.elem0.loc7_38 [template = constants.%Convert.bound.3]
// CHECK:STDOUT:   %Convert.specific_fn.loc7_38: <specific function> = specific_function %Convert.bound.loc7_38, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.3]
// CHECK:STDOUT:   %int.convert_checked.loc7_38: init %i32 = call %Convert.specific_fn.loc7_38(%int_1) [template = constants.%int_1.2]
// CHECK:STDOUT:   %.loc7_38.1: %i32 = value_of_initializer %int.convert_checked.loc7_38 [template = constants.%int_1.2]
// CHECK:STDOUT:   %.loc7_38.2: %i32 = converted %int_1, %.loc7_38.1 [template = constants.%int_1.2]
// CHECK:STDOUT:   %int.ssub.loc7_39: init %i32 = call %Sub.ref.loc7_14(%.loc7_35.2, %.loc7_38.2) [template = constants.%int_-2147483648]
// CHECK:STDOUT:   %.loc7_40.1: %i32 = value_of_initializer %int.ssub.loc7_39 [template = constants.%int_-2147483648]
// CHECK:STDOUT:   %.loc7_40.2: %i32 = converted %int.ssub.loc7_39, %.loc7_40.1 [template = constants.%int_-2147483648]
// CHECK:STDOUT:   %b: %i32 = bind_name b, %.loc7_40.2
// CHECK:STDOUT:   %Sub.ref.loc11_14: %Sub.type.1 = name_ref Sub, file.%Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %Sub.ref.loc11_18: %Sub.type.1 = name_ref Sub, file.%Sub.decl [template = constants.%Sub]
// CHECK:STDOUT:   %int_0.loc11: Core.IntLiteral = int_value 0 [template = constants.%int_0.1]
// CHECK:STDOUT:   %int_2147483647.loc11: Core.IntLiteral = int_value 2147483647 [template = constants.%int_2147483647.1]
// CHECK:STDOUT:   %impl.elem0.loc11_22: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc11_22: <bound method> = bound_method %int_0.loc11, %impl.elem0.loc11_22 [template = constants.%Convert.bound.1]
// CHECK:STDOUT:   %Convert.specific_fn.loc11_22: <specific function> = specific_function %Convert.bound.loc11_22, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.1]
// CHECK:STDOUT:   %int.convert_checked.loc11_22: init %i32 = call %Convert.specific_fn.loc11_22(%int_0.loc11) [template = constants.%int_0.2]
// CHECK:STDOUT:   %.loc11_22.1: %i32 = value_of_initializer %int.convert_checked.loc11_22 [template = constants.%int_0.2]
// CHECK:STDOUT:   %.loc11_22.2: %i32 = converted %int_0.loc11, %.loc11_22.1 [template = constants.%int_0.2]
// CHECK:STDOUT:   %impl.elem0.loc11_25: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc11_25: <bound method> = bound_method %int_2147483647.loc11, %impl.elem0.loc11_25 [template = constants.%Convert.bound.2]
// CHECK:STDOUT:   %Convert.specific_fn.loc11_25: <specific function> = specific_function %Convert.bound.loc11_25, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.2]
// CHECK:STDOUT:   %int.convert_checked.loc11_25: init %i32 = call %Convert.specific_fn.loc11_25(%int_2147483647.loc11) [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc11_25.1: %i32 = value_of_initializer %int.convert_checked.loc11_25 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc11_25.2: %i32 = converted %int_2147483647.loc11, %.loc11_25.1 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %int.ssub.loc11_35: init %i32 = call %Sub.ref.loc11_18(%.loc11_22.2, %.loc11_25.2) [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %int_2: Core.IntLiteral = int_value 2 [template = constants.%int_2.1]
// CHECK:STDOUT:   %.loc11_35.1: %i32 = value_of_initializer %int.ssub.loc11_35 [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %.loc11_35.2: %i32 = converted %int.ssub.loc11_35, %.loc11_35.1 [template = constants.%int_-2147483647]
// CHECK:STDOUT:   %impl.elem0.loc11_38: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc11_38: <bound method> = bound_method %int_2, %impl.elem0.loc11_38 [template = constants.%Convert.bound.4]
// CHECK:STDOUT:   %Convert.specific_fn.loc11_38: <specific function> = specific_function %Convert.bound.loc11_38, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.4]
// CHECK:STDOUT:   %int.convert_checked.loc11_38: init %i32 = call %Convert.specific_fn.loc11_38(%int_2) [template = constants.%int_2.2]
// CHECK:STDOUT:   %.loc11_38.1: %i32 = value_of_initializer %int.convert_checked.loc11_38 [template = constants.%int_2.2]
// CHECK:STDOUT:   %.loc11_38.2: %i32 = converted %int_2, %.loc11_38.1 [template = constants.%int_2.2]
// CHECK:STDOUT:   %int.ssub.loc11_39: init %i32 = call %Sub.ref.loc11_14(%.loc11_35.2, %.loc11_38.2) [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc11_40.1: %i32 = value_of_initializer %int.ssub.loc11_39 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %.loc11_40.2: %i32 = converted %int.ssub.loc11_39, %.loc11_40.1 [template = constants.%int_2147483647.2]
// CHECK:STDOUT:   %c: %i32 = bind_name c, %.loc11_40.2
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
