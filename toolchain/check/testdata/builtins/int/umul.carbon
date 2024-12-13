// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/builtins/int/umul.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/builtins/int/umul.carbon

// --- int_mul.carbon

fn Mul(a: i32, b: i32) -> i32 = "int.umul";

var arr: [i32; Mul(3, 2)];
let arr_p: [i32; 6]* = &arr;

fn RuntimeCall(a: i32, b: i32) -> i32 {
  return Mul(a, b);
}

// --- overflow.carbon

package Overflow;

fn Mul(a: i32, b: i32) -> i32 = "int.umul";

let a: i32 = Mul(0x7FFF, 0x10000);
let b: i32 = Mul(0x8000, 0x10000);

// CHECK:STDOUT: --- int_mul.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [template]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [template]
// CHECK:STDOUT:   %Mul.type.1: type = fn_type @Mul.1 [template]
// CHECK:STDOUT:   %Mul: %Mul.type.1 = struct_value () [template]
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
// CHECK:STDOUT:   %int_6.1: %i32 = int_value 6 [template]
// CHECK:STDOUT:   %Convert.type.11: type = fn_type @Convert.3, @impl.2(%int_32) [template]
// CHECK:STDOUT:   %Convert.11: %Convert.type.11 = struct_value () [template]
// CHECK:STDOUT:   %interface.6: <witness> = interface_witness (%Convert.11) [template]
// CHECK:STDOUT:   %Convert.bound.3: <bound method> = bound_method %int_6.1, %Convert.11 [template]
// CHECK:STDOUT:   %Convert.specific_fn.3: <specific function> = specific_function %Convert.bound.3, @Convert.3(%int_32) [template]
// CHECK:STDOUT:   %int_6.2: Core.IntLiteral = int_value 6 [template]
// CHECK:STDOUT:   %array_type: type = array_type %int_6.2, %i32 [template]
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
// CHECK:STDOUT:     .Mul = %Mul.decl
// CHECK:STDOUT:     .arr = %arr
// CHECK:STDOUT:     .arr_p = @__global_init.%arr_p
// CHECK:STDOUT:     .RuntimeCall = %RuntimeCall.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Mul.decl: %Mul.type.1 = fn_decl @Mul.1 [template = constants.%Mul] {
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
// CHECK:STDOUT:   %Mul.ref: %Mul.type.1 = name_ref Mul, %Mul.decl [template = constants.%Mul]
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
// CHECK:STDOUT:   %int.umul: init %i32 = call %Mul.ref(%.loc4_20.2, %.loc4_23.2) [template = constants.%int_6.1]
// CHECK:STDOUT:   %impl.elem0.loc4_24: %Convert.type.5 = interface_witness_access constants.%interface.6, element0 [template = constants.%Convert.11]
// CHECK:STDOUT:   %Convert.bound.loc4_24: <bound method> = bound_method %int.umul, %impl.elem0.loc4_24 [template = constants.%Convert.bound.3]
// CHECK:STDOUT:   %Convert.specific_fn.loc4_24: <specific function> = specific_function %Convert.bound.loc4_24, @Convert.3(constants.%int_32) [template = constants.%Convert.specific_fn.3]
// CHECK:STDOUT:   %.loc4_24.1: %i32 = value_of_initializer %int.umul [template = constants.%int_6.1]
// CHECK:STDOUT:   %.loc4_24.2: %i32 = converted %int.umul, %.loc4_24.1 [template = constants.%int_6.1]
// CHECK:STDOUT:   %int.convert_checked.loc4_24: init Core.IntLiteral = call %Convert.specific_fn.loc4_24(%.loc4_24.2) [template = constants.%int_6.2]
// CHECK:STDOUT:   %.loc4_24.3: Core.IntLiteral = value_of_initializer %int.convert_checked.loc4_24 [template = constants.%int_6.2]
// CHECK:STDOUT:   %.loc4_24.4: Core.IntLiteral = converted %int.umul, %.loc4_24.3 [template = constants.%int_6.2]
// CHECK:STDOUT:   %array_type.loc4: type = array_type %.loc4_24.4, %i32 [template = constants.%array_type]
// CHECK:STDOUT:   %arr.var: ref %array_type = var arr
// CHECK:STDOUT:   %arr: ref %array_type = bind_name arr, %arr.var
// CHECK:STDOUT:   %int_32.loc5: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:   %i32.loc5: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:   %int_6: Core.IntLiteral = int_value 6 [template = constants.%int_6.2]
// CHECK:STDOUT:   %array_type.loc5: type = array_type %int_6, %i32 [template = constants.%array_type]
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
// CHECK:STDOUT: fn @Mul.1(%a.param_patt: %i32, %b.param_patt: %i32) -> %i32 = "int.umul";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @RuntimeCall(%a.param_patt: %i32, %b.param_patt: %i32) -> %i32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Mul.ref: %Mul.type.1 = name_ref Mul, file.%Mul.decl [template = constants.%Mul]
// CHECK:STDOUT:   %a.ref: %i32 = name_ref a, %a
// CHECK:STDOUT:   %b.ref: %i32 = name_ref b, %b
// CHECK:STDOUT:   %int.umul: init %i32 = call %Mul.ref(%a.ref, %b.ref)
// CHECK:STDOUT:   %.loc8_19.1: %i32 = value_of_initializer %int.umul
// CHECK:STDOUT:   %.loc8_19.2: %i32 = converted %int.umul, %.loc8_19.1
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
// CHECK:STDOUT: --- overflow.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [template]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [template]
// CHECK:STDOUT:   %Mul.type.1: type = fn_type @Mul.1 [template]
// CHECK:STDOUT:   %Mul: %Mul.type.1 = struct_value () [template]
// CHECK:STDOUT:   %int_32767.1: Core.IntLiteral = int_value 32767 [template]
// CHECK:STDOUT:   %int_65536.1: Core.IntLiteral = int_value 65536 [template]
// CHECK:STDOUT:   %Convert.type.2: type = fn_type @Convert.1, @ImplicitAs(%i32) [template]
// CHECK:STDOUT:   %Convert.type.10: type = fn_type @Convert.2, @impl.1(%int_32) [template]
// CHECK:STDOUT:   %Convert.10: %Convert.type.10 = struct_value () [template]
// CHECK:STDOUT:   %interface.5: <witness> = interface_witness (%Convert.10) [template]
// CHECK:STDOUT:   %Convert.bound.1: <bound method> = bound_method %int_32767.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.1: <specific function> = specific_function %Convert.bound.1, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_32767.2: %i32 = int_value 32767 [template]
// CHECK:STDOUT:   %Convert.bound.2: <bound method> = bound_method %int_65536.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.2: <specific function> = specific_function %Convert.bound.2, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_65536.2: %i32 = int_value 65536 [template]
// CHECK:STDOUT:   %int_2147418112: %i32 = int_value 2147418112 [template]
// CHECK:STDOUT:   %int_32768.1: Core.IntLiteral = int_value 32768 [template]
// CHECK:STDOUT:   %Convert.bound.3: <bound method> = bound_method %int_32768.1, %Convert.10 [template]
// CHECK:STDOUT:   %Convert.specific_fn.3: <specific function> = specific_function %Convert.bound.3, @Convert.2(%int_32) [template]
// CHECK:STDOUT:   %int_32768.2: %i32 = int_value 32768 [template]
// CHECK:STDOUT:   %int_-2147483648: %i32 = int_value -2147483648 [template]
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
// CHECK:STDOUT:     .Mul = %Mul.decl
// CHECK:STDOUT:     .a = @__global_init.%a
// CHECK:STDOUT:     .b = @__global_init.%b
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Mul.decl: %Mul.type.1 = fn_decl @Mul.1 [template = constants.%Mul] {
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
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Mul.1(%a.param_patt: %i32, %b.param_patt: %i32) -> %i32 = "int.umul";
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Mul.ref.loc6: %Mul.type.1 = name_ref Mul, file.%Mul.decl [template = constants.%Mul]
// CHECK:STDOUT:   %int_32767: Core.IntLiteral = int_value 32767 [template = constants.%int_32767.1]
// CHECK:STDOUT:   %int_65536.loc6: Core.IntLiteral = int_value 65536 [template = constants.%int_65536.1]
// CHECK:STDOUT:   %impl.elem0.loc6_18: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc6_18: <bound method> = bound_method %int_32767, %impl.elem0.loc6_18 [template = constants.%Convert.bound.1]
// CHECK:STDOUT:   %Convert.specific_fn.loc6_18: <specific function> = specific_function %Convert.bound.loc6_18, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.1]
// CHECK:STDOUT:   %int.convert_checked.loc6_18: init %i32 = call %Convert.specific_fn.loc6_18(%int_32767) [template = constants.%int_32767.2]
// CHECK:STDOUT:   %.loc6_18.1: %i32 = value_of_initializer %int.convert_checked.loc6_18 [template = constants.%int_32767.2]
// CHECK:STDOUT:   %.loc6_18.2: %i32 = converted %int_32767, %.loc6_18.1 [template = constants.%int_32767.2]
// CHECK:STDOUT:   %impl.elem0.loc6_26: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc6_26: <bound method> = bound_method %int_65536.loc6, %impl.elem0.loc6_26 [template = constants.%Convert.bound.2]
// CHECK:STDOUT:   %Convert.specific_fn.loc6_26: <specific function> = specific_function %Convert.bound.loc6_26, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.2]
// CHECK:STDOUT:   %int.convert_checked.loc6_26: init %i32 = call %Convert.specific_fn.loc6_26(%int_65536.loc6) [template = constants.%int_65536.2]
// CHECK:STDOUT:   %.loc6_26.1: %i32 = value_of_initializer %int.convert_checked.loc6_26 [template = constants.%int_65536.2]
// CHECK:STDOUT:   %.loc6_26.2: %i32 = converted %int_65536.loc6, %.loc6_26.1 [template = constants.%int_65536.2]
// CHECK:STDOUT:   %int.umul.loc6: init %i32 = call %Mul.ref.loc6(%.loc6_18.2, %.loc6_26.2) [template = constants.%int_2147418112]
// CHECK:STDOUT:   %.loc6_34.1: %i32 = value_of_initializer %int.umul.loc6 [template = constants.%int_2147418112]
// CHECK:STDOUT:   %.loc6_34.2: %i32 = converted %int.umul.loc6, %.loc6_34.1 [template = constants.%int_2147418112]
// CHECK:STDOUT:   %a: %i32 = bind_name a, %.loc6_34.2
// CHECK:STDOUT:   %Mul.ref.loc7: %Mul.type.1 = name_ref Mul, file.%Mul.decl [template = constants.%Mul]
// CHECK:STDOUT:   %int_32768: Core.IntLiteral = int_value 32768 [template = constants.%int_32768.1]
// CHECK:STDOUT:   %int_65536.loc7: Core.IntLiteral = int_value 65536 [template = constants.%int_65536.1]
// CHECK:STDOUT:   %impl.elem0.loc7_18: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc7_18: <bound method> = bound_method %int_32768, %impl.elem0.loc7_18 [template = constants.%Convert.bound.3]
// CHECK:STDOUT:   %Convert.specific_fn.loc7_18: <specific function> = specific_function %Convert.bound.loc7_18, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.3]
// CHECK:STDOUT:   %int.convert_checked.loc7_18: init %i32 = call %Convert.specific_fn.loc7_18(%int_32768) [template = constants.%int_32768.2]
// CHECK:STDOUT:   %.loc7_18.1: %i32 = value_of_initializer %int.convert_checked.loc7_18 [template = constants.%int_32768.2]
// CHECK:STDOUT:   %.loc7_18.2: %i32 = converted %int_32768, %.loc7_18.1 [template = constants.%int_32768.2]
// CHECK:STDOUT:   %impl.elem0.loc7_26: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.10]
// CHECK:STDOUT:   %Convert.bound.loc7_26: <bound method> = bound_method %int_65536.loc7, %impl.elem0.loc7_26 [template = constants.%Convert.bound.2]
// CHECK:STDOUT:   %Convert.specific_fn.loc7_26: <specific function> = specific_function %Convert.bound.loc7_26, @Convert.2(constants.%int_32) [template = constants.%Convert.specific_fn.2]
// CHECK:STDOUT:   %int.convert_checked.loc7_26: init %i32 = call %Convert.specific_fn.loc7_26(%int_65536.loc7) [template = constants.%int_65536.2]
// CHECK:STDOUT:   %.loc7_26.1: %i32 = value_of_initializer %int.convert_checked.loc7_26 [template = constants.%int_65536.2]
// CHECK:STDOUT:   %.loc7_26.2: %i32 = converted %int_65536.loc7, %.loc7_26.1 [template = constants.%int_65536.2]
// CHECK:STDOUT:   %int.umul.loc7: init %i32 = call %Mul.ref.loc7(%.loc7_18.2, %.loc7_26.2) [template = constants.%int_-2147483648]
// CHECK:STDOUT:   %.loc7_34.1: %i32 = value_of_initializer %int.umul.loc7 [template = constants.%int_-2147483648]
// CHECK:STDOUT:   %.loc7_34.2: %i32 = converted %int.umul.loc7, %.loc7_34.1 [template = constants.%int_-2147483648]
// CHECK:STDOUT:   %b: %i32 = bind_name b, %.loc7_34.2
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
