// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/eval/fail_symbolic.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/eval/fail_symbolic.carbon

// TODO: This should work.
fn G(N:! i32) {
  // CHECK:STDERR: fail_symbolic.carbon:[[@LINE+3]]:16: error: semantics TODO: `symbolic array bound` [SemanticsTodo]
  // CHECK:STDERR:   var k: [i32; N];
  // CHECK:STDERR:                ^
  var k: [i32; N];
}

// CHECK:STDOUT: --- fail_symbolic.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [template]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [template]
// CHECK:STDOUT:   %N.2: %i32 = bind_symbolic_name N, 0 [symbolic]
// CHECK:STDOUT:   %N.patt.2: %i32 = symbolic_binding_pattern N, 0 [symbolic]
// CHECK:STDOUT:   %G.type: type = fn_type @G [template]
// CHECK:STDOUT:   %G: %G.type = struct_value () [template]
// CHECK:STDOUT:   %Convert.type.2: type = fn_type @Convert.1, @ImplicitAs(Core.IntLiteral) [template]
// CHECK:STDOUT:   %Convert.type.9: type = fn_type @Convert.3, @impl.2(%int_32) [template]
// CHECK:STDOUT:   %Convert.9: %Convert.type.9 = struct_value () [template]
// CHECK:STDOUT:   %interface.5: <witness> = interface_witness (%Convert.9) [template]
// CHECK:STDOUT:   %Convert.bound: <bound method> = bound_method %N.2, %Convert.9 [symbolic]
// CHECK:STDOUT:   %Convert.specific_fn: <specific function> = specific_function %Convert.bound, @Convert.3(%int_32) [symbolic]
// CHECK:STDOUT:   %int.convert_checked: init Core.IntLiteral = call %Convert.specific_fn(%N.2) [symbolic]
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
// CHECK:STDOUT:     .G = %G.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %G.decl: %G.type = fn_decl @G [template = constants.%G] {
// CHECK:STDOUT:     %N.patt.loc12_6.1: %i32 = symbolic_binding_pattern N, 0 [symbolic = %N.patt.loc12_6.2 (constants.%N.patt.2)]
// CHECK:STDOUT:     %N.param_patt: %i32 = value_param_pattern %N.patt.loc12_6.1, runtime_param<invalid> [symbolic = %N.patt.loc12_6.2 (constants.%N.patt.2)]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc12: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc12: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %N.param: %i32 = value_param runtime_param<invalid>
// CHECK:STDOUT:     %N.loc12_6.1: %i32 = bind_symbolic_name N, 0, %N.param [symbolic = %N.loc12_6.2 (constants.%N.2)]
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @G(%N.loc12_6.1: %i32) {
// CHECK:STDOUT:   %N.loc12_6.2: %i32 = bind_symbolic_name N, 0 [symbolic = %N.loc12_6.2 (constants.%N.2)]
// CHECK:STDOUT:   %N.patt.loc12_6.2: %i32 = symbolic_binding_pattern N, 0 [symbolic = %N.patt.loc12_6.2 (constants.%N.patt.2)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %Convert.bound.loc16_16.2: <bound method> = bound_method %N.loc12_6.2, constants.%Convert.9 [symbolic = %Convert.bound.loc16_16.2 (constants.%Convert.bound)]
// CHECK:STDOUT:   %Convert.specific_fn.loc16_16.2: <specific function> = specific_function %Convert.bound.loc16_16.2, @Convert.3(constants.%int_32) [symbolic = %Convert.specific_fn.loc16_16.2 (constants.%Convert.specific_fn)]
// CHECK:STDOUT:   %int.convert_checked.loc16_16.2: init Core.IntLiteral = call %Convert.specific_fn.loc16_16.2(%N.loc12_6.2) [symbolic = %int.convert_checked.loc16_16.2 (constants.%int.convert_checked)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%N.param_patt: %i32) {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     %int_32.loc16: Core.IntLiteral = int_value 32 [template = constants.%int_32]
// CHECK:STDOUT:     %i32.loc16: type = class_type @Int, @Int(constants.%int_32) [template = constants.%i32]
// CHECK:STDOUT:     %N.ref: %i32 = name_ref N, %N.loc12_6.1 [symbolic = %N.loc12_6.2 (constants.%N.2)]
// CHECK:STDOUT:     %impl.elem0: %Convert.type.2 = interface_witness_access constants.%interface.5, element0 [template = constants.%Convert.9]
// CHECK:STDOUT:     %Convert.bound.loc16_16.1: <bound method> = bound_method %N.ref, %impl.elem0 [symbolic = %Convert.bound.loc16_16.2 (constants.%Convert.bound)]
// CHECK:STDOUT:     %Convert.specific_fn.loc16_16.1: <specific function> = specific_function %Convert.bound.loc16_16.1, @Convert.3(constants.%int_32) [symbolic = %Convert.specific_fn.loc16_16.2 (constants.%Convert.specific_fn)]
// CHECK:STDOUT:     %int.convert_checked.loc16_16.1: init Core.IntLiteral = call %Convert.specific_fn.loc16_16.1(%N.ref) [symbolic = %int.convert_checked.loc16_16.2 (constants.%int.convert_checked)]
// CHECK:STDOUT:     %.loc16_16.1: Core.IntLiteral = value_of_initializer %int.convert_checked.loc16_16.1 [symbolic = %int.convert_checked.loc16_16.2 (constants.%int.convert_checked)]
// CHECK:STDOUT:     %.loc16_16.2: Core.IntLiteral = converted %N.ref, %.loc16_16.1 [symbolic = %int.convert_checked.loc16_16.2 (constants.%int.convert_checked)]
// CHECK:STDOUT:     %array_type: type = array_type %.loc16_16.2, %i32 [template = <error>]
// CHECK:STDOUT:     %k.var: ref <error> = var k
// CHECK:STDOUT:     %k: ref <error> = bind_name k, %k.var
// CHECK:STDOUT:     return
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @G(constants.%N.2) {
// CHECK:STDOUT:   %N.loc12_6.2 => constants.%N.2
// CHECK:STDOUT:   %N.patt.loc12_6.2 => constants.%N.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
