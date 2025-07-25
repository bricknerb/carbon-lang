// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/primitives.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/return/fail_returned_var_type.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/return/fail_returned_var_type.carbon

fn Mismatch() -> i32 {
  // CHECK:STDERR: fail_returned_var_type.carbon:[[@LINE+7]]:19: error: type `f64` of `returned var` does not match return type of enclosing function [ReturnedVarWrongType]
  // CHECK:STDERR:   returned var v: f64 = 0.0;
  // CHECK:STDERR:                   ^~~
  // CHECK:STDERR: fail_returned_var_type.carbon:[[@LINE-4]]:15: note: return type of function is `i32` [ReturnTypeHereNote]
  // CHECK:STDERR: fn Mismatch() -> i32 {
  // CHECK:STDERR:               ^~~~~~
  // CHECK:STDERR:
  returned var v: f64 = 0.0;
  return var;
}

// CHECK:STDOUT: --- fail_returned_var_type.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.7ce: type = pattern_type %i32 [concrete]
// CHECK:STDOUT:   %Mismatch.type: type = fn_type @Mismatch [concrete]
// CHECK:STDOUT:   %Mismatch: %Mismatch.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_64: Core.IntLiteral = int_value 64 [concrete]
// CHECK:STDOUT:   %Float.type: type = fn_type @Float [concrete]
// CHECK:STDOUT:   %Float: %Float.type = struct_value () [concrete]
// CHECK:STDOUT:   %pattern_type.3de: type = pattern_type f64 [concrete]
// CHECK:STDOUT:   %float: f64 = float_literal 0 [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.3ef: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(f64) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.868: %T.as.Destroy.impl.Op.type.3ef = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.ef1: type = ptr_type f64 [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn: <specific function> = specific_function %T.as.Destroy.impl.Op.868, @T.as.Destroy.impl.Op(f64) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     .Float = %Core.Float
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/parts/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT:   %Core.Float: %Float.type = import_ref Core//prelude/parts/float, Float, loaded [concrete = constants.%Float]
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Mismatch = %Mismatch.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Mismatch.decl: %Mismatch.type = fn_decl @Mismatch [concrete = constants.%Mismatch] {
// CHECK:STDOUT:     %return.patt: %pattern_type.7ce = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.7ce = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %return.param: ref %i32 = out_param call_param0
// CHECK:STDOUT:     %return: ref %i32 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Mismatch() -> %i32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: %pattern_type.3de = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: %pattern_type.3de = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref f64 = var %v.var_patt
// CHECK:STDOUT:   %float: f64 = float_literal 0 [concrete = constants.%float]
// CHECK:STDOUT:   assign %v.var, %float
// CHECK:STDOUT:   %.loc23_19.1: type = splice_block %.loc23_19.3 [concrete = f64] {
// CHECK:STDOUT:     %int_64: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:     %Float.call: init type = call constants.%Float(%int_64) [concrete = f64]
// CHECK:STDOUT:     %.loc23_19.2: type = value_of_initializer %Float.call [concrete = f64]
// CHECK:STDOUT:     %.loc23_19.3: type = converted %Float.call, %.loc23_19.2 [concrete = f64]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: ref f64 = bind_name v, %v.var
// CHECK:STDOUT:   %.loc23_16: f64 = bind_value %v
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound: <bound method> = bound_method %v.var, constants.%T.as.Destroy.impl.Op.868
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.868, @T.as.Destroy.impl.Op(f64) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %v.var, %T.as.Destroy.impl.Op.specific_fn
// CHECK:STDOUT:   %addr: %ptr.ef1 = addr_of %v.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call: init %empty_tuple.type = call %bound_method(%addr)
// CHECK:STDOUT:   return %.loc23_16
// CHECK:STDOUT: }
// CHECK:STDOUT:
