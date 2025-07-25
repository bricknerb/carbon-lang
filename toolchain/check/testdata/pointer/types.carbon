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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/pointer/types.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/pointer/types.carbon

fn Ptr(p: i32*) -> i32* {
  return p;
}

fn ConstPtr(p: const i32*) -> (const i32)* {
  return p;
}

// CHECK:STDOUT: --- types.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %ptr.235: type = ptr_type %i32 [concrete]
// CHECK:STDOUT:   %pattern_type.fe8: type = pattern_type %ptr.235 [concrete]
// CHECK:STDOUT:   %Ptr.type: type = fn_type @Ptr [concrete]
// CHECK:STDOUT:   %Ptr: %Ptr.type = struct_value () [concrete]
// CHECK:STDOUT:   %const: type = const_type %i32 [concrete]
// CHECK:STDOUT:   %ptr.36b: type = ptr_type %const [concrete]
// CHECK:STDOUT:   %pattern_type.bff: type = pattern_type %ptr.36b [concrete]
// CHECK:STDOUT:   %ConstPtr.type: type = fn_type @ConstPtr [concrete]
// CHECK:STDOUT:   %ConstPtr: %ConstPtr.type = struct_value () [concrete]
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
// CHECK:STDOUT:     .Ptr = %Ptr.decl
// CHECK:STDOUT:     .ConstPtr = %ConstPtr.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Ptr.decl: %Ptr.type = fn_decl @Ptr [concrete = constants.%Ptr] {
// CHECK:STDOUT:     %p.patt: %pattern_type.fe8 = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: %pattern_type.fe8 = value_param_pattern %p.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.fe8 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.fe8 = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc15_20: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc15_20: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %ptr.loc15_23: type = ptr_type %i32.loc15_20 [concrete = constants.%ptr.235]
// CHECK:STDOUT:     %p.param: %ptr.235 = value_param call_param0
// CHECK:STDOUT:     %.loc15: type = splice_block %ptr.loc15_14 [concrete = constants.%ptr.235] {
// CHECK:STDOUT:       %int_32.loc15_11: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %i32.loc15_11: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:       %ptr.loc15_14: type = ptr_type %i32.loc15_11 [concrete = constants.%ptr.235]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: %ptr.235 = bind_name p, %p.param
// CHECK:STDOUT:     %return.param: ref %ptr.235 = out_param call_param1
// CHECK:STDOUT:     %return: ref %ptr.235 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %ConstPtr.decl: %ConstPtr.type = fn_decl @ConstPtr [concrete = constants.%ConstPtr] {
// CHECK:STDOUT:     %p.patt: %pattern_type.bff = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: %pattern_type.bff = value_param_pattern %p.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.bff = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.bff = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc19_38: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc19_38: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %const.loc19_32: type = const_type %i32.loc19_38 [concrete = constants.%const]
// CHECK:STDOUT:     %ptr.loc19_42: type = ptr_type %const.loc19_32 [concrete = constants.%ptr.36b]
// CHECK:STDOUT:     %p.param: %ptr.36b = value_param call_param0
// CHECK:STDOUT:     %.loc19: type = splice_block %ptr.loc19_25 [concrete = constants.%ptr.36b] {
// CHECK:STDOUT:       %int_32.loc19_22: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %i32.loc19_22: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:       %const.loc19_16: type = const_type %i32.loc19_22 [concrete = constants.%const]
// CHECK:STDOUT:       %ptr.loc19_25: type = ptr_type %const.loc19_16 [concrete = constants.%ptr.36b]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: %ptr.36b = bind_name p, %p.param
// CHECK:STDOUT:     %return.param: ref %ptr.36b = out_param call_param1
// CHECK:STDOUT:     %return: ref %ptr.36b = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Ptr(%p.param: %ptr.235) -> %ptr.235 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %p.ref: %ptr.235 = name_ref p, %p
// CHECK:STDOUT:   return %p.ref
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @ConstPtr(%p.param: %ptr.36b) -> %ptr.36b {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %p.ref: %ptr.36b = name_ref p, %p
// CHECK:STDOUT:   return %p.ref
// CHECK:STDOUT: }
// CHECK:STDOUT:
