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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/function/generic/indirect_generic_type.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/function/generic/indirect_generic_type.carbon

fn F(T:! type, p: T**) -> T* {
  return *p;
}

// CHECK:STDOUT: --- indirect_generic_type.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %ptr.79f: type = ptr_type %T [symbolic]
// CHECK:STDOUT:   %ptr.a13: type = ptr_type %ptr.79f [symbolic]
// CHECK:STDOUT:   %pattern_type.12d: type = pattern_type %ptr.a13 [symbolic]
// CHECK:STDOUT:   %pattern_type.afe: type = pattern_type %ptr.79f [symbolic]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %require_complete.6e5: <witness> = require_complete_type %ptr.79f [symbolic]
// CHECK:STDOUT:   %require_complete.132: <witness> = require_complete_type %ptr.a13 [symbolic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {
// CHECK:STDOUT:     %T.patt: %pattern_type.98f = symbolic_binding_pattern T, 0 [concrete]
// CHECK:STDOUT:     %p.patt: @F.%pattern_type.loc15_16 (%pattern_type.12d) = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: @F.%pattern_type.loc15_16 (%pattern_type.12d) = value_param_pattern %p.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: @F.%pattern_type.loc15_24 (%pattern_type.afe) = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: @F.%pattern_type.loc15_24 (%pattern_type.afe) = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %T.ref.loc15_27: type = name_ref T, %T.loc15_6.2 [symbolic = %T.loc15_6.1 (constants.%T)]
// CHECK:STDOUT:     %ptr.loc15_28: type = ptr_type %T.ref.loc15_27 [symbolic = %ptr.loc15_20.1 (constants.%ptr.79f)]
// CHECK:STDOUT:     %T.loc15_6.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc15_6.1 (constants.%T)]
// CHECK:STDOUT:     %p.param: @F.%ptr.loc15_21.1 (%ptr.a13) = value_param call_param0
// CHECK:STDOUT:     %.loc15: type = splice_block %ptr.loc15_21.2 [symbolic = %ptr.loc15_21.1 (constants.%ptr.a13)] {
// CHECK:STDOUT:       %T.ref.loc15_19: type = name_ref T, %T.loc15_6.2 [symbolic = %T.loc15_6.1 (constants.%T)]
// CHECK:STDOUT:       %ptr.loc15_20.2: type = ptr_type %T.ref.loc15_19 [symbolic = %ptr.loc15_20.1 (constants.%ptr.79f)]
// CHECK:STDOUT:       %ptr.loc15_21.2: type = ptr_type %ptr.loc15_20.2 [symbolic = %ptr.loc15_21.1 (constants.%ptr.a13)]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: @F.%ptr.loc15_21.1 (%ptr.a13) = bind_name p, %p.param
// CHECK:STDOUT:     %return.param: ref @F.%ptr.loc15_20.1 (%ptr.79f) = out_param call_param1
// CHECK:STDOUT:     %return: ref @F.%ptr.loc15_20.1 (%ptr.79f) = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F(%T.loc15_6.2: type) {
// CHECK:STDOUT:   %T.loc15_6.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc15_6.1 (constants.%T)]
// CHECK:STDOUT:   %ptr.loc15_20.1: type = ptr_type %T.loc15_6.1 [symbolic = %ptr.loc15_20.1 (constants.%ptr.79f)]
// CHECK:STDOUT:   %ptr.loc15_21.1: type = ptr_type %ptr.loc15_20.1 [symbolic = %ptr.loc15_21.1 (constants.%ptr.a13)]
// CHECK:STDOUT:   %pattern_type.loc15_16: type = pattern_type %ptr.loc15_21.1 [symbolic = %pattern_type.loc15_16 (constants.%pattern_type.12d)]
// CHECK:STDOUT:   %pattern_type.loc15_24: type = pattern_type %ptr.loc15_20.1 [symbolic = %pattern_type.loc15_24 (constants.%pattern_type.afe)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %require_complete.loc15_24: <witness> = require_complete_type %ptr.loc15_20.1 [symbolic = %require_complete.loc15_24 (constants.%require_complete.6e5)]
// CHECK:STDOUT:   %require_complete.loc15_17: <witness> = require_complete_type %ptr.loc15_21.1 [symbolic = %require_complete.loc15_17 (constants.%require_complete.132)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%p.param: @F.%ptr.loc15_21.1 (%ptr.a13)) -> @F.%ptr.loc15_20.1 (%ptr.79f) {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     %p.ref: @F.%ptr.loc15_21.1 (%ptr.a13) = name_ref p, %p
// CHECK:STDOUT:     %.loc16_10.1: ref @F.%ptr.loc15_20.1 (%ptr.79f) = deref %p.ref
// CHECK:STDOUT:     %.loc16_10.2: @F.%ptr.loc15_20.1 (%ptr.79f) = bind_value %.loc16_10.1
// CHECK:STDOUT:     return %.loc16_10.2
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F(constants.%T) {
// CHECK:STDOUT:   %T.loc15_6.1 => constants.%T
// CHECK:STDOUT:   %ptr.loc15_20.1 => constants.%ptr.79f
// CHECK:STDOUT:   %ptr.loc15_21.1 => constants.%ptr.a13
// CHECK:STDOUT:   %pattern_type.loc15_16 => constants.%pattern_type.12d
// CHECK:STDOUT:   %pattern_type.loc15_24 => constants.%pattern_type.afe
// CHECK:STDOUT: }
// CHECK:STDOUT:
