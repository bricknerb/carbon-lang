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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/impl/impl_forall.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/impl/impl_forall.carbon

interface Simple {
  fn F();
}

impl forall [T:! type] T as Simple {
  fn F() {}
}

// CHECK:STDOUT: --- impl_forall.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Simple.type: type = facet_type <@Simple> [concrete]
// CHECK:STDOUT:   %Self: %Simple.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %F.type.e2e: type = fn_type @F.loc16 [concrete]
// CHECK:STDOUT:   %F.df8: %F.type.e2e = struct_value () [concrete]
// CHECK:STDOUT:   %Simple.assoc_type: type = assoc_entity_type @Simple [concrete]
// CHECK:STDOUT:   %assoc0: %Simple.assoc_type = assoc_entity element0, @Simple.%F.decl [concrete]
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type: type = pattern_type type [concrete]
// CHECK:STDOUT:   %Simple.impl_witness: <witness> = impl_witness file.%Simple.impl_witness_table, @Simple.impl(%T) [symbolic]
// CHECK:STDOUT:   %F.type.1e1: type = fn_type @F.loc20, @Simple.impl(%T) [symbolic]
// CHECK:STDOUT:   %F.4c3: %F.type.1e1 = struct_value () [symbolic]
// CHECK:STDOUT:   %Simple.facet: %Simple.type = facet_value %T, (%Simple.impl_witness) [symbolic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Simple = %Simple.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Simple.decl: type = interface_decl @Simple [concrete = constants.%Simple.type] {} {}
// CHECK:STDOUT:   impl_decl @Simple.impl [concrete] {
// CHECK:STDOUT:     %T.patt: %pattern_type = symbolic_binding_pattern T, 0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %T.ref: type = name_ref T, %T.loc19_14.1 [symbolic = %T.loc19_14.2 (constants.%T)]
// CHECK:STDOUT:     %Simple.ref: type = name_ref Simple, file.%Simple.decl [concrete = constants.%Simple.type]
// CHECK:STDOUT:     %T.loc19_14.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc19_14.2 (constants.%T)]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Simple.impl_witness_table = impl_witness_table (@Simple.impl.%F.decl), @Simple.impl [concrete]
// CHECK:STDOUT:   %Simple.impl_witness: <witness> = impl_witness %Simple.impl_witness_table, @Simple.impl(constants.%T) [symbolic = @Simple.impl.%Simple.impl_witness (constants.%Simple.impl_witness)]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Simple {
// CHECK:STDOUT:   %Self: %Simple.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self]
// CHECK:STDOUT:   %F.decl: %F.type.e2e = fn_decl @F.loc16 [concrete = constants.%F.df8] {} {}
// CHECK:STDOUT:   %assoc0: %Simple.assoc_type = assoc_entity element0, %F.decl [concrete = constants.%assoc0]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   .F = %assoc0
// CHECK:STDOUT:   witness = (%F.decl)
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic impl @Simple.impl(%T.loc19_14.1: type) {
// CHECK:STDOUT:   %T.loc19_14.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc19_14.2 (constants.%T)]
// CHECK:STDOUT:   %Simple.impl_witness: <witness> = impl_witness file.%Simple.impl_witness_table, @Simple.impl(%T.loc19_14.2) [symbolic = %Simple.impl_witness (constants.%Simple.impl_witness)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %F.type: type = fn_type @F.loc20, @Simple.impl(%T.loc19_14.2) [symbolic = %F.type (constants.%F.type.1e1)]
// CHECK:STDOUT:   %F: @Simple.impl.%F.type (%F.type.1e1) = struct_value () [symbolic = %F (constants.%F.4c3)]
// CHECK:STDOUT:
// CHECK:STDOUT:   impl: %T.ref as %Simple.ref {
// CHECK:STDOUT:     %F.decl: @Simple.impl.%F.type (%F.type.1e1) = fn_decl @F.loc20 [symbolic = @Simple.impl.%F (constants.%F.4c3)] {} {}
// CHECK:STDOUT:
// CHECK:STDOUT:   !members:
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:     witness = file.%Simple.impl_witness
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F.loc16(@Simple.%Self: %Simple.type) {
// CHECK:STDOUT:   fn();
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F.loc20(@Simple.impl.%T.loc19_14.1: type) {
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:
// CHECK:STDOUT:   fn() {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     return
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.loc16(constants.%Self) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Simple.impl(constants.%T) {
// CHECK:STDOUT:   %T.loc19_14.2 => constants.%T
// CHECK:STDOUT:   %Simple.impl_witness => constants.%Simple.impl_witness
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %F.type => constants.%F.type.1e1
// CHECK:STDOUT:   %F => constants.%F.4c3
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.loc20(constants.%T) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.loc16(constants.%Simple.facet) {}
// CHECK:STDOUT:
