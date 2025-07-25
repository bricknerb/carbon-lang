// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/primitives.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/where_expr/dot_self_index.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/where_expr/dot_self_index.carbon

interface Empty(W:! type) {
  let A:! type;
}

// T has index 0
// .Self has index invalid, not 1
// V has index 1, does not match .Self
//@dump-sem-ir-begin
fn H(T:! type, U: Empty(T) where .A = T*, V:! type) {}
//@dump-sem-ir-end

fn G(U: Empty(i32) where .A = i32*) {
  H(i32, U, bool);
}

// CHECK:STDOUT: --- dot_self_index.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %Empty.type.d5a: type = generic_interface_type @Empty [concrete]
// CHECK:STDOUT:   %Empty.generic: %Empty.type.d5a = struct_value () [concrete]
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %Empty.type.3e5fde.2: type = facet_type <@Empty, @Empty(%T)> [symbolic]
// CHECK:STDOUT:   %.Self.c95: %Empty.type.3e5fde.2 = bind_symbolic_name .Self [symbolic]
// CHECK:STDOUT:   %Empty.assoc_type.3cf698.2: type = assoc_entity_type @Empty, @Empty(%T) [symbolic]
// CHECK:STDOUT:   %assoc0.3715ce.2: %Empty.assoc_type.3cf698.2 = assoc_entity element0, @Empty.%A [symbolic]
// CHECK:STDOUT:   %require_complete.22f: <witness> = require_complete_type %Empty.type.3e5fde.2 [symbolic]
// CHECK:STDOUT:   %.Self.as_type.a75: type = facet_access_type %.Self.c95 [symbolic]
// CHECK:STDOUT:   %Empty.lookup_impl_witness.b1d: <witness> = lookup_impl_witness %.Self.c95, @Empty, @Empty(%T) [symbolic]
// CHECK:STDOUT:   %impl.elem0.c6e: type = impl_witness_access %Empty.lookup_impl_witness.b1d, element0 [symbolic]
// CHECK:STDOUT:   %ptr.79f: type = ptr_type %T [symbolic]
// CHECK:STDOUT:   %Empty_where.type.b73: type = facet_type <@Empty, @Empty(%T) where %impl.elem0.c6e = %ptr.79f> [symbolic]
// CHECK:STDOUT:   %pattern_type.668: type = pattern_type %Empty_where.type.b73 [symbolic]
// CHECK:STDOUT:   %V: type = bind_symbolic_name V, 1 [symbolic]
// CHECK:STDOUT:   %H.type: type = fn_type @H [concrete]
// CHECK:STDOUT:   %H: %H.type = struct_value () [concrete]
// CHECK:STDOUT:   %require_complete.af8: <witness> = require_complete_type %Empty_where.type.b73 [symbolic]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %Empty.type.f0b: type = facet_type <@Empty, @Empty(%i32)> [concrete]
// CHECK:STDOUT:   %.Self.e6e: %Empty.type.f0b = bind_symbolic_name .Self [symbolic_self]
// CHECK:STDOUT:   %Empty.assoc_type.7c7: type = assoc_entity_type @Empty, @Empty(%i32) [concrete]
// CHECK:STDOUT:   %assoc0.758: %Empty.assoc_type.7c7 = assoc_entity element0, @Empty.%A [concrete]
// CHECK:STDOUT:   %.Self.as_type.920: type = facet_access_type %.Self.e6e [symbolic_self]
// CHECK:STDOUT:   %Empty.lookup_impl_witness.4e3: <witness> = lookup_impl_witness %.Self.e6e, @Empty, @Empty(%i32) [symbolic_self]
// CHECK:STDOUT:   %impl.elem0.64a: type = impl_witness_access %Empty.lookup_impl_witness.4e3, element0 [symbolic_self]
// CHECK:STDOUT:   %ptr.235: type = ptr_type %i32 [concrete]
// CHECK:STDOUT:   %Empty_where.type.fc2: type = facet_type <@Empty, @Empty(%i32) where %impl.elem0.64a = %ptr.235> [concrete]
// CHECK:STDOUT:   %pattern_type.012: type = pattern_type %Empty_where.type.fc2 [concrete]
// CHECK:STDOUT:   %complete_type.091: <witness> = complete_type_witness %Empty.type.f0b [concrete]
// CHECK:STDOUT:   %complete_type.a3f: <witness> = complete_type_witness %Empty_where.type.fc2 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   %H.decl: %H.type = fn_decl @H [concrete = constants.%H] {
// CHECK:STDOUT:     %T.patt: %pattern_type.98f = symbolic_binding_pattern T, 0 [concrete]
// CHECK:STDOUT:     %U.patt: @H.%pattern_type (%pattern_type.668) = binding_pattern U [concrete]
// CHECK:STDOUT:     %U.param_patt: @H.%pattern_type (%pattern_type.668) = value_param_pattern %U.patt, call_param0 [concrete]
// CHECK:STDOUT:     %V.patt: %pattern_type.98f = symbolic_binding_pattern V, 1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %T.loc21_6.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc21_6.1 (constants.%T)]
// CHECK:STDOUT:     %U.param: @H.%Empty_where.type (%Empty_where.type.b73) = value_param call_param0
// CHECK:STDOUT:     %.loc21_28.1: type = splice_block %.loc21_28.2 [symbolic = %Empty_where.type (constants.%Empty_where.type.b73)] {
// CHECK:STDOUT:       %Empty.ref: %Empty.type.d5a = name_ref Empty, file.%Empty.decl [concrete = constants.%Empty.generic]
// CHECK:STDOUT:       %T.ref.loc21_25: type = name_ref T, %T.loc21_6.2 [symbolic = %T.loc21_6.1 (constants.%T)]
// CHECK:STDOUT:       %Empty.type.loc21_26.2: type = facet_type <@Empty, @Empty(constants.%T)> [symbolic = %Empty.type.loc21_26.1 (constants.%Empty.type.3e5fde.2)]
// CHECK:STDOUT:       <elided>
// CHECK:STDOUT:       %.Self.ref: @H.%Empty.type.loc21_26.1 (%Empty.type.3e5fde.2) = name_ref .Self, %.Self.2 [symbolic = %.Self.1 (constants.%.Self.c95)]
// CHECK:STDOUT:       %.loc21_34.1: @H.%Empty.assoc_type (%Empty.assoc_type.3cf698.2) = specific_constant @A.%assoc0, @Empty(constants.%T) [symbolic = %assoc0 (constants.%assoc0.3715ce.2)]
// CHECK:STDOUT:       %A.ref: @H.%Empty.assoc_type (%Empty.assoc_type.3cf698.2) = name_ref A, %.loc21_34.1 [symbolic = %assoc0 (constants.%assoc0.3715ce.2)]
// CHECK:STDOUT:       %.Self.as_type.loc21_34.2: type = facet_access_type %.Self.ref [symbolic = %.Self.as_type.loc21_34.1 (constants.%.Self.as_type.a75)]
// CHECK:STDOUT:       %.loc21_34.2: type = converted %.Self.ref, %.Self.as_type.loc21_34.2 [symbolic = %.Self.as_type.loc21_34.1 (constants.%.Self.as_type.a75)]
// CHECK:STDOUT:       %impl.elem0.loc21_34.2: type = impl_witness_access constants.%Empty.lookup_impl_witness.b1d, element0 [symbolic = %impl.elem0.loc21_34.1 (constants.%impl.elem0.c6e)]
// CHECK:STDOUT:       %T.ref.loc21_39: type = name_ref T, %T.loc21_6.2 [symbolic = %T.loc21_6.1 (constants.%T)]
// CHECK:STDOUT:       %ptr.loc21_40.2: type = ptr_type %T.ref.loc21_39 [symbolic = %ptr.loc21_40.1 (constants.%ptr.79f)]
// CHECK:STDOUT:       %.loc21_28.2: type = where_expr %.Self.2 [symbolic = %Empty_where.type (constants.%Empty_where.type.b73)] {
// CHECK:STDOUT:         requirement_rewrite %impl.elem0.loc21_34.2, %ptr.loc21_40.2
// CHECK:STDOUT:       }
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %U: @H.%Empty_where.type (%Empty_where.type.b73) = bind_name U, %U.param
// CHECK:STDOUT:     %V.loc21_43.2: type = bind_symbolic_name V, 1 [symbolic = %V.loc21_43.1 (constants.%V)]
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @H(%T.loc21_6.2: type, %V.loc21_43.2: type) {
// CHECK:STDOUT:   %T.loc21_6.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc21_6.1 (constants.%T)]
// CHECK:STDOUT:   %Empty.type.loc21_26.1: type = facet_type <@Empty, @Empty(%T.loc21_6.1)> [symbolic = %Empty.type.loc21_26.1 (constants.%Empty.type.3e5fde.2)]
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %require_complete.loc21_34: <witness> = require_complete_type %Empty.type.loc21_26.1 [symbolic = %require_complete.loc21_34 (constants.%require_complete.22f)]
// CHECK:STDOUT:   %Empty.assoc_type: type = assoc_entity_type @Empty, @Empty(%T.loc21_6.1) [symbolic = %Empty.assoc_type (constants.%Empty.assoc_type.3cf698.2)]
// CHECK:STDOUT:   %assoc0: @H.%Empty.assoc_type (%Empty.assoc_type.3cf698.2) = assoc_entity element0, @Empty.%A [symbolic = %assoc0 (constants.%assoc0.3715ce.2)]
// CHECK:STDOUT:   %.Self.as_type.loc21_34.1: type = facet_access_type %.Self.1 [symbolic = %.Self.as_type.loc21_34.1 (constants.%.Self.as_type.a75)]
// CHECK:STDOUT:   %Empty.lookup_impl_witness: <witness> = lookup_impl_witness %.Self.1, @Empty, @Empty(%T.loc21_6.1) [symbolic = %Empty.lookup_impl_witness (constants.%Empty.lookup_impl_witness.b1d)]
// CHECK:STDOUT:   %impl.elem0.loc21_34.1: type = impl_witness_access %Empty.lookup_impl_witness, element0 [symbolic = %impl.elem0.loc21_34.1 (constants.%impl.elem0.c6e)]
// CHECK:STDOUT:   %ptr.loc21_40.1: type = ptr_type %T.loc21_6.1 [symbolic = %ptr.loc21_40.1 (constants.%ptr.79f)]
// CHECK:STDOUT:   %Empty_where.type: type = facet_type <@Empty, @Empty(%T.loc21_6.1) where %impl.elem0.loc21_34.1 = %ptr.loc21_40.1> [symbolic = %Empty_where.type (constants.%Empty_where.type.b73)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %Empty_where.type [symbolic = %pattern_type (constants.%pattern_type.668)]
// CHECK:STDOUT:   %V.loc21_43.1: type = bind_symbolic_name V, 1 [symbolic = %V.loc21_43.1 (constants.%V)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %require_complete.loc21_17: <witness> = require_complete_type %Empty_where.type [symbolic = %require_complete.loc21_17 (constants.%require_complete.af8)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%U.param: @H.%Empty_where.type (%Empty_where.type.b73)) {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     return
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @H(constants.%T, constants.%V) {
// CHECK:STDOUT:   %T.loc21_6.1 => constants.%T
// CHECK:STDOUT:   %Empty.type.loc21_26.1 => constants.%Empty.type.3e5fde.2
// CHECK:STDOUT:   %.Self.1 => constants.%.Self.c95
// CHECK:STDOUT:   %require_complete.loc21_34 => constants.%require_complete.22f
// CHECK:STDOUT:   %Empty.assoc_type => constants.%Empty.assoc_type.3cf698.2
// CHECK:STDOUT:   %assoc0 => constants.%assoc0.3715ce.2
// CHECK:STDOUT:   %.Self.as_type.loc21_34.1 => constants.%.Self.as_type.a75
// CHECK:STDOUT:   %Empty.lookup_impl_witness => constants.%Empty.lookup_impl_witness.b1d
// CHECK:STDOUT:   %impl.elem0.loc21_34.1 => constants.%impl.elem0.c6e
// CHECK:STDOUT:   %ptr.loc21_40.1 => constants.%ptr.79f
// CHECK:STDOUT:   %Empty_where.type => constants.%Empty_where.type.b73
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.668
// CHECK:STDOUT:   %V.loc21_43.1 => constants.%V
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @H(constants.%i32, bool) {
// CHECK:STDOUT:   %T.loc21_6.1 => constants.%i32
// CHECK:STDOUT:   %Empty.type.loc21_26.1 => constants.%Empty.type.f0b
// CHECK:STDOUT:   %.Self.1 => constants.%.Self.e6e
// CHECK:STDOUT:   %require_complete.loc21_34 => constants.%complete_type.091
// CHECK:STDOUT:   %Empty.assoc_type => constants.%Empty.assoc_type.7c7
// CHECK:STDOUT:   %assoc0 => constants.%assoc0.758
// CHECK:STDOUT:   %.Self.as_type.loc21_34.1 => constants.%.Self.as_type.920
// CHECK:STDOUT:   %Empty.lookup_impl_witness => constants.%Empty.lookup_impl_witness.4e3
// CHECK:STDOUT:   %impl.elem0.loc21_34.1 => constants.%impl.elem0.64a
// CHECK:STDOUT:   %ptr.loc21_40.1 => constants.%ptr.235
// CHECK:STDOUT:   %Empty_where.type => constants.%Empty_where.type.fc2
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.012
// CHECK:STDOUT:   %V.loc21_43.1 => bool
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %require_complete.loc21_17 => constants.%complete_type.a3f
// CHECK:STDOUT: }
// CHECK:STDOUT:
