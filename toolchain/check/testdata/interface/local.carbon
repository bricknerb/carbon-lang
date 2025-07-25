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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interface/local.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interface/local.carbon

fn F() {
  interface I {
    fn G[self: Self]();
  }
  impl () as I {
    fn G[self: Self]() {}
  }
  ().(I.G)();
}

// CHECK:STDOUT: --- local.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %I.type: type = facet_type <@I> [concrete]
// CHECK:STDOUT:   %Self: %I.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %Self.as_type: type = facet_access_type %Self [symbolic]
// CHECK:STDOUT:   %pattern_type.744: type = pattern_type %Self.as_type [symbolic]
// CHECK:STDOUT:   %I.G.type: type = fn_type @I.G [concrete]
// CHECK:STDOUT:   %I.G: %I.G.type = struct_value () [concrete]
// CHECK:STDOUT:   %I.assoc_type: type = assoc_entity_type @I [concrete]
// CHECK:STDOUT:   %assoc0: %I.assoc_type = assoc_entity element0, @I.%I.G.decl [concrete]
// CHECK:STDOUT:   %I.impl_witness: <witness> = impl_witness @F.%I.impl_witness_table [concrete]
// CHECK:STDOUT:   %pattern_type.cb1: type = pattern_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %empty_tuple.type.as.I.impl.G.type: type = fn_type @empty_tuple.type.as.I.impl.G [concrete]
// CHECK:STDOUT:   %empty_tuple.type.as.I.impl.G: %empty_tuple.type.as.I.impl.G.type = struct_value () [concrete]
// CHECK:STDOUT:   %I.facet: %I.type = facet_value %empty_tuple.type, (%I.impl_witness) [concrete]
// CHECK:STDOUT:   %.ba5: type = fn_type_with_self_type %I.G.type, %I.facet [concrete]
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @I {
// CHECK:STDOUT:   %Self: %I.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self]
// CHECK:STDOUT:   %I.G.decl: %I.G.type = fn_decl @I.G [concrete = constants.%I.G] {
// CHECK:STDOUT:     %self.patt: @I.G.%pattern_type (%pattern_type.744) = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: @I.G.%pattern_type (%pattern_type.744) = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: @I.G.%Self.as_type.loc17_16.1 (%Self.as_type) = value_param call_param0
// CHECK:STDOUT:     %.loc17_16.1: type = splice_block %.loc17_16.2 [symbolic = %Self.as_type.loc17_16.1 (constants.%Self.as_type)] {
// CHECK:STDOUT:       %Self.ref: %I.type = name_ref Self, @I.%Self [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:       %Self.as_type.loc17_16.2: type = facet_access_type %Self.ref [symbolic = %Self.as_type.loc17_16.1 (constants.%Self.as_type)]
// CHECK:STDOUT:       %.loc17_16.2: type = converted %Self.ref, %Self.as_type.loc17_16.2 [symbolic = %Self.as_type.loc17_16.1 (constants.%Self.as_type)]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %self: @I.G.%Self.as_type.loc17_16.1 (%Self.as_type) = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %assoc0: %I.assoc_type = assoc_entity element0, %I.G.decl [concrete = constants.%assoc0]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   .G = %assoc0
// CHECK:STDOUT:   witness = (%I.G.decl)
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: impl @empty_tuple.type.as.I.impl: %.loc19_9.2 as %I.ref {
// CHECK:STDOUT:   %empty_tuple.type.as.I.impl.G.decl: %empty_tuple.type.as.I.impl.G.type = fn_decl @empty_tuple.type.as.I.impl.G [concrete = constants.%empty_tuple.type.as.I.impl.G] {
// CHECK:STDOUT:     %self.patt: %pattern_type.cb1 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.cb1 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %empty_tuple.type = value_param call_param0
// CHECK:STDOUT:     %Self.ref: type = name_ref Self, @empty_tuple.type.as.I.impl.%.loc19_9.2 [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %self: %empty_tuple.type = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .G = %empty_tuple.type.as.I.impl.G.decl
// CHECK:STDOUT:   witness = @F.%I.impl_witness
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %I.decl: type = interface_decl @I [concrete = constants.%I.type] {} {}
// CHECK:STDOUT:   impl_decl @empty_tuple.type.as.I.impl [concrete] {} {
// CHECK:STDOUT:     %.loc19_9.1: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc19_9.2: type = converted %.loc19_9.1, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %I.ref: type = name_ref I, @F.%I.decl [concrete = constants.%I.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %I.impl_witness_table = impl_witness_table (@empty_tuple.type.as.I.impl.%empty_tuple.type.as.I.impl.G.decl), @empty_tuple.type.as.I.impl [concrete]
// CHECK:STDOUT:   %I.impl_witness: <witness> = impl_witness %I.impl_witness_table [concrete = constants.%I.impl_witness]
// CHECK:STDOUT:   %.loc22_4.1: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:   %I.ref: type = name_ref I, %I.decl [concrete = constants.%I.type]
// CHECK:STDOUT:   %G.ref: %I.assoc_type = name_ref G, @I.%assoc0 [concrete = constants.%assoc0]
// CHECK:STDOUT:   %impl.elem0: %.ba5 = impl_witness_access constants.%I.impl_witness, element0 [concrete = constants.%empty_tuple.type.as.I.impl.G]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %.loc22_4.1, %impl.elem0
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %.loc22_4.2: %empty_tuple.type = converted %.loc22_4.1, %empty_tuple [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %empty_tuple.type.as.I.impl.G.call: init %empty_tuple.type = call %bound_method(%.loc22_4.2)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @I.G(@I.%Self: %I.type) {
// CHECK:STDOUT:   %Self: %I.type = bind_symbolic_name Self, 0 [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:   %Self.as_type.loc17_16.1: type = facet_access_type %Self [symbolic = %Self.as_type.loc17_16.1 (constants.%Self.as_type)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %Self.as_type.loc17_16.1 [symbolic = %pattern_type (constants.%pattern_type.744)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%self.param: @I.G.%Self.as_type.loc17_16.1 (%Self.as_type));
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @empty_tuple.type.as.I.impl.G(%self.param: %empty_tuple.type) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @I.G(constants.%Self) {
// CHECK:STDOUT:   %Self => constants.%Self
// CHECK:STDOUT:   %Self.as_type.loc17_16.1 => constants.%Self.as_type
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.744
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @I.G(constants.%I.facet) {
// CHECK:STDOUT:   %Self => constants.%I.facet
// CHECK:STDOUT:   %Self.as_type.loc17_16.1 => constants.%empty_tuple.type
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.cb1
// CHECK:STDOUT: }
// CHECK:STDOUT:
