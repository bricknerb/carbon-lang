// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/destroy.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/impl/import_self.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/impl/import_self.carbon

// --- a.carbon

library "[[@TEST_NAME]]";

interface Add {
  fn Op[self: Self](other: Self) -> Self;
}

// --- b.carbon

library "[[@TEST_NAME]]";

import library "a";

impl () as Add {
  fn Op[self: Self](other: Self) -> Self { return (); }
}

fn F(x: (), y: ()) -> () {
  return x.(Add.Op)(y);
}

// CHECK:STDOUT: --- a.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Add.type: type = facet_type <@Add> [concrete]
// CHECK:STDOUT:   %Self: %Add.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %Self.as_type: type = facet_access_type %Self [symbolic]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %Self.as_type [symbolic]
// CHECK:STDOUT:   %Op.type: type = fn_type @Op [concrete]
// CHECK:STDOUT:   %Op: %Op.type = struct_value () [concrete]
// CHECK:STDOUT:   %Add.assoc_type: type = assoc_entity_type @Add [concrete]
// CHECK:STDOUT:   %assoc0: %Add.assoc_type = assoc_entity element0, @Add.%Op.decl [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Add = %Add.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Add.decl: type = interface_decl @Add [concrete = constants.%Add.type] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Add {
// CHECK:STDOUT:   %Self: %Add.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self]
// CHECK:STDOUT:   %Op.decl: %Op.type = fn_decl @Op [concrete = constants.%Op] {
// CHECK:STDOUT:     %self.patt: @Op.%pattern_type (%pattern_type) = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: @Op.%pattern_type (%pattern_type) = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %other.patt: @Op.%pattern_type (%pattern_type) = binding_pattern other [concrete]
// CHECK:STDOUT:     %other.param_patt: @Op.%pattern_type (%pattern_type) = value_param_pattern %other.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: @Op.%pattern_type (%pattern_type) = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: @Op.%pattern_type (%pattern_type) = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Self.ref.loc5_37: %Add.type = name_ref Self, @Add.%Self [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:     %Self.as_type.loc5_37: type = facet_access_type %Self.ref.loc5_37 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:     %.loc5_37: type = converted %Self.ref.loc5_37, %Self.as_type.loc5_37 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:     %self.param: @Op.%Self.as_type.loc5_15.1 (%Self.as_type) = value_param call_param0
// CHECK:STDOUT:     %.loc5_15.1: type = splice_block %.loc5_15.2 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)] {
// CHECK:STDOUT:       %Self.ref.loc5_15: %Add.type = name_ref Self, @Add.%Self [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:       %Self.as_type.loc5_15.2: type = facet_access_type %Self.ref.loc5_15 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:       %.loc5_15.2: type = converted %Self.ref.loc5_15, %Self.as_type.loc5_15.2 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %self: @Op.%Self.as_type.loc5_15.1 (%Self.as_type) = bind_name self, %self.param
// CHECK:STDOUT:     %other.param: @Op.%Self.as_type.loc5_15.1 (%Self.as_type) = value_param call_param1
// CHECK:STDOUT:     %.loc5_28.1: type = splice_block %.loc5_28.2 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)] {
// CHECK:STDOUT:       %Self.ref.loc5_28: %Add.type = name_ref Self, @Add.%Self [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:       %Self.as_type.loc5_28: type = facet_access_type %Self.ref.loc5_28 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:       %.loc5_28.2: type = converted %Self.ref.loc5_28, %Self.as_type.loc5_28 [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %other: @Op.%Self.as_type.loc5_15.1 (%Self.as_type) = bind_name other, %other.param
// CHECK:STDOUT:     %return.param: ref @Op.%Self.as_type.loc5_15.1 (%Self.as_type) = out_param call_param2
// CHECK:STDOUT:     %return: ref @Op.%Self.as_type.loc5_15.1 (%Self.as_type) = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %assoc0: %Add.assoc_type = assoc_entity element0, %Op.decl [concrete = constants.%assoc0]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   .Op = %assoc0
// CHECK:STDOUT:   witness = (%Op.decl)
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Op(@Add.%Self: %Add.type) {
// CHECK:STDOUT:   %Self: %Add.type = bind_symbolic_name Self, 0 [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:   %Self.as_type.loc5_15.1: type = facet_access_type %Self [symbolic = %Self.as_type.loc5_15.1 (constants.%Self.as_type)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %Self.as_type.loc5_15.1 [symbolic = %pattern_type (constants.%pattern_type)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%self.param: @Op.%Self.as_type.loc5_15.1 (%Self.as_type), %other.param: @Op.%Self.as_type.loc5_15.1 (%Self.as_type)) -> @Op.%Self.as_type.loc5_15.1 (%Self.as_type);
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Op(constants.%Self) {
// CHECK:STDOUT:   %Self => constants.%Self
// CHECK:STDOUT:   %Self.as_type.loc5_15.1 => constants.%Self.as_type
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- b.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %Add.type: type = facet_type <@Add> [concrete]
// CHECK:STDOUT:   %Self.b3d: %Add.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %Op.type.31b: type = fn_type @Op.1 [concrete]
// CHECK:STDOUT:   %Op.d59: %Op.type.31b = struct_value () [concrete]
// CHECK:STDOUT:   %Self.as_type.f73: type = facet_access_type %Self.b3d [symbolic]
// CHECK:STDOUT:   %pattern_type.8f9: type = pattern_type %Self.as_type.f73 [symbolic]
// CHECK:STDOUT:   %Add.impl_witness: <witness> = impl_witness file.%Add.impl_witness_table [concrete]
// CHECK:STDOUT:   %pattern_type.cb1: type = pattern_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %Op.type.adf: type = fn_type @Op.loc7 [concrete]
// CHECK:STDOUT:   %Op.489: %Op.type.adf = struct_value () [concrete]
// CHECK:STDOUT:   %Add.facet: %Add.type = facet_value %empty_tuple.type, (%Add.impl_witness) [concrete]
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %Add.assoc_type: type = assoc_entity_type @Add [concrete]
// CHECK:STDOUT:   %assoc0.5ca: %Add.assoc_type = assoc_entity element0, imports.%Main.import_ref.5a3 [concrete]
// CHECK:STDOUT:   %.59e: type = fn_type_with_self_type %Op.type.31b, %Add.facet [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %Op.type.a63: type = fn_type @Op.3, @Destroy.impl(%empty_tuple.type) [concrete]
// CHECK:STDOUT:   %Op.ea3: %Op.type.a63 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.843: type = ptr_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %Op.specific_fn: <specific function> = specific_function %Op.ea3, @Op.3(%empty_tuple.type) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.Add: type = import_ref Main//a, Add, loaded [concrete = constants.%Add.type]
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Main.import_ref.07c = import_ref Main//a, inst19 [no loc], unloaded
// CHECK:STDOUT:   %Main.import_ref.f99: %Add.assoc_type = import_ref Main//a, loc5_41, loaded [concrete = constants.%assoc0.5ca]
// CHECK:STDOUT:   %Main.Op: %Op.type.31b = import_ref Main//a, Op, loaded [concrete = constants.%Op.d59]
// CHECK:STDOUT:   %Main.import_ref.e5e: %Add.type = import_ref Main//a, inst19 [no loc], loaded [symbolic = constants.%Self.b3d]
// CHECK:STDOUT:   %Main.import_ref.5a3: %Op.type.31b = import_ref Main//a, loc5_41, loaded [concrete = constants.%Op.d59]
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Add = imports.%Main.Add
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   impl_decl @Add.impl [concrete] {} {
// CHECK:STDOUT:     %.loc6_7.1: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc6_7.2: type = converted %.loc6_7.1, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %Add.ref: type = name_ref Add, imports.%Main.Add [concrete = constants.%Add.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Add.impl_witness_table = impl_witness_table (@Add.impl.%Op.decl), @Add.impl [concrete]
// CHECK:STDOUT:   %Add.impl_witness: <witness> = impl_witness %Add.impl_witness_table [concrete = constants.%Add.impl_witness]
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {
// CHECK:STDOUT:     %x.patt: %pattern_type.cb1 = binding_pattern x [concrete]
// CHECK:STDOUT:     %x.param_patt: %pattern_type.cb1 = value_param_pattern %x.patt, call_param0 [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.cb1 = binding_pattern y [concrete]
// CHECK:STDOUT:     %y.param_patt: %pattern_type.cb1 = value_param_pattern %y.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.cb1 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.cb1 = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %.loc10_24.1: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc10_24.2: type = converted %.loc10_24.1, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %x.param: %empty_tuple.type = value_param call_param0
// CHECK:STDOUT:     %.loc10_10.1: type = splice_block %.loc10_10.3 [concrete = constants.%empty_tuple.type] {
// CHECK:STDOUT:       %.loc10_10.2: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:       %.loc10_10.3: type = converted %.loc10_10.2, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %x: %empty_tuple.type = bind_name x, %x.param
// CHECK:STDOUT:     %y.param: %empty_tuple.type = value_param call_param1
// CHECK:STDOUT:     %.loc10_17.1: type = splice_block %.loc10_17.3 [concrete = constants.%empty_tuple.type] {
// CHECK:STDOUT:       %.loc10_17.2: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:       %.loc10_17.3: type = converted %.loc10_17.2, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %y: %empty_tuple.type = bind_name y, %y.param
// CHECK:STDOUT:     %return.param: ref %empty_tuple.type = out_param call_param2
// CHECK:STDOUT:     %return: ref %empty_tuple.type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Add [from "a.carbon"] {
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.07c
// CHECK:STDOUT:   .Op = imports.%Main.import_ref.f99
// CHECK:STDOUT:   witness = (imports.%Main.Op)
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: impl @Add.impl: %.loc6_7.2 as %Add.ref {
// CHECK:STDOUT:   %Op.decl: %Op.type.adf = fn_decl @Op.loc7 [concrete = constants.%Op.489] {
// CHECK:STDOUT:     %self.patt: %pattern_type.cb1 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.cb1 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %other.patt: %pattern_type.cb1 = binding_pattern other [concrete]
// CHECK:STDOUT:     %other.param_patt: %pattern_type.cb1 = value_param_pattern %other.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.cb1 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.cb1 = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Self.ref.loc7_37: type = name_ref Self, @Add.impl.%.loc6_7.2 [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %self.param: %empty_tuple.type = value_param call_param0
// CHECK:STDOUT:     %Self.ref.loc7_15: type = name_ref Self, @Add.impl.%.loc6_7.2 [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %self: %empty_tuple.type = bind_name self, %self.param
// CHECK:STDOUT:     %other.param: %empty_tuple.type = value_param call_param1
// CHECK:STDOUT:     %Self.ref.loc7_28: type = name_ref Self, @Add.impl.%.loc6_7.2 [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %other: %empty_tuple.type = bind_name other, %other.param
// CHECK:STDOUT:     %return.param: ref %empty_tuple.type = out_param call_param2
// CHECK:STDOUT:     %return: ref %empty_tuple.type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Op = %Op.decl
// CHECK:STDOUT:   witness = file.%Add.impl_witness
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Op.1(imports.%Main.import_ref.e5e: %Add.type) [from "a.carbon"] {
// CHECK:STDOUT:   %Self: %Add.type = bind_symbolic_name Self, 0 [symbolic = %Self (constants.%Self.b3d)]
// CHECK:STDOUT:   %Self.as_type: type = facet_access_type %Self [symbolic = %Self.as_type (constants.%Self.as_type.f73)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %Self.as_type [symbolic = %pattern_type (constants.%pattern_type.8f9)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn;
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Op.loc7(%self.param: %empty_tuple.type, %other.param: %empty_tuple.type) -> %empty_tuple.type {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %.loc7_52: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %.loc7_53: %empty_tuple.type = converted %.loc7_52, %empty_tuple [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   return %.loc7_53
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F(%x.param: %empty_tuple.type, %y.param: %empty_tuple.type) -> %empty_tuple.type {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %x.ref: %empty_tuple.type = name_ref x, %x
// CHECK:STDOUT:   %Add.ref: type = name_ref Add, imports.%Main.Add [concrete = constants.%Add.type]
// CHECK:STDOUT:   %Op.ref: %Add.assoc_type = name_ref Op, imports.%Main.import_ref.f99 [concrete = constants.%assoc0.5ca]
// CHECK:STDOUT:   %impl.elem0: %.59e = impl_witness_access constants.%Add.impl_witness, element0 [concrete = constants.%Op.489]
// CHECK:STDOUT:   %bound_method.loc11_11: <bound method> = bound_method %x.ref, %impl.elem0
// CHECK:STDOUT:   %y.ref: %empty_tuple.type = name_ref y, %y
// CHECK:STDOUT:   %Op.call: init %empty_tuple.type = call %bound_method.loc11_11(%x.ref, %y.ref)
// CHECK:STDOUT:   %.loc11_22.1: ref %empty_tuple.type = temporary_storage
// CHECK:STDOUT:   %.loc11_22.2: ref %empty_tuple.type = temporary %.loc11_22.1, %Op.call
// CHECK:STDOUT:   %tuple: %empty_tuple.type = tuple_value () [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %.loc11_23: %empty_tuple.type = converted %Op.call, %tuple [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %Op.bound: <bound method> = bound_method %.loc11_22.1, constants.%Op.ea3
// CHECK:STDOUT:   %Op.specific_fn: <specific function> = specific_function constants.%Op.ea3, @Op.3(constants.%empty_tuple.type) [concrete = constants.%Op.specific_fn]
// CHECK:STDOUT:   %bound_method.loc11_22: <bound method> = bound_method %.loc11_22.1, %Op.specific_fn
// CHECK:STDOUT:   %addr: %ptr.843 = addr_of %.loc11_22.1
// CHECK:STDOUT:   %no_op: init %empty_tuple.type = call %bound_method.loc11_22(%addr)
// CHECK:STDOUT:   return %.loc11_23
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Op.1(constants.%Self.b3d) {
// CHECK:STDOUT:   %Self => constants.%Self.b3d
// CHECK:STDOUT:   %Self.as_type => constants.%Self.as_type.f73
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.8f9
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Op.1(constants.%Add.facet) {
// CHECK:STDOUT:   %Self => constants.%Add.facet
// CHECK:STDOUT:   %Self.as_type => constants.%empty_tuple.type
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.cb1
// CHECK:STDOUT: }
// CHECK:STDOUT:
