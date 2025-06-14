// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/destroy.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interface/fail_member_lookup.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interface/fail_member_lookup.carbon

// --- core.carbon
package Core;

interface ImplicitAs(Dest:! type) {
  fn Convert[self: Self]() -> Dest;
}

// --- fail_member_lookup.carbon
library "[[@TEST_NAME]]";

import Core;

interface Interface {
  fn F();

  let T:! type;
}

fn F() {
  // CHECK:STDERR: fail_member_lookup.carbon:[[@LINE+4]]:3: error: value of type `<associated entity in Interface>` is not callable [CallToNonCallable]
  // CHECK:STDERR:   Interface.F();
  // CHECK:STDERR:   ^~~~~~~~~~~~~
  // CHECK:STDERR:
  Interface.F();

  // CHECK:STDERR: fail_member_lookup.carbon:[[@LINE+7]]:10: error: cannot implicitly convert non-type value of type `<associated entity in Interface>` to `type` [ConversionFailureNonTypeToFacet]
  // CHECK:STDERR:   var v: Interface.T;
  // CHECK:STDERR:          ^~~~~~~~~~~
  // CHECK:STDERR: fail_member_lookup.carbon:[[@LINE+4]]:10: note: type `<associated entity in Interface>` does not implement interface `Core.ImplicitAs(type)` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   var v: Interface.T;
  // CHECK:STDERR:          ^~~~~~~~~~~
  // CHECK:STDERR:
  var v: Interface.T;
}

interface Different {}

// CHECK:STDERR: fail_member_lookup.carbon:[[@LINE+4]]:24: error: cannot convert type `U` that implements `Different` into type implementing `Interface` [ConversionFailureFacetToFacet]
// CHECK:STDERR: fn G(U:! Different) -> U.(Interface.T);
// CHECK:STDERR:                        ^~~~~~~~~~~~~~~
// CHECK:STDERR:
fn G(U:! Different) -> U.(Interface.T);

// CHECK:STDOUT: --- core.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.96f: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.96f = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.07f: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic]
// CHECK:STDOUT:   %Self: %ImplicitAs.type.07f = bind_symbolic_name Self, 1 [symbolic]
// CHECK:STDOUT:   %Self.as_type: type = facet_access_type %Self [symbolic]
// CHECK:STDOUT:   %pattern_type.a93: type = pattern_type %Self.as_type [symbolic]
// CHECK:STDOUT:   %pattern_type.7dc: type = pattern_type %Dest [symbolic]
// CHECK:STDOUT:   %Convert.type: type = fn_type @Convert, @ImplicitAs(%Dest) [symbolic]
// CHECK:STDOUT:   %Convert: %Convert.type = struct_value () [symbolic]
// CHECK:STDOUT:   %ImplicitAs.assoc_type: type = assoc_entity_type @ImplicitAs, @ImplicitAs(%Dest) [symbolic]
// CHECK:STDOUT:   %assoc0: %ImplicitAs.assoc_type = assoc_entity element0, @ImplicitAs.%Convert.decl [symbolic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .ImplicitAs = %ImplicitAs.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %ImplicitAs.decl: %ImplicitAs.type.96f = interface_decl @ImplicitAs [concrete = constants.%ImplicitAs.generic] {
// CHECK:STDOUT:     %Dest.patt: %pattern_type.98f = symbolic_binding_pattern Dest, 0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Dest.loc3_22.1: type = bind_symbolic_name Dest, 0 [symbolic = %Dest.loc3_22.2 (constants.%Dest)]
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic interface @ImplicitAs(%Dest.loc3_22.1: type) {
// CHECK:STDOUT:   %Dest.loc3_22.2: type = bind_symbolic_name Dest, 0 [symbolic = %Dest.loc3_22.2 (constants.%Dest)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %ImplicitAs.type: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest.loc3_22.2)> [symbolic = %ImplicitAs.type (constants.%ImplicitAs.type.07f)]
// CHECK:STDOUT:   %Self.2: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.07f) = bind_symbolic_name Self, 1 [symbolic = %Self.2 (constants.%Self)]
// CHECK:STDOUT:   %Convert.type: type = fn_type @Convert, @ImplicitAs(%Dest.loc3_22.2) [symbolic = %Convert.type (constants.%Convert.type)]
// CHECK:STDOUT:   %Convert: @ImplicitAs.%Convert.type (%Convert.type) = struct_value () [symbolic = %Convert (constants.%Convert)]
// CHECK:STDOUT:   %ImplicitAs.assoc_type: type = assoc_entity_type @ImplicitAs, @ImplicitAs(%Dest.loc3_22.2) [symbolic = %ImplicitAs.assoc_type (constants.%ImplicitAs.assoc_type)]
// CHECK:STDOUT:   %assoc0.loc4_35.2: @ImplicitAs.%ImplicitAs.assoc_type (%ImplicitAs.assoc_type) = assoc_entity element0, %Convert.decl [symbolic = %assoc0.loc4_35.2 (constants.%assoc0)]
// CHECK:STDOUT:
// CHECK:STDOUT:   interface {
// CHECK:STDOUT:     %Self.1: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.07f) = bind_symbolic_name Self, 1 [symbolic = %Self.2 (constants.%Self)]
// CHECK:STDOUT:     %Convert.decl: @ImplicitAs.%Convert.type (%Convert.type) = fn_decl @Convert [symbolic = @ImplicitAs.%Convert (constants.%Convert)] {
// CHECK:STDOUT:       %self.patt: @Convert.%pattern_type.loc4_14 (%pattern_type.a93) = binding_pattern self [concrete]
// CHECK:STDOUT:       %self.param_patt: @Convert.%pattern_type.loc4_14 (%pattern_type.a93) = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:       %return.patt: @Convert.%pattern_type.loc4_28 (%pattern_type.7dc) = return_slot_pattern [concrete]
// CHECK:STDOUT:       %return.param_patt: @Convert.%pattern_type.loc4_28 (%pattern_type.7dc) = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:     } {
// CHECK:STDOUT:       %Dest.ref: type = name_ref Dest, @ImplicitAs.%Dest.loc3_22.1 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:       %self.param: @Convert.%Self.as_type.loc4_20.1 (%Self.as_type) = value_param call_param0
// CHECK:STDOUT:       %.loc4_20.1: type = splice_block %.loc4_20.3 [symbolic = %Self.as_type.loc4_20.1 (constants.%Self.as_type)] {
// CHECK:STDOUT:         %.loc4_20.2: @Convert.%ImplicitAs.type (%ImplicitAs.type.07f) = specific_constant @ImplicitAs.%Self.1, @ImplicitAs(constants.%Dest) [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:         %Self.ref: @Convert.%ImplicitAs.type (%ImplicitAs.type.07f) = name_ref Self, %.loc4_20.2 [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:         %Self.as_type.loc4_20.2: type = facet_access_type %Self.ref [symbolic = %Self.as_type.loc4_20.1 (constants.%Self.as_type)]
// CHECK:STDOUT:         %.loc4_20.3: type = converted %Self.ref, %Self.as_type.loc4_20.2 [symbolic = %Self.as_type.loc4_20.1 (constants.%Self.as_type)]
// CHECK:STDOUT:       }
// CHECK:STDOUT:       %self: @Convert.%Self.as_type.loc4_20.1 (%Self.as_type) = bind_name self, %self.param
// CHECK:STDOUT:       %return.param: ref @Convert.%Dest (%Dest) = out_param call_param1
// CHECK:STDOUT:       %return: ref @Convert.%Dest (%Dest) = return_slot %return.param
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %assoc0.loc4_35.1: @ImplicitAs.%ImplicitAs.assoc_type (%ImplicitAs.assoc_type) = assoc_entity element0, %Convert.decl [symbolic = %assoc0.loc4_35.2 (constants.%assoc0)]
// CHECK:STDOUT:
// CHECK:STDOUT:   !members:
// CHECK:STDOUT:     .Self = %Self.1
// CHECK:STDOUT:     .Dest = <poisoned>
// CHECK:STDOUT:     .Convert = %assoc0.loc4_35.1
// CHECK:STDOUT:     witness = (%Convert.decl)
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Convert(@ImplicitAs.%Dest.loc3_22.1: type, @ImplicitAs.%Self.1: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.07f)) {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:   %ImplicitAs.type: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic = %ImplicitAs.type (constants.%ImplicitAs.type.07f)]
// CHECK:STDOUT:   %Self: @Convert.%ImplicitAs.type (%ImplicitAs.type.07f) = bind_symbolic_name Self, 1 [symbolic = %Self (constants.%Self)]
// CHECK:STDOUT:   %Self.as_type.loc4_20.1: type = facet_access_type %Self [symbolic = %Self.as_type.loc4_20.1 (constants.%Self.as_type)]
// CHECK:STDOUT:   %pattern_type.loc4_14: type = pattern_type %Self.as_type.loc4_20.1 [symbolic = %pattern_type.loc4_14 (constants.%pattern_type.a93)]
// CHECK:STDOUT:   %pattern_type.loc4_28: type = pattern_type %Dest [symbolic = %pattern_type.loc4_28 (constants.%pattern_type.7dc)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%self.param: @Convert.%Self.as_type.loc4_20.1 (%Self.as_type)) -> @Convert.%Dest (%Dest);
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @ImplicitAs(constants.%Dest) {
// CHECK:STDOUT:   %Dest.loc3_22.2 => constants.%Dest
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Convert(constants.%Dest, constants.%Self) {
// CHECK:STDOUT:   %Dest => constants.%Dest
// CHECK:STDOUT:   %ImplicitAs.type => constants.%ImplicitAs.type.07f
// CHECK:STDOUT:   %Self => constants.%Self
// CHECK:STDOUT:   %Self.as_type.loc4_20.1 => constants.%Self.as_type
// CHECK:STDOUT:   %pattern_type.loc4_14 => constants.%pattern_type.a93
// CHECK:STDOUT:   %pattern_type.loc4_28 => constants.%pattern_type.7dc
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_member_lookup.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Interface.type: type = facet_type <@Interface> [concrete]
// CHECK:STDOUT:   %Self.719: %Interface.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %F.type.1ad: type = fn_type @F.1 [concrete]
// CHECK:STDOUT:   %F.5d3: %F.type.1ad = struct_value () [concrete]
// CHECK:STDOUT:   %Interface.assoc_type: type = assoc_entity_type @Interface [concrete]
// CHECK:STDOUT:   %assoc0.b4c: %Interface.assoc_type = assoc_entity element0, @Interface.%F.decl [concrete]
// CHECK:STDOUT:   %assoc1: %Interface.assoc_type = assoc_entity element1, @Interface.%T [concrete]
// CHECK:STDOUT:   %F.type.b25: type = fn_type @F.2 [concrete]
// CHECK:STDOUT:   %F.c41: %F.type.b25 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.cc7: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.cc7 = struct_value () [concrete]
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic]
// CHECK:STDOUT:   %ImplicitAs.type.d62: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic]
// CHECK:STDOUT:   %Self.519: %ImplicitAs.type.d62 = bind_symbolic_name Self, 1 [symbolic]
// CHECK:STDOUT:   %Convert.type.275: type = fn_type @Convert, @ImplicitAs(%Dest) [symbolic]
// CHECK:STDOUT:   %Convert.42e: %Convert.type.275 = struct_value () [symbolic]
// CHECK:STDOUT:   %pattern_type.7dc: type = pattern_type %Dest [symbolic]
// CHECK:STDOUT:   %Self.as_type: type = facet_access_type %Self.519 [symbolic]
// CHECK:STDOUT:   %pattern_type.f3e: type = pattern_type %Self.as_type [symbolic]
// CHECK:STDOUT:   %ImplicitAs.assoc_type.ca0: type = assoc_entity_type @ImplicitAs, @ImplicitAs(%Dest) [symbolic]
// CHECK:STDOUT:   %assoc0.9f5: %ImplicitAs.assoc_type.ca0 = assoc_entity element0, imports.%Core.import_ref.1c7 [symbolic]
// CHECK:STDOUT:   %ImplicitAs.type.dc8: type = facet_type <@ImplicitAs, @ImplicitAs(type)> [concrete]
// CHECK:STDOUT:   %Self.94d: %ImplicitAs.type.dc8 = bind_symbolic_name Self, 1 [symbolic]
// CHECK:STDOUT:   %Convert.type.30b: type = fn_type @Convert, @ImplicitAs(type) [concrete]
// CHECK:STDOUT:   %Convert.732: %Convert.type.30b = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.assoc_type.ce4: type = assoc_entity_type @ImplicitAs, @ImplicitAs(type) [concrete]
// CHECK:STDOUT:   %assoc0.73f: %ImplicitAs.assoc_type.ce4 = assoc_entity element0, imports.%Core.import_ref.1c7 [concrete]
// CHECK:STDOUT:   %assoc0.dc0: %ImplicitAs.assoc_type.ca0 = assoc_entity element0, imports.%Core.import_ref.207 [symbolic]
// CHECK:STDOUT:   %Different.type: type = facet_type <@Different> [concrete]
// CHECK:STDOUT:   %Self.e7f: %Different.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %U: %Different.type = bind_symbolic_name U, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.855: type = pattern_type %Different.type [concrete]
// CHECK:STDOUT:   %G.type: type = fn_type @G [concrete]
// CHECK:STDOUT:   %G: %G.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .ImplicitAs = %Core.ImplicitAs
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//default
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.ImplicitAs: %ImplicitAs.type.cc7 = import_ref Core//default, ImplicitAs, loaded [concrete = constants.%ImplicitAs.generic]
// CHECK:STDOUT:   %Core.import_ref.5ab3ec.1: type = import_ref Core//default, loc3_22, loaded [symbolic = @ImplicitAs.%Dest (constants.%Dest)]
// CHECK:STDOUT:   %Core.import_ref.ff5 = import_ref Core//default, inst25 [no loc], unloaded
// CHECK:STDOUT:   %Core.import_ref.492: @ImplicitAs.%ImplicitAs.assoc_type (%ImplicitAs.assoc_type.ca0) = import_ref Core//default, loc4_35, loaded [symbolic = @ImplicitAs.%assoc0 (constants.%assoc0.dc0)]
// CHECK:STDOUT:   %Core.Convert = import_ref Core//default, Convert, unloaded
// CHECK:STDOUT:   %Core.import_ref.5ab3ec.2: type = import_ref Core//default, loc3_22, loaded [symbolic = @ImplicitAs.%Dest (constants.%Dest)]
// CHECK:STDOUT:   %Core.import_ref.ce1: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.d62) = import_ref Core//default, inst25 [no loc], loaded [symbolic = @ImplicitAs.%Self (constants.%Self.519)]
// CHECK:STDOUT:   %Core.import_ref.1c7: @ImplicitAs.%Convert.type (%Convert.type.275) = import_ref Core//default, loc4_35, loaded [symbolic = @ImplicitAs.%Convert (constants.%Convert.42e)]
// CHECK:STDOUT:   %Core.import_ref.207 = import_ref Core//default, loc4_35, unloaded
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Interface = %Interface.decl
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:     .Different = %Different.decl
// CHECK:STDOUT:     .G = %G.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Interface.decl: type = interface_decl @Interface [concrete = constants.%Interface.type] {} {}
// CHECK:STDOUT:   %F.decl: %F.type.b25 = fn_decl @F.2 [concrete = constants.%F.c41] {} {}
// CHECK:STDOUT:   %Different.decl: type = interface_decl @Different [concrete = constants.%Different.type] {} {}
// CHECK:STDOUT:   %G.decl: %G.type = fn_decl @G [concrete = constants.%G] {
// CHECK:STDOUT:     %U.patt: %pattern_type.855 = symbolic_binding_pattern U, 0 [concrete]
// CHECK:STDOUT:     %return.patt: <error> = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: <error> = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %U.ref: %Different.type = name_ref U, %U.loc34_6.1 [symbolic = %U.loc34_6.2 (constants.%U)]
// CHECK:STDOUT:     %Interface.ref: type = name_ref Interface, file.%Interface.decl [concrete = constants.%Interface.type]
// CHECK:STDOUT:     %T.ref: %Interface.assoc_type = name_ref T, @T.%assoc1 [concrete = constants.%assoc1]
// CHECK:STDOUT:     %Different.ref: type = name_ref Different, file.%Different.decl [concrete = constants.%Different.type]
// CHECK:STDOUT:     %U.loc34_6.1: %Different.type = bind_symbolic_name U, 0 [symbolic = %U.loc34_6.2 (constants.%U)]
// CHECK:STDOUT:     %return.param: ref <error> = out_param call_param0
// CHECK:STDOUT:     %return: ref <error> = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Interface {
// CHECK:STDOUT:   %Self: %Interface.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self.719]
// CHECK:STDOUT:   %F.decl: %F.type.1ad = fn_decl @F.1 [concrete = constants.%F.5d3] {} {}
// CHECK:STDOUT:   %assoc0: %Interface.assoc_type = assoc_entity element0, %F.decl [concrete = constants.%assoc0.b4c]
// CHECK:STDOUT:   %T: type = assoc_const_decl @T [concrete] {
// CHECK:STDOUT:     %assoc1: %Interface.assoc_type = assoc_entity element1, @Interface.%T [concrete = constants.%assoc1]
// CHECK:STDOUT:   }
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   .F = %assoc0
// CHECK:STDOUT:   .T = @T.%assoc1
// CHECK:STDOUT:   witness = (%F.decl, %T)
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic interface @ImplicitAs(imports.%Core.import_ref.5ab3ec.1: type) [from "core.carbon"] {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %ImplicitAs.type: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic = %ImplicitAs.type (constants.%ImplicitAs.type.d62)]
// CHECK:STDOUT:   %Self: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.d62) = bind_symbolic_name Self, 1 [symbolic = %Self (constants.%Self.519)]
// CHECK:STDOUT:   %Convert.type: type = fn_type @Convert, @ImplicitAs(%Dest) [symbolic = %Convert.type (constants.%Convert.type.275)]
// CHECK:STDOUT:   %Convert: @ImplicitAs.%Convert.type (%Convert.type.275) = struct_value () [symbolic = %Convert (constants.%Convert.42e)]
// CHECK:STDOUT:   %ImplicitAs.assoc_type: type = assoc_entity_type @ImplicitAs, @ImplicitAs(%Dest) [symbolic = %ImplicitAs.assoc_type (constants.%ImplicitAs.assoc_type.ca0)]
// CHECK:STDOUT:   %assoc0: @ImplicitAs.%ImplicitAs.assoc_type (%ImplicitAs.assoc_type.ca0) = assoc_entity element0, imports.%Core.import_ref.1c7 [symbolic = %assoc0 (constants.%assoc0.9f5)]
// CHECK:STDOUT:
// CHECK:STDOUT:   interface {
// CHECK:STDOUT:   !members:
// CHECK:STDOUT:     .Self = imports.%Core.import_ref.ff5
// CHECK:STDOUT:     .Convert = imports.%Core.import_ref.492
// CHECK:STDOUT:     witness = (imports.%Core.Convert)
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Different {
// CHECK:STDOUT:   %Self: %Different.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self.e7f]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   witness = ()
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic assoc_const @T(@Interface.%Self: %Interface.type) {
// CHECK:STDOUT:   assoc_const T:! type;
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F.1(@Interface.%Self: %Interface.type) {
// CHECK:STDOUT:   fn();
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F.2() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Interface.ref.loc16: type = name_ref Interface, file.%Interface.decl [concrete = constants.%Interface.type]
// CHECK:STDOUT:   %F.ref: %Interface.assoc_type = name_ref F, @Interface.%assoc0 [concrete = constants.%assoc0.b4c]
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: <error> = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: <error> = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref <error> = var %v.var_patt [concrete = <error>]
// CHECK:STDOUT:   %.1: <error> = splice_block <error> [concrete = <error>] {
// CHECK:STDOUT:     %Interface.ref.loc25: type = name_ref Interface, file.%Interface.decl [concrete = constants.%Interface.type]
// CHECK:STDOUT:     %T.ref: %Interface.assoc_type = name_ref T, @T.%assoc1 [concrete = constants.%assoc1]
// CHECK:STDOUT:     %.loc25: type = converted %T.ref, <error> [concrete = <error>]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: <error> = bind_name v, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Convert(imports.%Core.import_ref.5ab3ec.2: type, imports.%Core.import_ref.ce1: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.d62)) [from "core.carbon"] {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:   %ImplicitAs.type: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic = %ImplicitAs.type (constants.%ImplicitAs.type.d62)]
// CHECK:STDOUT:   %Self: @Convert.%ImplicitAs.type (%ImplicitAs.type.d62) = bind_symbolic_name Self, 1 [symbolic = %Self (constants.%Self.519)]
// CHECK:STDOUT:   %Self.as_type: type = facet_access_type %Self [symbolic = %Self.as_type (constants.%Self.as_type)]
// CHECK:STDOUT:   %pattern_type.1: type = pattern_type %Self.as_type [symbolic = %pattern_type.1 (constants.%pattern_type.f3e)]
// CHECK:STDOUT:   %pattern_type.2: type = pattern_type %Dest [symbolic = %pattern_type.2 (constants.%pattern_type.7dc)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn;
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @G(%U.loc34_6.1: %Different.type) {
// CHECK:STDOUT:   %U.loc34_6.2: %Different.type = bind_symbolic_name U, 0 [symbolic = %U.loc34_6.2 (constants.%U)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn() -> <error>;
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.1(constants.%Self.719) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @T(constants.%Self.719) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @ImplicitAs(constants.%Dest) {
// CHECK:STDOUT:   %Dest => constants.%Dest
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Convert(constants.%Dest, constants.%Self.519) {
// CHECK:STDOUT:   %Dest => constants.%Dest
// CHECK:STDOUT:   %ImplicitAs.type => constants.%ImplicitAs.type.d62
// CHECK:STDOUT:   %Self => constants.%Self.519
// CHECK:STDOUT:   %Self.as_type => constants.%Self.as_type
// CHECK:STDOUT:   %pattern_type.1 => constants.%pattern_type.f3e
// CHECK:STDOUT:   %pattern_type.2 => constants.%pattern_type.7dc
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @ImplicitAs(type) {
// CHECK:STDOUT:   %Dest => type
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %ImplicitAs.type => constants.%ImplicitAs.type.dc8
// CHECK:STDOUT:   %Self => constants.%Self.94d
// CHECK:STDOUT:   %Convert.type => constants.%Convert.type.30b
// CHECK:STDOUT:   %Convert => constants.%Convert.732
// CHECK:STDOUT:   %ImplicitAs.assoc_type => constants.%ImplicitAs.assoc_type.ce4
// CHECK:STDOUT:   %assoc0 => constants.%assoc0.73f
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @G(constants.%U) {
// CHECK:STDOUT:   %U.loc34_6.2 => constants.%U
// CHECK:STDOUT: }
// CHECK:STDOUT:
