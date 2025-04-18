// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/min_prelude/convert.carbon
// EXTRA-ARGS: --custom-core
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/facet/min_prelude/convert_class_value_to_facet_value_value.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/facet/min_prelude/convert_class_value_to_facet_value_value.carbon

interface Animal {}

fn WalkAnimal[T:! Animal](a: T) {}

class Goat {}
impl Goat as Animal {}

fn F() {
  WalkAnimal({} as Goat);
}

// CHECK:STDOUT: --- convert_class_value_to_facet_value_value.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Animal.type: type = facet_type <@Animal> [concrete]
// CHECK:STDOUT:   %Self: %Animal.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %T: %Animal.type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %T.patt: %Animal.type = symbolic_binding_pattern T, 0 [symbolic]
// CHECK:STDOUT:   %T.as_type: type = facet_access_type %T [symbolic]
// CHECK:STDOUT:   %WalkAnimal.type: type = fn_type @WalkAnimal [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %WalkAnimal: %WalkAnimal.type = struct_value () [concrete]
// CHECK:STDOUT:   %require_complete: <witness> = require_complete_type %T.as_type [symbolic]
// CHECK:STDOUT:   %Goat: type = class_type @Goat [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %impl_witness: <witness> = impl_witness () [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %Goat.val: %Goat = struct_value () [concrete]
// CHECK:STDOUT:   %Animal.facet: %Animal.type = facet_value %Goat, (%impl_witness) [concrete]
// CHECK:STDOUT:   %WalkAnimal.specific_fn: <specific function> = specific_function %WalkAnimal, @WalkAnimal(%Animal.facet) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Animal = %Animal.decl
// CHECK:STDOUT:     .WalkAnimal = %WalkAnimal.decl
// CHECK:STDOUT:     .Goat = %Goat.decl
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Animal.decl: type = interface_decl @Animal [concrete = constants.%Animal.type] {} {}
// CHECK:STDOUT:   %WalkAnimal.decl: %WalkAnimal.type = fn_decl @WalkAnimal [concrete = constants.%WalkAnimal] {
// CHECK:STDOUT:     %T.patt.loc16_15.1: %Animal.type = symbolic_binding_pattern T, 0 [symbolic = %T.patt.loc16_15.2 (constants.%T.patt)]
// CHECK:STDOUT:     %a.patt: @WalkAnimal.%T.as_type.loc16_30.2 (%T.as_type) = binding_pattern a
// CHECK:STDOUT:     %a.param_patt: @WalkAnimal.%T.as_type.loc16_30.2 (%T.as_type) = value_param_pattern %a.patt, call_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Animal.ref: type = name_ref Animal, file.%Animal.decl [concrete = constants.%Animal.type]
// CHECK:STDOUT:     %T.loc16_15.1: %Animal.type = bind_symbolic_name T, 0 [symbolic = %T.loc16_15.2 (constants.%T)]
// CHECK:STDOUT:     %a.param: @WalkAnimal.%T.as_type.loc16_30.2 (%T.as_type) = value_param call_param0
// CHECK:STDOUT:     %.loc16_30.1: type = splice_block %.loc16_30.2 [symbolic = %T.as_type.loc16_30.2 (constants.%T.as_type)] {
// CHECK:STDOUT:       %T.ref: %Animal.type = name_ref T, %T.loc16_15.1 [symbolic = %T.loc16_15.2 (constants.%T)]
// CHECK:STDOUT:       %T.as_type.loc16_30.1: type = facet_access_type %T.ref [symbolic = %T.as_type.loc16_30.2 (constants.%T.as_type)]
// CHECK:STDOUT:       %.loc16_30.2: type = converted %T.ref, %T.as_type.loc16_30.1 [symbolic = %T.as_type.loc16_30.2 (constants.%T.as_type)]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %a: @WalkAnimal.%T.as_type.loc16_30.2 (%T.as_type) = bind_name a, %a.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Goat.decl: type = class_decl @Goat [concrete = constants.%Goat] {} {}
// CHECK:STDOUT:   impl_decl @impl [concrete] {} {
// CHECK:STDOUT:     %Goat.ref: type = name_ref Goat, file.%Goat.decl [concrete = constants.%Goat]
// CHECK:STDOUT:     %Animal.ref: type = name_ref Animal, file.%Animal.decl [concrete = constants.%Animal.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %impl_witness: <witness> = impl_witness () [concrete = constants.%impl_witness]
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Animal {
// CHECK:STDOUT:   %Self: %Animal.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   witness = ()
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: impl @impl: %Goat.ref as %Animal.ref {
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   witness = file.%impl_witness
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Goat {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Goat
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @WalkAnimal(%T.loc16_15.1: %Animal.type) {
// CHECK:STDOUT:   %T.loc16_15.2: %Animal.type = bind_symbolic_name T, 0 [symbolic = %T.loc16_15.2 (constants.%T)]
// CHECK:STDOUT:   %T.patt.loc16_15.2: %Animal.type = symbolic_binding_pattern T, 0 [symbolic = %T.patt.loc16_15.2 (constants.%T.patt)]
// CHECK:STDOUT:   %T.as_type.loc16_30.2: type = facet_access_type %T.loc16_15.2 [symbolic = %T.as_type.loc16_30.2 (constants.%T.as_type)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %require_complete: <witness> = require_complete_type @WalkAnimal.%T.as_type.loc16_30.2 (%T.as_type) [symbolic = %require_complete (constants.%require_complete)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn[%T.patt.loc16_15.1: %Animal.type](%a.param_patt: @WalkAnimal.%T.as_type.loc16_30.2 (%T.as_type)) {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     return
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %WalkAnimal.ref: %WalkAnimal.type = name_ref WalkAnimal, file.%WalkAnimal.decl [concrete = constants.%WalkAnimal]
// CHECK:STDOUT:   %.loc22_15.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %Goat.ref: type = name_ref Goat, file.%Goat.decl [concrete = constants.%Goat]
// CHECK:STDOUT:   %.loc22_15.2: ref %Goat = temporary_storage
// CHECK:STDOUT:   %.loc22_15.3: init %Goat = class_init (), %.loc22_15.2 [concrete = constants.%Goat.val]
// CHECK:STDOUT:   %.loc22_15.4: ref %Goat = temporary %.loc22_15.2, %.loc22_15.3
// CHECK:STDOUT:   %.loc22_17.1: ref %Goat = converted %.loc22_15.1, %.loc22_15.4
// CHECK:STDOUT:   %Animal.facet.loc22_24.1: %Animal.type = facet_value constants.%Goat, (constants.%impl_witness) [concrete = constants.%Animal.facet]
// CHECK:STDOUT:   %.loc22_24.1: %Animal.type = converted constants.%Goat, %Animal.facet.loc22_24.1 [concrete = constants.%Animal.facet]
// CHECK:STDOUT:   %Animal.facet.loc22_24.2: %Animal.type = facet_value constants.%Goat, (constants.%impl_witness) [concrete = constants.%Animal.facet]
// CHECK:STDOUT:   %.loc22_24.2: %Animal.type = converted constants.%Goat, %Animal.facet.loc22_24.2 [concrete = constants.%Animal.facet]
// CHECK:STDOUT:   %WalkAnimal.specific_fn: <specific function> = specific_function %WalkAnimal.ref, @WalkAnimal(constants.%Animal.facet) [concrete = constants.%WalkAnimal.specific_fn]
// CHECK:STDOUT:   %.loc22_17.2: %Goat = bind_value %.loc22_17.1
// CHECK:STDOUT:   %WalkAnimal.call: init %empty_tuple.type = call %WalkAnimal.specific_fn(%.loc22_17.2)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @WalkAnimal(constants.%T) {
// CHECK:STDOUT:   %T.loc16_15.2 => constants.%T
// CHECK:STDOUT:   %T.patt.loc16_15.2 => constants.%T.patt
// CHECK:STDOUT:   %T.as_type.loc16_30.2 => constants.%T.as_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @WalkAnimal(constants.%Animal.facet) {
// CHECK:STDOUT:   %T.loc16_15.2 => constants.%Animal.facet
// CHECK:STDOUT:   %T.patt.loc16_15.2 => constants.%T.patt
// CHECK:STDOUT:   %T.as_type.loc16_30.2 => constants.%Goat
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %require_complete => constants.%complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- include_files/convert.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic]
// CHECK:STDOUT:   %Dest.patt: type = symbolic_binding_pattern Dest, 0 [symbolic]
// CHECK:STDOUT:   %As.type.b51: type = generic_interface_type @As [concrete]
// CHECK:STDOUT:   %As.generic: %As.type.b51 = struct_value () [concrete]
// CHECK:STDOUT:   %As.type.8ba: type = facet_type <@As, @As(%Dest)> [symbolic]
// CHECK:STDOUT:   %Self.b4e: %As.type.8ba = bind_symbolic_name Self, 1 [symbolic]
// CHECK:STDOUT:   %Self.as_type.7f0: type = facet_access_type %Self.b4e [symbolic]
// CHECK:STDOUT:   %Convert.type.ad1: type = fn_type @Convert.1, @As(%Dest) [symbolic]
// CHECK:STDOUT:   %Convert.0ed: %Convert.type.ad1 = struct_value () [symbolic]
// CHECK:STDOUT:   %As.assoc_type: type = assoc_entity_type @As, @As(%Dest) [symbolic]
// CHECK:STDOUT:   %assoc0.1d5: %As.assoc_type = assoc_entity element0, @As.%Convert.decl [symbolic]
// CHECK:STDOUT:   %ImplicitAs.type.96f: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.96f = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.07f: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic]
// CHECK:STDOUT:   %Self.0f3: %ImplicitAs.type.07f = bind_symbolic_name Self, 1 [symbolic]
// CHECK:STDOUT:   %Self.as_type.419: type = facet_access_type %Self.0f3 [symbolic]
// CHECK:STDOUT:   %Convert.type.4cf: type = fn_type @Convert.2, @ImplicitAs(%Dest) [symbolic]
// CHECK:STDOUT:   %Convert.147: %Convert.type.4cf = struct_value () [symbolic]
// CHECK:STDOUT:   %ImplicitAs.assoc_type: type = assoc_entity_type @ImplicitAs, @ImplicitAs(%Dest) [symbolic]
// CHECK:STDOUT:   %assoc0.8f8: %ImplicitAs.assoc_type = assoc_entity element0, @ImplicitAs.%Convert.decl [symbolic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .As = %As.decl
// CHECK:STDOUT:     .ImplicitAs = %ImplicitAs.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %As.decl: %As.type.b51 = interface_decl @As [concrete = constants.%As.generic] {
// CHECK:STDOUT:     %Dest.patt.loc8_14.1: type = symbolic_binding_pattern Dest, 0 [symbolic = %Dest.patt.loc8_14.2 (constants.%Dest.patt)]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Dest.loc8_14.1: type = bind_symbolic_name Dest, 0 [symbolic = %Dest.loc8_14.2 (constants.%Dest)]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %ImplicitAs.decl: %ImplicitAs.type.96f = interface_decl @ImplicitAs [concrete = constants.%ImplicitAs.generic] {
// CHECK:STDOUT:     %Dest.patt.loc12_22.1: type = symbolic_binding_pattern Dest, 0 [symbolic = %Dest.patt.loc12_22.2 (constants.%Dest.patt)]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Dest.loc12_22.1: type = bind_symbolic_name Dest, 0 [symbolic = %Dest.loc12_22.2 (constants.%Dest)]
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic interface @As(%Dest.loc8_14.1: type) {
// CHECK:STDOUT:   %Dest.loc8_14.2: type = bind_symbolic_name Dest, 0 [symbolic = %Dest.loc8_14.2 (constants.%Dest)]
// CHECK:STDOUT:   %Dest.patt.loc8_14.2: type = symbolic_binding_pattern Dest, 0 [symbolic = %Dest.patt.loc8_14.2 (constants.%Dest.patt)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %As.type: type = facet_type <@As, @As(%Dest.loc8_14.2)> [symbolic = %As.type (constants.%As.type.8ba)]
// CHECK:STDOUT:   %Self.2: @As.%As.type (%As.type.8ba) = bind_symbolic_name Self, 1 [symbolic = %Self.2 (constants.%Self.b4e)]
// CHECK:STDOUT:   %Convert.type: type = fn_type @Convert.1, @As(%Dest.loc8_14.2) [symbolic = %Convert.type (constants.%Convert.type.ad1)]
// CHECK:STDOUT:   %Convert: @As.%Convert.type (%Convert.type.ad1) = struct_value () [symbolic = %Convert (constants.%Convert.0ed)]
// CHECK:STDOUT:   %As.assoc_type: type = assoc_entity_type @As, @As(%Dest.loc8_14.2) [symbolic = %As.assoc_type (constants.%As.assoc_type)]
// CHECK:STDOUT:   %assoc0.loc9_35.2: @As.%As.assoc_type (%As.assoc_type) = assoc_entity element0, %Convert.decl [symbolic = %assoc0.loc9_35.2 (constants.%assoc0.1d5)]
// CHECK:STDOUT:
// CHECK:STDOUT:   interface {
// CHECK:STDOUT:     %Self.1: @As.%As.type (%As.type.8ba) = bind_symbolic_name Self, 1 [symbolic = %Self.2 (constants.%Self.b4e)]
// CHECK:STDOUT:     %Convert.decl: @As.%Convert.type (%Convert.type.ad1) = fn_decl @Convert.1 [symbolic = @As.%Convert (constants.%Convert.0ed)] {
// CHECK:STDOUT:       %self.patt: @Convert.1.%Self.as_type.loc9_20.1 (%Self.as_type.7f0) = binding_pattern self
// CHECK:STDOUT:       %self.param_patt: @Convert.1.%Self.as_type.loc9_20.1 (%Self.as_type.7f0) = value_param_pattern %self.patt, call_param0
// CHECK:STDOUT:       %return.patt: @Convert.1.%Dest (%Dest) = return_slot_pattern
// CHECK:STDOUT:       %return.param_patt: @Convert.1.%Dest (%Dest) = out_param_pattern %return.patt, call_param1
// CHECK:STDOUT:     } {
// CHECK:STDOUT:       %Dest.ref: type = name_ref Dest, @As.%Dest.loc8_14.1 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:       %self.param: @Convert.1.%Self.as_type.loc9_20.1 (%Self.as_type.7f0) = value_param call_param0
// CHECK:STDOUT:       %.loc9_20.1: type = splice_block %.loc9_20.3 [symbolic = %Self.as_type.loc9_20.1 (constants.%Self.as_type.7f0)] {
// CHECK:STDOUT:         %.loc9_20.2: @Convert.1.%As.type (%As.type.8ba) = specific_constant @As.%Self.1, @As(constants.%Dest) [symbolic = %Self (constants.%Self.b4e)]
// CHECK:STDOUT:         %Self.ref: @Convert.1.%As.type (%As.type.8ba) = name_ref Self, %.loc9_20.2 [symbolic = %Self (constants.%Self.b4e)]
// CHECK:STDOUT:         %Self.as_type.loc9_20.2: type = facet_access_type %Self.ref [symbolic = %Self.as_type.loc9_20.1 (constants.%Self.as_type.7f0)]
// CHECK:STDOUT:         %.loc9_20.3: type = converted %Self.ref, %Self.as_type.loc9_20.2 [symbolic = %Self.as_type.loc9_20.1 (constants.%Self.as_type.7f0)]
// CHECK:STDOUT:       }
// CHECK:STDOUT:       %self: @Convert.1.%Self.as_type.loc9_20.1 (%Self.as_type.7f0) = bind_name self, %self.param
// CHECK:STDOUT:       %return.param: ref @Convert.1.%Dest (%Dest) = out_param call_param1
// CHECK:STDOUT:       %return: ref @Convert.1.%Dest (%Dest) = return_slot %return.param
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %assoc0.loc9_35.1: @As.%As.assoc_type (%As.assoc_type) = assoc_entity element0, %Convert.decl [symbolic = %assoc0.loc9_35.2 (constants.%assoc0.1d5)]
// CHECK:STDOUT:
// CHECK:STDOUT:   !members:
// CHECK:STDOUT:     .Self = %Self.1
// CHECK:STDOUT:     .Dest = <poisoned>
// CHECK:STDOUT:     .Convert = %assoc0.loc9_35.1
// CHECK:STDOUT:     witness = (%Convert.decl)
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic interface @ImplicitAs(%Dest.loc12_22.1: type) {
// CHECK:STDOUT:   %Dest.loc12_22.2: type = bind_symbolic_name Dest, 0 [symbolic = %Dest.loc12_22.2 (constants.%Dest)]
// CHECK:STDOUT:   %Dest.patt.loc12_22.2: type = symbolic_binding_pattern Dest, 0 [symbolic = %Dest.patt.loc12_22.2 (constants.%Dest.patt)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %ImplicitAs.type: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest.loc12_22.2)> [symbolic = %ImplicitAs.type (constants.%ImplicitAs.type.07f)]
// CHECK:STDOUT:   %Self.2: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.07f) = bind_symbolic_name Self, 1 [symbolic = %Self.2 (constants.%Self.0f3)]
// CHECK:STDOUT:   %Convert.type: type = fn_type @Convert.2, @ImplicitAs(%Dest.loc12_22.2) [symbolic = %Convert.type (constants.%Convert.type.4cf)]
// CHECK:STDOUT:   %Convert: @ImplicitAs.%Convert.type (%Convert.type.4cf) = struct_value () [symbolic = %Convert (constants.%Convert.147)]
// CHECK:STDOUT:   %ImplicitAs.assoc_type: type = assoc_entity_type @ImplicitAs, @ImplicitAs(%Dest.loc12_22.2) [symbolic = %ImplicitAs.assoc_type (constants.%ImplicitAs.assoc_type)]
// CHECK:STDOUT:   %assoc0.loc14_35.2: @ImplicitAs.%ImplicitAs.assoc_type (%ImplicitAs.assoc_type) = assoc_entity element0, %Convert.decl [symbolic = %assoc0.loc14_35.2 (constants.%assoc0.8f8)]
// CHECK:STDOUT:
// CHECK:STDOUT:   interface {
// CHECK:STDOUT:     %Self.1: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.07f) = bind_symbolic_name Self, 1 [symbolic = %Self.2 (constants.%Self.0f3)]
// CHECK:STDOUT:     %Convert.decl: @ImplicitAs.%Convert.type (%Convert.type.4cf) = fn_decl @Convert.2 [symbolic = @ImplicitAs.%Convert (constants.%Convert.147)] {
// CHECK:STDOUT:       %self.patt: @Convert.2.%Self.as_type.loc14_20.1 (%Self.as_type.419) = binding_pattern self
// CHECK:STDOUT:       %self.param_patt: @Convert.2.%Self.as_type.loc14_20.1 (%Self.as_type.419) = value_param_pattern %self.patt, call_param0
// CHECK:STDOUT:       %return.patt: @Convert.2.%Dest (%Dest) = return_slot_pattern
// CHECK:STDOUT:       %return.param_patt: @Convert.2.%Dest (%Dest) = out_param_pattern %return.patt, call_param1
// CHECK:STDOUT:     } {
// CHECK:STDOUT:       %Dest.ref: type = name_ref Dest, @ImplicitAs.%Dest.loc12_22.1 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:       %self.param: @Convert.2.%Self.as_type.loc14_20.1 (%Self.as_type.419) = value_param call_param0
// CHECK:STDOUT:       %.loc14_20.1: type = splice_block %.loc14_20.3 [symbolic = %Self.as_type.loc14_20.1 (constants.%Self.as_type.419)] {
// CHECK:STDOUT:         %.loc14_20.2: @Convert.2.%ImplicitAs.type (%ImplicitAs.type.07f) = specific_constant @ImplicitAs.%Self.1, @ImplicitAs(constants.%Dest) [symbolic = %Self (constants.%Self.0f3)]
// CHECK:STDOUT:         %Self.ref: @Convert.2.%ImplicitAs.type (%ImplicitAs.type.07f) = name_ref Self, %.loc14_20.2 [symbolic = %Self (constants.%Self.0f3)]
// CHECK:STDOUT:         %Self.as_type.loc14_20.2: type = facet_access_type %Self.ref [symbolic = %Self.as_type.loc14_20.1 (constants.%Self.as_type.419)]
// CHECK:STDOUT:         %.loc14_20.3: type = converted %Self.ref, %Self.as_type.loc14_20.2 [symbolic = %Self.as_type.loc14_20.1 (constants.%Self.as_type.419)]
// CHECK:STDOUT:       }
// CHECK:STDOUT:       %self: @Convert.2.%Self.as_type.loc14_20.1 (%Self.as_type.419) = bind_name self, %self.param
// CHECK:STDOUT:       %return.param: ref @Convert.2.%Dest (%Dest) = out_param call_param1
// CHECK:STDOUT:       %return: ref @Convert.2.%Dest (%Dest) = return_slot %return.param
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %assoc0.loc14_35.1: @ImplicitAs.%ImplicitAs.assoc_type (%ImplicitAs.assoc_type) = assoc_entity element0, %Convert.decl [symbolic = %assoc0.loc14_35.2 (constants.%assoc0.8f8)]
// CHECK:STDOUT:
// CHECK:STDOUT:   !members:
// CHECK:STDOUT:     .Self = %Self.1
// CHECK:STDOUT:     .Dest = <poisoned>
// CHECK:STDOUT:     .Convert = %assoc0.loc14_35.1
// CHECK:STDOUT:     witness = (%Convert.decl)
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Convert.1(@As.%Dest.loc8_14.1: type, @As.%Self.1: @As.%As.type (%As.type.8ba)) {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:   %As.type: type = facet_type <@As, @As(%Dest)> [symbolic = %As.type (constants.%As.type.8ba)]
// CHECK:STDOUT:   %Self: @Convert.1.%As.type (%As.type.8ba) = bind_symbolic_name Self, 1 [symbolic = %Self (constants.%Self.b4e)]
// CHECK:STDOUT:   %Self.as_type.loc9_20.1: type = facet_access_type %Self [symbolic = %Self.as_type.loc9_20.1 (constants.%Self.as_type.7f0)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn[%self.param_patt: @Convert.1.%Self.as_type.loc9_20.1 (%Self.as_type.7f0)]() -> @Convert.1.%Dest (%Dest);
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Convert.2(@ImplicitAs.%Dest.loc12_22.1: type, @ImplicitAs.%Self.1: @ImplicitAs.%ImplicitAs.type (%ImplicitAs.type.07f)) {
// CHECK:STDOUT:   %Dest: type = bind_symbolic_name Dest, 0 [symbolic = %Dest (constants.%Dest)]
// CHECK:STDOUT:   %ImplicitAs.type: type = facet_type <@ImplicitAs, @ImplicitAs(%Dest)> [symbolic = %ImplicitAs.type (constants.%ImplicitAs.type.07f)]
// CHECK:STDOUT:   %Self: @Convert.2.%ImplicitAs.type (%ImplicitAs.type.07f) = bind_symbolic_name Self, 1 [symbolic = %Self (constants.%Self.0f3)]
// CHECK:STDOUT:   %Self.as_type.loc14_20.1: type = facet_access_type %Self [symbolic = %Self.as_type.loc14_20.1 (constants.%Self.as_type.419)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn[%self.param_patt: @Convert.2.%Self.as_type.loc14_20.1 (%Self.as_type.419)]() -> @Convert.2.%Dest (%Dest);
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @As(constants.%Dest) {
// CHECK:STDOUT:   %Dest.loc8_14.2 => constants.%Dest
// CHECK:STDOUT:   %Dest.patt.loc8_14.2 => constants.%Dest.patt
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Convert.1(constants.%Dest, constants.%Self.b4e) {
// CHECK:STDOUT:   %Dest => constants.%Dest
// CHECK:STDOUT:   %As.type => constants.%As.type.8ba
// CHECK:STDOUT:   %Self => constants.%Self.b4e
// CHECK:STDOUT:   %Self.as_type.loc9_20.1 => constants.%Self.as_type.7f0
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @As(@Convert.1.%Dest) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @As(%Dest.loc8_14.2) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @ImplicitAs(constants.%Dest) {
// CHECK:STDOUT:   %Dest.loc12_22.2 => constants.%Dest
// CHECK:STDOUT:   %Dest.patt.loc12_22.2 => constants.%Dest.patt
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Convert.2(constants.%Dest, constants.%Self.0f3) {
// CHECK:STDOUT:   %Dest => constants.%Dest
// CHECK:STDOUT:   %ImplicitAs.type => constants.%ImplicitAs.type.07f
// CHECK:STDOUT:   %Self => constants.%Self.0f3
// CHECK:STDOUT:   %Self.as_type.loc14_20.1 => constants.%Self.as_type.419
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @ImplicitAs(@Convert.2.%Dest) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @ImplicitAs(%Dest.loc12_22.2) {}
// CHECK:STDOUT:
