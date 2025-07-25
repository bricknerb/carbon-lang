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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/self_type.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/self_type.carbon

class Class {
  fn F[self: Self]() -> i32;
  fn Make() -> Self {
    returned var s: Self;
    s = {.p = &s};
    return var;
  }
  var p: Self*;
}

fn Class.F[self: Self]() -> i32 {
  return (*self.p).F();
}

// CHECK:STDOUT: --- self_type.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Class: type = class_type @Class [concrete]
// CHECK:STDOUT:   %pattern_type.761: type = pattern_type %Class [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.7ce: type = pattern_type %i32 [concrete]
// CHECK:STDOUT:   %Class.F.type: type = fn_type @Class.F [concrete]
// CHECK:STDOUT:   %Class.F: %Class.F.type = struct_value () [concrete]
// CHECK:STDOUT:   %Class.Make.type: type = fn_type @Class.Make [concrete]
// CHECK:STDOUT:   %Class.Make: %Class.Make.type = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.e71: type = ptr_type %Class [concrete]
// CHECK:STDOUT:   %Class.elem: type = unbound_element_type %Class, %ptr.e71 [concrete]
// CHECK:STDOUT:   %struct_type.p: type = struct_type {.p: %ptr.e71} [concrete]
// CHECK:STDOUT:   %complete_type.56d: <witness> = complete_type_witness %struct_type.p [concrete]
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
// CHECK:STDOUT:     .Class = %Class.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Class.decl: type = class_decl @Class [concrete = constants.%Class] {} {}
// CHECK:STDOUT:   %Class.F.decl: %Class.F.type = fn_decl @Class.F [concrete = constants.%Class.F] {
// CHECK:STDOUT:     %self.patt: %pattern_type.761 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.761 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.7ce = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.7ce = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc25: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc25: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %self.param.loc25: %Class = value_param call_param0
// CHECK:STDOUT:     %Self.ref.loc25: type = name_ref Self, constants.%Class [concrete = constants.%Class]
// CHECK:STDOUT:     %self.loc25: %Class = bind_name self, %self.param.loc25
// CHECK:STDOUT:     %return.param.loc25: ref %i32 = out_param call_param1
// CHECK:STDOUT:     %return.loc25: ref %i32 = return_slot %return.param.loc25
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Class {
// CHECK:STDOUT:   %Class.F.decl: %Class.F.type = fn_decl @Class.F [concrete = constants.%Class.F] {
// CHECK:STDOUT:     %self.patt: %pattern_type.761 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.761 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.7ce = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.7ce = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc16: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc16: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %self.param.loc16: %Class = value_param call_param0
// CHECK:STDOUT:     %Self.ref.loc16: type = name_ref Self, constants.%Class [concrete = constants.%Class]
// CHECK:STDOUT:     %self.loc16: %Class = bind_name self, %self.param.loc16
// CHECK:STDOUT:     %return.param.loc16: ref %i32 = out_param call_param1
// CHECK:STDOUT:     %return.loc16: ref %i32 = return_slot %return.param.loc16
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Class.Make.decl: %Class.Make.type = fn_decl @Class.Make [concrete = constants.%Class.Make] {
// CHECK:STDOUT:     %return.patt: %pattern_type.761 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.761 = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Self.ref.loc17: type = name_ref Self, constants.%Class [concrete = constants.%Class]
// CHECK:STDOUT:     %return.param: ref %Class = out_param call_param0
// CHECK:STDOUT:     %return: ref %Class = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Self.ref: type = name_ref Self, constants.%Class [concrete = constants.%Class]
// CHECK:STDOUT:   %ptr: type = ptr_type %Self.ref [concrete = constants.%ptr.e71]
// CHECK:STDOUT:   %.loc22: %Class.elem = field_decl p, element0 [concrete]
// CHECK:STDOUT:   %struct_type.p: type = struct_type {.p: %ptr.e71} [concrete = constants.%struct_type.p]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.p [concrete = constants.%complete_type.56d]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Class
// CHECK:STDOUT:   .F = %Class.F.decl
// CHECK:STDOUT:   .Make = %Class.Make.decl
// CHECK:STDOUT:   .p = %.loc22
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Class.F(%self.param.loc25: %Class) -> %i32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %self.ref: %Class = name_ref self, %self.loc25
// CHECK:STDOUT:   %p.ref: %Class.elem = name_ref p, @Class.%.loc22 [concrete = @Class.%.loc22]
// CHECK:STDOUT:   %.loc26_16.1: ref %ptr.e71 = class_element_access %self.ref, element0
// CHECK:STDOUT:   %.loc26_16.2: %ptr.e71 = bind_value %.loc26_16.1
// CHECK:STDOUT:   %.loc26_11.1: ref %Class = deref %.loc26_16.2
// CHECK:STDOUT:   %F.ref: %Class.F.type = name_ref F, @Class.%Class.F.decl [concrete = constants.%Class.F]
// CHECK:STDOUT:   %Class.F.bound: <bound method> = bound_method %.loc26_11.1, %F.ref
// CHECK:STDOUT:   %.loc26_11.2: %Class = bind_value %.loc26_11.1
// CHECK:STDOUT:   %Class.F.call: init %i32 = call %Class.F.bound(%.loc26_11.2)
// CHECK:STDOUT:   %.loc26_23.1: %i32 = value_of_initializer %Class.F.call
// CHECK:STDOUT:   %.loc26_23.2: %i32 = converted %Class.F.call, %.loc26_23.1
// CHECK:STDOUT:   return %.loc26_23.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Class.Make() -> %return.param: %Class {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %s.patt: %pattern_type.761 = binding_pattern s [concrete]
// CHECK:STDOUT:     %s.var_patt: %pattern_type.761 = var_pattern %s.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Self.ref.loc18: type = name_ref Self, constants.%Class [concrete = constants.%Class]
// CHECK:STDOUT:   %s: ref %Class = bind_name s, %return
// CHECK:STDOUT:   %s.ref.loc19_5: ref %Class = name_ref s, %s
// CHECK:STDOUT:   %s.ref.loc19_16: ref %Class = name_ref s, %s
// CHECK:STDOUT:   %addr: %ptr.e71 = addr_of %s.ref.loc19_16
// CHECK:STDOUT:   %.loc19_17.1: %struct_type.p = struct_literal (%addr)
// CHECK:STDOUT:   %.loc19_17.2: ref %ptr.e71 = class_element_access %s.ref.loc19_5, element0
// CHECK:STDOUT:   %.loc19_17.3: init %ptr.e71 = initialize_from %addr to %.loc19_17.2
// CHECK:STDOUT:   %.loc19_17.4: init %Class = class_init (%.loc19_17.3), %s.ref.loc19_5
// CHECK:STDOUT:   %.loc19_7: init %Class = converted %.loc19_17.1, %.loc19_17.4
// CHECK:STDOUT:   assign %s.ref.loc19_5, %.loc19_7
// CHECK:STDOUT:   return %s to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
