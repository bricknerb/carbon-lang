// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/init_nested.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/init_nested.carbon

class Inner {
  var a: i32;
  var b: i32;
}

fn MakeInner() -> Inner;

class Outer {
  var c: Inner;
  var d: Inner;
}

fn MakeOuter() -> Outer {
  return {.c = MakeInner(), .d = MakeInner()};
}

// CHECK:STDOUT: --- init_nested.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Inner: type = class_type @Inner [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %Inner.elem: type = unbound_element_type %Inner, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.a.b: type = struct_type {.a: %i32, .b: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.705: <witness> = complete_type_witness %struct_type.a.b [concrete]
// CHECK:STDOUT:   %pattern_type.a31: type = pattern_type %Inner [concrete]
// CHECK:STDOUT:   %MakeInner.type: type = fn_type @MakeInner [concrete]
// CHECK:STDOUT:   %MakeInner: %MakeInner.type = struct_value () [concrete]
// CHECK:STDOUT:   %Outer: type = class_type @Outer [concrete]
// CHECK:STDOUT:   %Outer.elem: type = unbound_element_type %Outer, %Inner [concrete]
// CHECK:STDOUT:   %struct_type.c.d.dce: type = struct_type {.c: %Inner, .d: %Inner} [concrete]
// CHECK:STDOUT:   %complete_type.8b6: <witness> = complete_type_witness %struct_type.c.d.dce [concrete]
// CHECK:STDOUT:   %pattern_type.e74: type = pattern_type %Outer [concrete]
// CHECK:STDOUT:   %MakeOuter.type: type = fn_type @MakeOuter [concrete]
// CHECK:STDOUT:   %MakeOuter: %MakeOuter.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/types/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Inner = %Inner.decl
// CHECK:STDOUT:     .MakeInner = %MakeInner.decl
// CHECK:STDOUT:     .Outer = %Outer.decl
// CHECK:STDOUT:     .MakeOuter = %MakeOuter.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Inner.decl: type = class_decl @Inner [concrete = constants.%Inner] {} {}
// CHECK:STDOUT:   %MakeInner.decl: %MakeInner.type = fn_decl @MakeInner [concrete = constants.%MakeInner] {
// CHECK:STDOUT:     %return.patt: %pattern_type.a31 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.a31 = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Inner.ref: type = name_ref Inner, file.%Inner.decl [concrete = constants.%Inner]
// CHECK:STDOUT:     %return.param: ref %Inner = out_param call_param0
// CHECK:STDOUT:     %return: ref %Inner = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Outer.decl: type = class_decl @Outer [concrete = constants.%Outer] {} {}
// CHECK:STDOUT:   %MakeOuter.decl: %MakeOuter.type = fn_decl @MakeOuter [concrete = constants.%MakeOuter] {
// CHECK:STDOUT:     %return.patt: %pattern_type.e74 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.e74 = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Outer.ref: type = name_ref Outer, file.%Outer.decl [concrete = constants.%Outer]
// CHECK:STDOUT:     %return.param: ref %Outer = out_param call_param0
// CHECK:STDOUT:     %return: ref %Outer = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Inner {
// CHECK:STDOUT:   %int_32.loc15: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32.loc15: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc15: %Inner.elem = field_decl a, element0 [concrete]
// CHECK:STDOUT:   %int_32.loc16: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32.loc16: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc16: %Inner.elem = field_decl b, element1 [concrete]
// CHECK:STDOUT:   %struct_type.a.b: type = struct_type {.a: %i32, .b: %i32} [concrete = constants.%struct_type.a.b]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.a.b [concrete = constants.%complete_type.705]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Inner
// CHECK:STDOUT:   .a = %.loc15
// CHECK:STDOUT:   .b = %.loc16
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Outer {
// CHECK:STDOUT:   %Inner.ref.loc22: type = name_ref Inner, file.%Inner.decl [concrete = constants.%Inner]
// CHECK:STDOUT:   %.loc22: %Outer.elem = field_decl c, element0 [concrete]
// CHECK:STDOUT:   %Inner.ref.loc23: type = name_ref Inner, file.%Inner.decl [concrete = constants.%Inner]
// CHECK:STDOUT:   %.loc23: %Outer.elem = field_decl d, element1 [concrete]
// CHECK:STDOUT:   %struct_type.c.d: type = struct_type {.c: %Inner, .d: %Inner} [concrete = constants.%struct_type.c.d.dce]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.c.d [concrete = constants.%complete_type.8b6]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Outer
// CHECK:STDOUT:   .Inner = <poisoned>
// CHECK:STDOUT:   .c = %.loc22
// CHECK:STDOUT:   .d = %.loc23
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @MakeInner() -> %return.param: %Inner;
// CHECK:STDOUT:
// CHECK:STDOUT: fn @MakeOuter() -> %return.param: %Outer {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %MakeInner.ref.loc27_16: %MakeInner.type = name_ref MakeInner, file.%MakeInner.decl [concrete = constants.%MakeInner]
// CHECK:STDOUT:   %.loc27_45.1: ref %Inner = class_element_access %return, element0
// CHECK:STDOUT:   %MakeInner.call.loc27_26: init %Inner = call %MakeInner.ref.loc27_16() to %.loc27_45.1
// CHECK:STDOUT:   %MakeInner.ref.loc27_34: %MakeInner.type = name_ref MakeInner, file.%MakeInner.decl [concrete = constants.%MakeInner]
// CHECK:STDOUT:   %.loc27_45.2: ref %Inner = class_element_access %return, element1
// CHECK:STDOUT:   %MakeInner.call.loc27_44: init %Inner = call %MakeInner.ref.loc27_34() to %.loc27_45.2
// CHECK:STDOUT:   %.loc27_45.3: %struct_type.c.d.dce = struct_literal (%MakeInner.call.loc27_26, %MakeInner.call.loc27_44)
// CHECK:STDOUT:   %.loc27_45.4: init %Outer = class_init (%MakeInner.call.loc27_26, %MakeInner.call.loc27_44), %return
// CHECK:STDOUT:   %.loc27_46: init %Outer = converted %.loc27_45.3, %.loc27_45.4
// CHECK:STDOUT:   return %.loc27_46 to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
