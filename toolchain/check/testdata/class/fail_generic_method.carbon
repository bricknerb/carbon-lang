// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/fail_generic_method.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/fail_generic_method.carbon

class Class(T:! type) {
  var a: T;
  fn F[self: Self](n: T);
}

// TODO: The follow-on errors here aren't great. Investigate whether we can
// enter the scope anyway if the parameters don't match.
// CHECK:STDERR: fail_generic_method.carbon:[[@LINE+15]]:10: error: type `<pattern for i32>` of parameter 1 in redeclaration differs from previous parameter type `<pattern for type>` [RedeclParamDiffersType]
// CHECK:STDERR: fn Class(N:! i32).F[self: Self](n: T) {}
// CHECK:STDERR:          ^
// CHECK:STDERR: fail_generic_method.carbon:[[@LINE-10]]:13: note: previous declaration's corresponding parameter here [RedeclParamPrevious]
// CHECK:STDERR: class Class(T:! type) {
// CHECK:STDERR:             ^
// CHECK:STDERR:
// CHECK:STDERR: fail_generic_method.carbon:[[@LINE+8]]:27: error: name `Self` not found [NameNotFound]
// CHECK:STDERR: fn Class(N:! i32).F[self: Self](n: T) {}
// CHECK:STDERR:                           ^~~~
// CHECK:STDERR:
// CHECK:STDERR: fail_generic_method.carbon:[[@LINE+4]]:36: error: name `T` not found [NameNotFound]
// CHECK:STDERR: fn Class(N:! i32).F[self: Self](n: T) {}
// CHECK:STDERR:                                    ^
// CHECK:STDERR:
fn Class(N:! i32).F[self: Self](n: T) {}

// CHECK:STDOUT: --- fail_generic_method.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %Class.type: type = generic_class_type @Class [concrete]
// CHECK:STDOUT:   %Class.generic: %Class.type = struct_value () [concrete]
// CHECK:STDOUT:   %Class: type = class_type @Class, @Class(%T) [symbolic]
// CHECK:STDOUT:   %require_complete.4ae: <witness> = require_complete_type %T [symbolic]
// CHECK:STDOUT:   %Class.elem: type = unbound_element_type %Class, %T [symbolic]
// CHECK:STDOUT:   %pattern_type.3c1: type = pattern_type %Class [symbolic]
// CHECK:STDOUT:   %pattern_type.7dc: type = pattern_type %T [symbolic]
// CHECK:STDOUT:   %F.type.6d6: type = fn_type @F.1, @Class(%T) [symbolic]
// CHECK:STDOUT:   %F.cca: %F.type.6d6 = struct_value () [symbolic]
// CHECK:STDOUT:   %struct_type.a: type = struct_type {.a: %T} [symbolic]
// CHECK:STDOUT:   %complete_type.f1b: <witness> = complete_type_witness %struct_type.a [symbolic]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %N.51e: %i32 = bind_symbolic_name N, 0 [symbolic]
// CHECK:STDOUT:   %F.type.b25: type = fn_type @F.2 [concrete]
// CHECK:STDOUT:   %F.c41: %F.type.b25 = struct_value () [concrete]
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
// CHECK:STDOUT:     .Class = %Class.decl
// CHECK:STDOUT:     .T = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Class.decl: %Class.type = class_decl @Class [concrete = constants.%Class.generic] {
// CHECK:STDOUT:     %T.patt: %pattern_type.98f = symbolic_binding_pattern T, 0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %T.loc14_13.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc14_13.2 (constants.%T)]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.decl: %F.type.b25 = fn_decl @F.2 [concrete = constants.%F.c41] {
// CHECK:STDOUT:     %self.patt: <error> = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: <error> = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %n.patt: <error> = binding_pattern n [concrete]
// CHECK:STDOUT:     %n.param_patt: <error> = value_param_pattern %n.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %.loc36: type = splice_block %i32 [concrete = constants.%i32] {
// CHECK:STDOUT:       %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %N.loc36_10.1: %i32 = bind_symbolic_name N, 0 [symbolic = %N.loc36_10.2 (constants.%N.51e)]
// CHECK:STDOUT:     %self.param: <error> = value_param call_param0
// CHECK:STDOUT:     %Self.ref: <error> = name_ref Self, <error> [concrete = <error>]
// CHECK:STDOUT:     %self: <error> = bind_name self, %self.param [concrete = <error>]
// CHECK:STDOUT:     %n.param: <error> = value_param call_param1
// CHECK:STDOUT:     %T.ref: <error> = name_ref T, <error> [concrete = <error>]
// CHECK:STDOUT:     %n: <error> = bind_name n, %n.param [concrete = <error>]
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic class @Class(%T.loc14_13.1: type) {
// CHECK:STDOUT:   %T.loc14_13.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc14_13.2 (constants.%T)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:   %require_complete: <witness> = require_complete_type %T.loc14_13.2 [symbolic = %require_complete (constants.%require_complete.4ae)]
// CHECK:STDOUT:   %Class: type = class_type @Class, @Class(%T.loc14_13.2) [symbolic = %Class (constants.%Class)]
// CHECK:STDOUT:   %Class.elem: type = unbound_element_type %Class, %T.loc14_13.2 [symbolic = %Class.elem (constants.%Class.elem)]
// CHECK:STDOUT:   %F.type: type = fn_type @F.1, @Class(%T.loc14_13.2) [symbolic = %F.type (constants.%F.type.6d6)]
// CHECK:STDOUT:   %F: @Class.%F.type (%F.type.6d6) = struct_value () [symbolic = %F (constants.%F.cca)]
// CHECK:STDOUT:   %struct_type.a.loc17_1.2: type = struct_type {.a: @Class.%T.loc14_13.2 (%T)} [symbolic = %struct_type.a.loc17_1.2 (constants.%struct_type.a)]
// CHECK:STDOUT:   %complete_type.loc17_1.2: <witness> = complete_type_witness %struct_type.a.loc17_1.2 [symbolic = %complete_type.loc17_1.2 (constants.%complete_type.f1b)]
// CHECK:STDOUT:
// CHECK:STDOUT:   class {
// CHECK:STDOUT:     %T.ref: type = name_ref T, %T.loc14_13.1 [symbolic = %T.loc14_13.2 (constants.%T)]
// CHECK:STDOUT:     %.loc15: @Class.%Class.elem (%Class.elem) = field_decl a, element0 [concrete]
// CHECK:STDOUT:     %F.decl: @Class.%F.type (%F.type.6d6) = fn_decl @F.1 [symbolic = @Class.%F (constants.%F.cca)] {
// CHECK:STDOUT:       %self.patt: @F.1.%pattern_type.loc16_8 (%pattern_type.3c1) = binding_pattern self [concrete]
// CHECK:STDOUT:       %self.param_patt: @F.1.%pattern_type.loc16_8 (%pattern_type.3c1) = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:       %n.patt: @F.1.%pattern_type.loc16_20 (%pattern_type.7dc) = binding_pattern n [concrete]
// CHECK:STDOUT:       %n.param_patt: @F.1.%pattern_type.loc16_20 (%pattern_type.7dc) = value_param_pattern %n.patt, call_param1 [concrete]
// CHECK:STDOUT:     } {
// CHECK:STDOUT:       %self.param: @F.1.%Class (%Class) = value_param call_param0
// CHECK:STDOUT:       %.loc16_14.1: type = splice_block %Self.ref [symbolic = %Class (constants.%Class)] {
// CHECK:STDOUT:         %.loc16_14.2: type = specific_constant constants.%Class, @Class(constants.%T) [symbolic = %Class (constants.%Class)]
// CHECK:STDOUT:         %Self.ref: type = name_ref Self, %.loc16_14.2 [symbolic = %Class (constants.%Class)]
// CHECK:STDOUT:       }
// CHECK:STDOUT:       %self: @F.1.%Class (%Class) = bind_name self, %self.param
// CHECK:STDOUT:       %n.param: @F.1.%T (%T) = value_param call_param1
// CHECK:STDOUT:       %T.ref: type = name_ref T, @Class.%T.loc14_13.1 [symbolic = %T (constants.%T)]
// CHECK:STDOUT:       %n: @F.1.%T (%T) = bind_name n, %n.param
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %struct_type.a.loc17_1.1: type = struct_type {.a: %T} [symbolic = %struct_type.a.loc17_1.2 (constants.%struct_type.a)]
// CHECK:STDOUT:     %complete_type.loc17_1.1: <witness> = complete_type_witness %struct_type.a.loc17_1.1 [symbolic = %complete_type.loc17_1.2 (constants.%complete_type.f1b)]
// CHECK:STDOUT:     complete_type_witness = %complete_type.loc17_1.1
// CHECK:STDOUT:
// CHECK:STDOUT:   !members:
// CHECK:STDOUT:     .Self = constants.%Class
// CHECK:STDOUT:     .T = <poisoned>
// CHECK:STDOUT:     .a = %.loc15
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F.1(@Class.%T.loc14_13.1: type) {
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic = %T (constants.%T)]
// CHECK:STDOUT:   %Class: type = class_type @Class, @Class(%T) [symbolic = %Class (constants.%Class)]
// CHECK:STDOUT:   %pattern_type.loc16_8: type = pattern_type %Class [symbolic = %pattern_type.loc16_8 (constants.%pattern_type.3c1)]
// CHECK:STDOUT:   %pattern_type.loc16_20: type = pattern_type %T [symbolic = %pattern_type.loc16_20 (constants.%pattern_type.7dc)]
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%self.param: @F.1.%Class (%Class), %n.param: @F.1.%T (%T));
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F.2(%N.loc36_10.1: %i32) {
// CHECK:STDOUT:   %N.loc36_10.2: %i32 = bind_symbolic_name N, 0 [symbolic = %N.loc36_10.2 (constants.%N.51e)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:
// CHECK:STDOUT:   fn(%self.param: <error>, %n.param: <error>) {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     return
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Class(constants.%T) {
// CHECK:STDOUT:   %T.loc14_13.2 => constants.%T
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.1(constants.%T) {
// CHECK:STDOUT:   %T => constants.%T
// CHECK:STDOUT:   %Class => constants.%Class
// CHECK:STDOUT:   %pattern_type.loc16_8 => constants.%pattern_type.3c1
// CHECK:STDOUT:   %pattern_type.loc16_20 => constants.%pattern_type.7dc
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.2(constants.%N.51e) {
// CHECK:STDOUT:   %N.loc36_10.2 => constants.%N.51e
// CHECK:STDOUT: }
// CHECK:STDOUT:
