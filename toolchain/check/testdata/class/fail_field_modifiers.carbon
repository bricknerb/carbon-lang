// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/fail_field_modifiers.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/fail_field_modifiers.carbon

class Class {

  // CHECK:STDERR: fail_field_modifiers.carbon:[[@LINE+4]]:3: error: `default` not allowed on `var` declaration [ModifierNotAllowedOnDeclaration]
  // CHECK:STDERR:   default var j: i32;
  // CHECK:STDERR:   ^~~~~~~
  // CHECK:STDERR:
  default var j: i32;

  // CHECK:STDERR: fail_field_modifiers.carbon:[[@LINE+4]]:3: error: `final` not allowed on `var` declaration [ModifierNotAllowedOnDeclaration]
  // CHECK:STDERR:   final var k: i32;
  // CHECK:STDERR:   ^~~~~
  // CHECK:STDERR:
  final var k: i32;

  // CHECK:STDERR: fail_field_modifiers.carbon:[[@LINE+4]]:3: error: `default` not allowed; requires interface scope [ModifierRequiresInterface]
  // CHECK:STDERR:   default let l: i32 = 0;
  // CHECK:STDERR:   ^~~~~~~
  // CHECK:STDERR:
  default let l: i32 = 0;

  // CHECK:STDERR: fail_field_modifiers.carbon:[[@LINE+4]]:3: error: `final` not allowed; requires interface scope [ModifierRequiresInterface]
  // CHECK:STDERR:   final let m: i32 = 1;
  // CHECK:STDERR:   ^~~~~
  // CHECK:STDERR:
  final let m: i32 = 1;
}

// CHECK:STDOUT: --- fail_field_modifiers.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Class: type = class_type @Class [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %Class.elem: type = unbound_element_type %Class, %i32 [concrete]
// CHECK:STDOUT:   %pattern_type.7ce: type = pattern_type %i32 [concrete]
// CHECK:STDOUT:   %int_0.5c6: Core.IntLiteral = int_value 0 [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.cc7: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.cc7 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.205: type = facet_type <@ImplicitAs, @ImplicitAs(%i32)> [concrete]
// CHECK:STDOUT:   %Convert.type.1b6: type = fn_type @Convert.1, @ImplicitAs(%i32) [concrete]
// CHECK:STDOUT:   %To.c80: Core.IntLiteral = bind_symbolic_name To, 0 [symbolic]
// CHECK:STDOUT:   %Convert.type.0f9: type = fn_type @Convert.2, @impl.4f9(%To.c80) [symbolic]
// CHECK:STDOUT:   %Convert.f06: %Convert.type.0f9 = struct_value () [symbolic]
// CHECK:STDOUT:   %ImplicitAs.impl_witness.c75: <witness> = impl_witness imports.%ImplicitAs.impl_witness_table.a2f, @impl.4f9(%int_32) [concrete]
// CHECK:STDOUT:   %Convert.type.035: type = fn_type @Convert.2, @impl.4f9(%int_32) [concrete]
// CHECK:STDOUT:   %Convert.956: %Convert.type.035 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.facet: %ImplicitAs.type.205 = facet_value Core.IntLiteral, (%ImplicitAs.impl_witness.c75) [concrete]
// CHECK:STDOUT:   %.9c3: type = fn_type_with_self_type %Convert.type.1b6, %ImplicitAs.facet [concrete]
// CHECK:STDOUT:   %Convert.bound.d04: <bound method> = bound_method %int_0.5c6, %Convert.956 [concrete]
// CHECK:STDOUT:   %Convert.specific_fn: <specific function> = specific_function %Convert.956, @Convert.2(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.b6e: <bound method> = bound_method %int_0.5c6, %Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_0.6a9: %i32 = int_value 0 [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %Convert.bound.ab5: <bound method> = bound_method %int_1.5b8, %Convert.956 [concrete]
// CHECK:STDOUT:   %bound_method.9a1: <bound method> = bound_method %int_1.5b8, %Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_1.5d2: %i32 = int_value 1 [concrete]
// CHECK:STDOUT:   %struct_type.j.k: type = struct_type {.j: %i32, .k: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.cf7: <witness> = complete_type_witness %struct_type.j.k [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     .ImplicitAs = %Core.ImplicitAs
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/types/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT:   %Core.ImplicitAs: %ImplicitAs.type.cc7 = import_ref Core//prelude/operators/as, ImplicitAs, loaded [concrete = constants.%ImplicitAs.generic]
// CHECK:STDOUT:   %Core.import_ref.a5b: @impl.4f9.%Convert.type (%Convert.type.0f9) = import_ref Core//prelude/types/int, loc19_39, loaded [symbolic = @impl.4f9.%Convert (constants.%Convert.f06)]
// CHECK:STDOUT:   %ImplicitAs.impl_witness_table.a2f = impl_witness_table (%Core.import_ref.a5b), @impl.4f9 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Class = %Class.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Class.decl: type = class_decl @Class [concrete = constants.%Class] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Class {
// CHECK:STDOUT:   %int_32.loc20: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32.loc20: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc20: %Class.elem = field_decl j, element0 [concrete]
// CHECK:STDOUT:   %int_32.loc26: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32.loc26: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc26: %Class.elem = field_decl k, element1 [concrete]
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %l.patt: %pattern_type.7ce = binding_pattern l [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete = constants.%int_0.5c6]
// CHECK:STDOUT:   %.loc32_18: type = splice_block %i32.loc32 [concrete = constants.%i32] {
// CHECK:STDOUT:     %int_32.loc32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %impl.elem0.loc32: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Convert.956]
// CHECK:STDOUT:   %bound_method.loc32_24.1: <bound method> = bound_method %int_0, %impl.elem0.loc32 [concrete = constants.%Convert.bound.d04]
// CHECK:STDOUT:   %specific_fn.loc32: <specific function> = specific_function %impl.elem0.loc32, @Convert.2(constants.%int_32) [concrete = constants.%Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc32_24.2: <bound method> = bound_method %int_0, %specific_fn.loc32 [concrete = constants.%bound_method.b6e]
// CHECK:STDOUT:   %int.convert_checked.loc32: init %i32 = call %bound_method.loc32_24.2(%int_0) [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc32_24.1: %i32 = value_of_initializer %int.convert_checked.loc32 [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc32_24.2: %i32 = converted %int_0, %.loc32_24.1 [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %l: %i32 = bind_name l, %.loc32_24.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %m.patt: %pattern_type.7ce = binding_pattern m [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %.loc38_16: type = splice_block %i32.loc38 [concrete = constants.%i32] {
// CHECK:STDOUT:     %int_32.loc38: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc38: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %impl.elem0.loc38: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Convert.956]
// CHECK:STDOUT:   %bound_method.loc38_22.1: <bound method> = bound_method %int_1, %impl.elem0.loc38 [concrete = constants.%Convert.bound.ab5]
// CHECK:STDOUT:   %specific_fn.loc38: <specific function> = specific_function %impl.elem0.loc38, @Convert.2(constants.%int_32) [concrete = constants.%Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc38_22.2: <bound method> = bound_method %int_1, %specific_fn.loc38 [concrete = constants.%bound_method.9a1]
// CHECK:STDOUT:   %int.convert_checked.loc38: init %i32 = call %bound_method.loc38_22.2(%int_1) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc38_22.1: %i32 = value_of_initializer %int.convert_checked.loc38 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc38_22.2: %i32 = converted %int_1, %.loc38_22.1 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %m: %i32 = bind_name m, %.loc38_22.2
// CHECK:STDOUT:   %struct_type.j.k: type = struct_type {.j: %i32, .k: %i32} [concrete = constants.%struct_type.j.k]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.j.k [concrete = constants.%complete_type.cf7]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Class
// CHECK:STDOUT:   .j = %.loc20
// CHECK:STDOUT:   .k = %.loc26
// CHECK:STDOUT:   .l = %l
// CHECK:STDOUT:   .m = %m
// CHECK:STDOUT: }
// CHECK:STDOUT:
