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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/struct/partially_const.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/struct/partially_const.carbon

fn Make(n: i32) -> {.a: i32, .b: i32, .c: i32} {
  return {.a = 0, .b = n, .c = 0};
}

// CHECK:STDOUT: --- partially_const.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.7ce: type = pattern_type %i32 [concrete]
// CHECK:STDOUT:   %struct_type.a.b.c.0b6: type = struct_type {.a: %i32, .b: %i32, .c: %i32} [concrete]
// CHECK:STDOUT:   %pattern_type.8ae: type = pattern_type %struct_type.a.b.c.0b6 [concrete]
// CHECK:STDOUT:   %Make.type: type = fn_type @Make [concrete]
// CHECK:STDOUT:   %Make: %Make.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_0.5c6: Core.IntLiteral = int_value 0 [concrete]
// CHECK:STDOUT:   %struct_type.a.b.c.1ee: type = struct_type {.a: Core.IntLiteral, .b: %i32, .c: Core.IntLiteral} [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.cc7: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.cc7 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.205: type = facet_type <@ImplicitAs, @ImplicitAs(%i32)> [concrete]
// CHECK:STDOUT:   %ImplicitAs.Convert.type.1b6: type = fn_type @ImplicitAs.Convert, @ImplicitAs(%i32) [concrete]
// CHECK:STDOUT:   %To: Core.IntLiteral = bind_symbolic_name To, 0 [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0f9: type = fn_type @Core.IntLiteral.as.ImplicitAs.impl.Convert, @Core.IntLiteral.as.ImplicitAs.impl(%To) [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.f06: %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0f9 = struct_value () [symbolic]
// CHECK:STDOUT:   %ImplicitAs.impl_witness.c75: <witness> = impl_witness imports.%ImplicitAs.impl_witness_table.a2f, @Core.IntLiteral.as.ImplicitAs.impl(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.035: type = fn_type @Core.IntLiteral.as.ImplicitAs.impl.Convert, @Core.IntLiteral.as.ImplicitAs.impl(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.956: %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.035 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.facet: %ImplicitAs.type.205 = facet_value Core.IntLiteral, (%ImplicitAs.impl_witness.c75) [concrete]
// CHECK:STDOUT:   %.9c3: type = fn_type_with_self_type %ImplicitAs.Convert.type.1b6, %ImplicitAs.facet [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound: <bound method> = bound_method %int_0.5c6, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn: <specific function> = specific_function %Core.IntLiteral.as.ImplicitAs.impl.Convert.956, @Core.IntLiteral.as.ImplicitAs.impl.Convert(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %int_0.5c6, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_0.6a9: %i32 = int_value 0 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     .ImplicitAs = %Core.ImplicitAs
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/parts/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT:   %Core.ImplicitAs: %ImplicitAs.type.cc7 = import_ref Core//prelude/parts/as, ImplicitAs, loaded [concrete = constants.%ImplicitAs.generic]
// CHECK:STDOUT:   %Core.import_ref.a5b: @Core.IntLiteral.as.ImplicitAs.impl.%Core.IntLiteral.as.ImplicitAs.impl.Convert.type (%Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0f9) = import_ref Core//prelude/parts/int, loc16_39, loaded [symbolic = @Core.IntLiteral.as.ImplicitAs.impl.%Core.IntLiteral.as.ImplicitAs.impl.Convert (constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.f06)]
// CHECK:STDOUT:   %ImplicitAs.impl_witness_table.a2f = impl_witness_table (%Core.import_ref.a5b), @Core.IntLiteral.as.ImplicitAs.impl [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Make = %Make.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Make.decl: %Make.type = fn_decl @Make [concrete = constants.%Make] {
// CHECK:STDOUT:     %n.patt: %pattern_type.7ce = binding_pattern n [concrete]
// CHECK:STDOUT:     %n.param_patt: %pattern_type.7ce = value_param_pattern %n.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.8ae = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.8ae = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32.loc15_25: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc15_25: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc15_34: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc15_34: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc15_43: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc15_43: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %struct_type.a.b.c: type = struct_type {.a: %i32, .b: %i32, .c: %i32} [concrete = constants.%struct_type.a.b.c.0b6]
// CHECK:STDOUT:     %n.param: %i32 = value_param call_param0
// CHECK:STDOUT:     %.loc15: type = splice_block %i32.loc15_12 [concrete = constants.%i32] {
// CHECK:STDOUT:       %int_32.loc15_12: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:       %i32.loc15_12: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %n: %i32 = bind_name n, %n.param
// CHECK:STDOUT:     %return.param: ref %struct_type.a.b.c.0b6 = out_param call_param1
// CHECK:STDOUT:     %return: ref %struct_type.a.b.c.0b6 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Make(%n.param: %i32) -> %return.param: %struct_type.a.b.c.0b6 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %int_0.loc16_16: Core.IntLiteral = int_value 0 [concrete = constants.%int_0.5c6]
// CHECK:STDOUT:   %n.ref: %i32 = name_ref n, %n
// CHECK:STDOUT:   %int_0.loc16_32: Core.IntLiteral = int_value 0 [concrete = constants.%int_0.5c6]
// CHECK:STDOUT:   %.loc16_33.1: %struct_type.a.b.c.1ee = struct_literal (%int_0.loc16_16, %n.ref, %int_0.loc16_32)
// CHECK:STDOUT:   %impl.elem0.loc16_33.1: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc16_33.1: <bound method> = bound_method %int_0.loc16_16, %impl.elem0.loc16_33.1 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound]
// CHECK:STDOUT:   %specific_fn.loc16_33.1: <specific function> = specific_function %impl.elem0.loc16_33.1, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc16_33.2: <bound method> = bound_method %int_0.loc16_16, %specific_fn.loc16_33.1 [concrete = constants.%bound_method]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_33.1: init %i32 = call %bound_method.loc16_33.2(%int_0.loc16_16) [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc16_33.2: init %i32 = converted %int_0.loc16_16, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_33.1 [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc16_33.3: ref %i32 = struct_access %return, element0
// CHECK:STDOUT:   %.loc16_33.4: init %i32 = initialize_from %.loc16_33.2 to %.loc16_33.3 [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc16_33.5: ref %i32 = struct_access %return, element1
// CHECK:STDOUT:   %.loc16_33.6: init %i32 = initialize_from %n.ref to %.loc16_33.5
// CHECK:STDOUT:   %impl.elem0.loc16_33.2: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc16_33.3: <bound method> = bound_method %int_0.loc16_32, %impl.elem0.loc16_33.2 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound]
// CHECK:STDOUT:   %specific_fn.loc16_33.2: <specific function> = specific_function %impl.elem0.loc16_33.2, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc16_33.4: <bound method> = bound_method %int_0.loc16_32, %specific_fn.loc16_33.2 [concrete = constants.%bound_method]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_33.2: init %i32 = call %bound_method.loc16_33.4(%int_0.loc16_32) [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc16_33.7: init %i32 = converted %int_0.loc16_32, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_33.2 [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc16_33.8: ref %i32 = struct_access %return, element2
// CHECK:STDOUT:   %.loc16_33.9: init %i32 = initialize_from %.loc16_33.7 to %.loc16_33.8 [concrete = constants.%int_0.6a9]
// CHECK:STDOUT:   %.loc16_33.10: init %struct_type.a.b.c.0b6 = struct_init (%.loc16_33.4, %.loc16_33.6, %.loc16_33.9) to %return
// CHECK:STDOUT:   %.loc16_34: init %struct_type.a.b.c.0b6 = converted %.loc16_33.1, %.loc16_33.10
// CHECK:STDOUT:   return %.loc16_34 to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
