// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/int.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/tuple/in_place_tuple_init.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/tuple/in_place_tuple_init.carbon

// --- in_place_tuple_init.carbon
library "[[@TEST_NAME]]";

fn F() -> (i32, i32);

fn G() -> (i32, i32) {
  //@dump-sem-ir-begin
  var v: (i32, i32) = F();
  v = F();
  return F();
  //@dump-sem-ir-end
}

fn H() -> i32 {
  //@dump-sem-ir-begin
  return G().0;
  //@dump-sem-ir-end
}

// --- nested_tuple_in_place.carbon
library "[[@TEST_NAME]]";

fn F() -> (i32, i32, i32);

fn G() {
  //@dump-sem-ir-begin
  var v: ((i32, i32, i32), (i32, i32, i32)) = (F(), F());
  //@dump-sem-ir-end
}

fn H() {
  //@dump-sem-ir-begin
  var v: (i32, (i32, i32, i32), i32) = (1, F(), 2);
  //@dump-sem-ir-end
}

// CHECK:STDOUT: --- in_place_tuple_init.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %tuple.type.24b: type = tuple_type (type, type) [concrete]
// CHECK:STDOUT:   %tuple.type.d07: type = tuple_type (%i32, %i32) [concrete]
// CHECK:STDOUT:   %pattern_type.511: type = pattern_type %tuple.type.d07 [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %G.type: type = fn_type @G [concrete]
// CHECK:STDOUT:   %G: %G.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @G() -> %return.param: %tuple.type.d07 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: %pattern_type.511 = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: %pattern_type.511 = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref %tuple.type.d07 = var %v.var_patt
// CHECK:STDOUT:   %F.ref.loc7: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %.loc7_3: ref %tuple.type.d07 = splice_block %v.var {}
// CHECK:STDOUT:   %F.call.loc7: init %tuple.type.d07 = call %F.ref.loc7() to %.loc7_3
// CHECK:STDOUT:   assign %v.var, %F.call.loc7
// CHECK:STDOUT:   %.loc7_19.1: type = splice_block %.loc7_19.3 [concrete = constants.%tuple.type.d07] {
// CHECK:STDOUT:     %int_32.loc7_11: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_11: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_16: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_16: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc7_19.2: %tuple.type.24b = tuple_literal (%i32.loc7_11, %i32.loc7_16)
// CHECK:STDOUT:     %.loc7_19.3: type = converted %.loc7_19.2, constants.%tuple.type.d07 [concrete = constants.%tuple.type.d07]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: ref %tuple.type.d07 = bind_name v, %v.var
// CHECK:STDOUT:   %v.ref: ref %tuple.type.d07 = name_ref v, %v
// CHECK:STDOUT:   %F.ref.loc8: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %.loc8: ref %tuple.type.d07 = splice_block %v.ref {}
// CHECK:STDOUT:   %F.call.loc8: init %tuple.type.d07 = call %F.ref.loc8() to %.loc8
// CHECK:STDOUT:   assign %v.ref, %F.call.loc8
// CHECK:STDOUT:   %F.ref.loc9: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %F.call.loc9: init %tuple.type.d07 = call %F.ref.loc9() to %.loc5_8
// CHECK:STDOUT:   return %F.call.loc9 to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @H() -> %i32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %G.ref: %G.type = name_ref G, file.%G.decl [concrete = constants.%G]
// CHECK:STDOUT:   %.loc15_12.1: ref %tuple.type.d07 = temporary_storage
// CHECK:STDOUT:   %G.call: init %tuple.type.d07 = call %G.ref() to %.loc15_12.1
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete = constants.%int_0]
// CHECK:STDOUT:   %.loc15_12.2: ref %tuple.type.d07 = temporary %.loc15_12.1, %G.call
// CHECK:STDOUT:   %tuple.elem0: ref %i32 = tuple_access %.loc15_12.2, element0
// CHECK:STDOUT:   %.loc15_13: %i32 = bind_value %tuple.elem0
// CHECK:STDOUT:   return %.loc15_13
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- nested_tuple_in_place.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %tuple.type.ff9: type = tuple_type (type, type, type) [concrete]
// CHECK:STDOUT:   %tuple.type.189: type = tuple_type (%i32, %i32, %i32) [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple.type.f9f: type = tuple_type (%tuple.type.ff9, %tuple.type.ff9) [concrete]
// CHECK:STDOUT:   %tuple.type.99b: type = tuple_type (%tuple.type.189, %tuple.type.189) [concrete]
// CHECK:STDOUT:   %pattern_type.d88: type = pattern_type %tuple.type.99b [concrete]
// CHECK:STDOUT:   %tuple.type.3a3: type = tuple_type (type, %tuple.type.ff9, type) [concrete]
// CHECK:STDOUT:   %tuple.type.516: type = tuple_type (%i32, %tuple.type.189, %i32) [concrete]
// CHECK:STDOUT:   %pattern_type.6b5: type = pattern_type %tuple.type.516 [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %int_2.ecc: Core.IntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %tuple.type.667: type = tuple_type (Core.IntLiteral, %tuple.type.189, Core.IntLiteral) [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.205: type = facet_type <@ImplicitAs, @ImplicitAs(%i32)> [concrete]
// CHECK:STDOUT:   %Convert.type.1b6: type = fn_type @Convert.1, @ImplicitAs(%i32) [concrete]
// CHECK:STDOUT:   %To: Core.IntLiteral = bind_symbolic_name To, 0 [symbolic]
// CHECK:STDOUT:   %Convert.type.0f9: type = fn_type @Convert.2, @impl.4f9(%To) [symbolic]
// CHECK:STDOUT:   %Convert.f06: %Convert.type.0f9 = struct_value () [symbolic]
// CHECK:STDOUT:   %ImplicitAs.impl_witness.c75: <witness> = impl_witness imports.%ImplicitAs.impl_witness_table.a2f, @impl.4f9(%int_32) [concrete]
// CHECK:STDOUT:   %Convert.type.035: type = fn_type @Convert.2, @impl.4f9(%int_32) [concrete]
// CHECK:STDOUT:   %Convert.956: %Convert.type.035 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.facet: %ImplicitAs.type.205 = facet_value Core.IntLiteral, (%ImplicitAs.impl_witness.c75) [concrete]
// CHECK:STDOUT:   %.9c3: type = fn_type_with_self_type %Convert.type.1b6, %ImplicitAs.facet [concrete]
// CHECK:STDOUT:   %Convert.bound.ab5: <bound method> = bound_method %int_1.5b8, %Convert.956 [concrete]
// CHECK:STDOUT:   %Convert.specific_fn: <specific function> = specific_function %Convert.956, @Convert.2(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.9a1: <bound method> = bound_method %int_1.5b8, %Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_1.5d2: %i32 = int_value 1 [concrete]
// CHECK:STDOUT:   %Convert.bound.ef9: <bound method> = bound_method %int_2.ecc, %Convert.956 [concrete]
// CHECK:STDOUT:   %bound_method.b92: <bound method> = bound_method %int_2.ecc, %Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_2.ef8: %i32 = int_value 2 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core.import_ref.a5b: @impl.4f9.%Convert.type (%Convert.type.0f9) = import_ref Core//prelude/parts/int, loc16_39, loaded [symbolic = @impl.4f9.%Convert (constants.%Convert.f06)]
// CHECK:STDOUT:   %ImplicitAs.impl_witness_table.a2f = impl_witness_table (%Core.import_ref.a5b), @impl.4f9 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @G() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: %pattern_type.d88 = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: %pattern_type.d88 = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref %tuple.type.99b = var %v.var_patt
// CHECK:STDOUT:   %F.ref.loc7_48: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %tuple.elem0: ref %tuple.type.189 = tuple_access %v.var, element0
// CHECK:STDOUT:   %F.call.loc7_50: init %tuple.type.189 = call %F.ref.loc7_48() to %tuple.elem0
// CHECK:STDOUT:   %F.ref.loc7_53: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %tuple.elem1: ref %tuple.type.189 = tuple_access %v.var, element1
// CHECK:STDOUT:   %F.call.loc7_55: init %tuple.type.189 = call %F.ref.loc7_53() to %tuple.elem1
// CHECK:STDOUT:   %.loc7_56.1: %tuple.type.99b = tuple_literal (%F.call.loc7_50, %F.call.loc7_55)
// CHECK:STDOUT:   %.loc7_56.2: init %tuple.type.99b = tuple_init (%F.call.loc7_50, %F.call.loc7_55) to %v.var
// CHECK:STDOUT:   %.loc7_3: init %tuple.type.99b = converted %.loc7_56.1, %.loc7_56.2
// CHECK:STDOUT:   assign %v.var, %.loc7_3
// CHECK:STDOUT:   %.loc7_43.1: type = splice_block %.loc7_43.5 [concrete = constants.%tuple.type.99b] {
// CHECK:STDOUT:     %int_32.loc7_12: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_12: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_17: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_17: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_22: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_22: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc7_25: %tuple.type.ff9 = tuple_literal (%i32.loc7_12, %i32.loc7_17, %i32.loc7_22)
// CHECK:STDOUT:     %int_32.loc7_29: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_29: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_34: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_34: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc7_39: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc7_39: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc7_42: %tuple.type.ff9 = tuple_literal (%i32.loc7_29, %i32.loc7_34, %i32.loc7_39)
// CHECK:STDOUT:     %.loc7_43.2: %tuple.type.f9f = tuple_literal (%.loc7_25, %.loc7_42)
// CHECK:STDOUT:     %.loc7_43.3: type = converted %.loc7_25, constants.%tuple.type.189 [concrete = constants.%tuple.type.189]
// CHECK:STDOUT:     %.loc7_43.4: type = converted %.loc7_42, constants.%tuple.type.189 [concrete = constants.%tuple.type.189]
// CHECK:STDOUT:     %.loc7_43.5: type = converted %.loc7_43.2, constants.%tuple.type.99b [concrete = constants.%tuple.type.99b]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: ref %tuple.type.99b = bind_name v, %v.var
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @H() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: %pattern_type.6b5 = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: %pattern_type.6b5 = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref %tuple.type.516 = var %v.var_patt
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %F.ref: %F.type = name_ref F, file.%F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %tuple.elem1: ref %tuple.type.189 = tuple_access %v.var, element1
// CHECK:STDOUT:   %F.call: init %tuple.type.189 = call %F.ref() to %tuple.elem1
// CHECK:STDOUT:   %int_2: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %.loc13_50.1: %tuple.type.667 = tuple_literal (%int_1, %F.call, %int_2)
// CHECK:STDOUT:   %impl.elem0.loc13_50.1: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Convert.956]
// CHECK:STDOUT:   %bound_method.loc13_50.1: <bound method> = bound_method %int_1, %impl.elem0.loc13_50.1 [concrete = constants.%Convert.bound.ab5]
// CHECK:STDOUT:   %specific_fn.loc13_50.1: <specific function> = specific_function %impl.elem0.loc13_50.1, @Convert.2(constants.%int_32) [concrete = constants.%Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc13_50.2: <bound method> = bound_method %int_1, %specific_fn.loc13_50.1 [concrete = constants.%bound_method.9a1]
// CHECK:STDOUT:   %int.convert_checked.loc13_50.1: init %i32 = call %bound_method.loc13_50.2(%int_1) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc13_50.2: init %i32 = converted %int_1, %int.convert_checked.loc13_50.1 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %tuple.elem0: ref %i32 = tuple_access %v.var, element0
// CHECK:STDOUT:   %.loc13_50.3: init %i32 = initialize_from %.loc13_50.2 to %tuple.elem0 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %impl.elem0.loc13_50.2: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Convert.956]
// CHECK:STDOUT:   %bound_method.loc13_50.3: <bound method> = bound_method %int_2, %impl.elem0.loc13_50.2 [concrete = constants.%Convert.bound.ef9]
// CHECK:STDOUT:   %specific_fn.loc13_50.2: <specific function> = specific_function %impl.elem0.loc13_50.2, @Convert.2(constants.%int_32) [concrete = constants.%Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc13_50.4: <bound method> = bound_method %int_2, %specific_fn.loc13_50.2 [concrete = constants.%bound_method.b92]
// CHECK:STDOUT:   %int.convert_checked.loc13_50.2: init %i32 = call %bound_method.loc13_50.4(%int_2) [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc13_50.4: init %i32 = converted %int_2, %int.convert_checked.loc13_50.2 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %tuple.elem2: ref %i32 = tuple_access %v.var, element2
// CHECK:STDOUT:   %.loc13_50.5: init %i32 = initialize_from %.loc13_50.4 to %tuple.elem2 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc13_50.6: init %tuple.type.516 = tuple_init (%.loc13_50.3, %F.call, %.loc13_50.5) to %v.var
// CHECK:STDOUT:   %.loc13_3: init %tuple.type.516 = converted %.loc13_50.1, %.loc13_50.6
// CHECK:STDOUT:   assign %v.var, %.loc13_3
// CHECK:STDOUT:   %.loc13_36.1: type = splice_block %.loc13_36.4 [concrete = constants.%tuple.type.516] {
// CHECK:STDOUT:     %int_32.loc13_11: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc13_11: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc13_17: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc13_17: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc13_22: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc13_22: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc13_27: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc13_27: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc13_30: %tuple.type.ff9 = tuple_literal (%i32.loc13_17, %i32.loc13_22, %i32.loc13_27)
// CHECK:STDOUT:     %int_32.loc13_33: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc13_33: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc13_36.2: %tuple.type.3a3 = tuple_literal (%i32.loc13_11, %.loc13_30, %i32.loc13_33)
// CHECK:STDOUT:     %.loc13_36.3: type = converted %.loc13_30, constants.%tuple.type.189 [concrete = constants.%tuple.type.189]
// CHECK:STDOUT:     %.loc13_36.4: type = converted %.loc13_36.2, constants.%tuple.type.516 [concrete = constants.%tuple.type.516]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: ref %tuple.type.516 = bind_name v, %v.var
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
