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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/pointer/address_of_lvalue.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/pointer/address_of_lvalue.carbon

fn F() {
  var s: {.a: i32, .b: i32} = {.a = 1, .b = 2};

  var p: {.a: i32, .b: i32}* = &s;
  var q: i32* = &s.a;
  var r: i32* = &s.b;

  var t: (i32, i32) = (1, 2);
  var t0: i32* = &t.0;
  var t1: i32* = &t.1;
}

// CHECK:STDOUT: --- address_of_lvalue.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %struct_type.a.b.501: type = struct_type {.a: %i32, .b: %i32} [concrete]
// CHECK:STDOUT:   %ptr.3ee: type = ptr_type %struct_type.a.b.501 [concrete]
// CHECK:STDOUT:   %pattern_type.851: type = pattern_type %struct_type.a.b.501 [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %int_2.ecc: Core.IntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %struct_type.a.b.cfd: type = struct_type {.a: Core.IntLiteral, .b: Core.IntLiteral} [concrete]
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
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ab5: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn: <specific function> = specific_function %Core.IntLiteral.as.ImplicitAs.impl.Convert.956, @Core.IntLiteral.as.ImplicitAs.impl.Convert(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.9a1: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_1.5d2: %i32 = int_value 1 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ef9: <bound method> = bound_method %int_2.ecc, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %bound_method.b92: <bound method> = bound_method %int_2.ecc, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_2.ef8: %i32 = int_value 2 [concrete]
// CHECK:STDOUT:   %struct: %struct_type.a.b.501 = struct_value (%int_1.5d2, %int_2.ef8) [concrete]
// CHECK:STDOUT:   %pattern_type.8dd: type = pattern_type %ptr.3ee [concrete]
// CHECK:STDOUT:   %ptr.235: type = ptr_type %i32 [concrete]
// CHECK:STDOUT:   %pattern_type.fe8: type = pattern_type %ptr.235 [concrete]
// CHECK:STDOUT:   %tuple.type.24b: type = tuple_type (type, type) [concrete]
// CHECK:STDOUT:   %tuple.type.d07: type = tuple_type (%i32, %i32) [concrete]
// CHECK:STDOUT:   %ptr.261: type = ptr_type %tuple.type.d07 [concrete]
// CHECK:STDOUT:   %pattern_type.511: type = pattern_type %tuple.type.d07 [concrete]
// CHECK:STDOUT:   %tuple.type.f94: type = tuple_type (Core.IntLiteral, Core.IntLiteral) [concrete]
// CHECK:STDOUT:   %tuple: %tuple.type.d07 = tuple_value (%int_1.5d2, %int_2.ef8) [concrete]
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.ba2: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%ptr.235) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.649: %T.as.Destroy.impl.Op.type.ba2 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.5d5: type = ptr_type %ptr.235 [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.829: <specific function> = specific_function %T.as.Destroy.impl.Op.649, @T.as.Destroy.impl.Op(%ptr.235) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.a4a: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%tuple.type.d07) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.7c6: %T.as.Destroy.impl.Op.type.a4a = struct_value () [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.bbb: <specific function> = specific_function %T.as.Destroy.impl.Op.7c6, @T.as.Destroy.impl.Op(%tuple.type.d07) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.757: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%ptr.3ee) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.e22: %T.as.Destroy.impl.Op.type.757 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.40c: type = ptr_type %ptr.3ee [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.ce1: <specific function> = specific_function %T.as.Destroy.impl.Op.e22, @T.as.Destroy.impl.Op(%ptr.3ee) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.da3: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%struct_type.a.b.501) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.9db: %T.as.Destroy.impl.Op.type.da3 = struct_value () [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.1e0: <specific function> = specific_function %T.as.Destroy.impl.Op.9db, @T.as.Destroy.impl.Op(%struct_type.a.b.501) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     .ImplicitAs = %Core.ImplicitAs
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude/parts/int, Int, loaded [concrete = constants.%Int.generic]
// CHECK:STDOUT:   %Core.ImplicitAs: %ImplicitAs.type.cc7 = import_ref Core//prelude/parts/as, ImplicitAs, loaded [concrete = constants.%ImplicitAs.generic]
// CHECK:STDOUT:   %Core.import_ref.a5b: @Core.IntLiteral.as.ImplicitAs.impl.%Core.IntLiteral.as.ImplicitAs.impl.Convert.type (%Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0f9) = import_ref Core//prelude/parts/int, loc16_39, loaded [symbolic = @Core.IntLiteral.as.ImplicitAs.impl.%Core.IntLiteral.as.ImplicitAs.impl.Convert (constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.f06)]
// CHECK:STDOUT:   %ImplicitAs.impl_witness_table.a2f = impl_witness_table (%Core.import_ref.a5b), @Core.IntLiteral.as.ImplicitAs.impl [concrete]
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %s.patt: %pattern_type.851 = binding_pattern s [concrete]
// CHECK:STDOUT:     %s.var_patt: %pattern_type.851 = var_pattern %s.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %s.var: ref %struct_type.a.b.501 = var %s.var_patt
// CHECK:STDOUT:   %int_1.loc16: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_2.loc16: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %.loc16_46.1: %struct_type.a.b.cfd = struct_literal (%int_1.loc16, %int_2.loc16)
// CHECK:STDOUT:   %impl.elem0.loc16_46.1: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc16_46.1: <bound method> = bound_method %int_1.loc16, %impl.elem0.loc16_46.1 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ab5]
// CHECK:STDOUT:   %specific_fn.loc16_46.1: <specific function> = specific_function %impl.elem0.loc16_46.1, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc16_46.2: <bound method> = bound_method %int_1.loc16, %specific_fn.loc16_46.1 [concrete = constants.%bound_method.9a1]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_46.1: init %i32 = call %bound_method.loc16_46.2(%int_1.loc16) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc16_46.2: init %i32 = converted %int_1.loc16, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_46.1 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc16_46.3: ref %i32 = struct_access %s.var, element0
// CHECK:STDOUT:   %.loc16_46.4: init %i32 = initialize_from %.loc16_46.2 to %.loc16_46.3 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %impl.elem0.loc16_46.2: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc16_46.3: <bound method> = bound_method %int_2.loc16, %impl.elem0.loc16_46.2 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ef9]
// CHECK:STDOUT:   %specific_fn.loc16_46.2: <specific function> = specific_function %impl.elem0.loc16_46.2, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc16_46.4: <bound method> = bound_method %int_2.loc16, %specific_fn.loc16_46.2 [concrete = constants.%bound_method.b92]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_46.2: init %i32 = call %bound_method.loc16_46.4(%int_2.loc16) [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc16_46.5: init %i32 = converted %int_2.loc16, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc16_46.2 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc16_46.6: ref %i32 = struct_access %s.var, element1
// CHECK:STDOUT:   %.loc16_46.7: init %i32 = initialize_from %.loc16_46.5 to %.loc16_46.6 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc16_46.8: init %struct_type.a.b.501 = struct_init (%.loc16_46.4, %.loc16_46.7) to %s.var [concrete = constants.%struct]
// CHECK:STDOUT:   %.loc16_3: init %struct_type.a.b.501 = converted %.loc16_46.1, %.loc16_46.8 [concrete = constants.%struct]
// CHECK:STDOUT:   assign %s.var, %.loc16_3
// CHECK:STDOUT:   %.loc16_27: type = splice_block %struct_type.a.b.loc16 [concrete = constants.%struct_type.a.b.501] {
// CHECK:STDOUT:     %int_32.loc16_15: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc16_15: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc16_24: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc16_24: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %struct_type.a.b.loc16: type = struct_type {.a: %i32, .b: %i32} [concrete = constants.%struct_type.a.b.501]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %s: ref %struct_type.a.b.501 = bind_name s, %s.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %p.patt: %pattern_type.8dd = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.var_patt: %pattern_type.8dd = var_pattern %p.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %p.var: ref %ptr.3ee = var %p.var_patt
// CHECK:STDOUT:   %s.ref.loc18: ref %struct_type.a.b.501 = name_ref s, %s
// CHECK:STDOUT:   %addr.loc18_32: %ptr.3ee = addr_of %s.ref.loc18
// CHECK:STDOUT:   assign %p.var, %addr.loc18_32
// CHECK:STDOUT:   %.loc18: type = splice_block %ptr.loc18 [concrete = constants.%ptr.3ee] {
// CHECK:STDOUT:     %int_32.loc18_15: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc18_15: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc18_24: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc18_24: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %struct_type.a.b.loc18: type = struct_type {.a: %i32, .b: %i32} [concrete = constants.%struct_type.a.b.501]
// CHECK:STDOUT:     %ptr.loc18: type = ptr_type %struct_type.a.b.loc18 [concrete = constants.%ptr.3ee]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %p: ref %ptr.3ee = bind_name p, %p.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %q.patt: %pattern_type.fe8 = binding_pattern q [concrete]
// CHECK:STDOUT:     %q.var_patt: %pattern_type.fe8 = var_pattern %q.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %q.var: ref %ptr.235 = var %q.var_patt
// CHECK:STDOUT:   %s.ref.loc19: ref %struct_type.a.b.501 = name_ref s, %s
// CHECK:STDOUT:   %.loc19_19: ref %i32 = struct_access %s.ref.loc19, element0
// CHECK:STDOUT:   %addr.loc19_17: %ptr.235 = addr_of %.loc19_19
// CHECK:STDOUT:   assign %q.var, %addr.loc19_17
// CHECK:STDOUT:   %.loc19_13: type = splice_block %ptr.loc19 [concrete = constants.%ptr.235] {
// CHECK:STDOUT:     %int_32.loc19: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc19: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %ptr.loc19: type = ptr_type %i32.loc19 [concrete = constants.%ptr.235]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %q: ref %ptr.235 = bind_name q, %q.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %r.patt: %pattern_type.fe8 = binding_pattern r [concrete]
// CHECK:STDOUT:     %r.var_patt: %pattern_type.fe8 = var_pattern %r.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %r.var: ref %ptr.235 = var %r.var_patt
// CHECK:STDOUT:   %s.ref.loc20: ref %struct_type.a.b.501 = name_ref s, %s
// CHECK:STDOUT:   %.loc20_19: ref %i32 = struct_access %s.ref.loc20, element1
// CHECK:STDOUT:   %addr.loc20_17: %ptr.235 = addr_of %.loc20_19
// CHECK:STDOUT:   assign %r.var, %addr.loc20_17
// CHECK:STDOUT:   %.loc20_13: type = splice_block %ptr.loc20 [concrete = constants.%ptr.235] {
// CHECK:STDOUT:     %int_32.loc20: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc20: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %ptr.loc20: type = ptr_type %i32.loc20 [concrete = constants.%ptr.235]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %r: ref %ptr.235 = bind_name r, %r.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %t.patt: %pattern_type.511 = binding_pattern t [concrete]
// CHECK:STDOUT:     %t.var_patt: %pattern_type.511 = var_pattern %t.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %t.var: ref %tuple.type.d07 = var %t.var_patt
// CHECK:STDOUT:   %int_1.loc22: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_2.loc22: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %.loc22_28.1: %tuple.type.f94 = tuple_literal (%int_1.loc22, %int_2.loc22)
// CHECK:STDOUT:   %impl.elem0.loc22_28.1: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc22_28.1: <bound method> = bound_method %int_1.loc22, %impl.elem0.loc22_28.1 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ab5]
// CHECK:STDOUT:   %specific_fn.loc22_28.1: <specific function> = specific_function %impl.elem0.loc22_28.1, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc22_28.2: <bound method> = bound_method %int_1.loc22, %specific_fn.loc22_28.1 [concrete = constants.%bound_method.9a1]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc22_28.1: init %i32 = call %bound_method.loc22_28.2(%int_1.loc22) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc22_28.2: init %i32 = converted %int_1.loc22, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc22_28.1 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %tuple.elem0.loc22: ref %i32 = tuple_access %t.var, element0
// CHECK:STDOUT:   %.loc22_28.3: init %i32 = initialize_from %.loc22_28.2 to %tuple.elem0.loc22 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %impl.elem0.loc22_28.2: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc22_28.3: <bound method> = bound_method %int_2.loc22, %impl.elem0.loc22_28.2 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ef9]
// CHECK:STDOUT:   %specific_fn.loc22_28.2: <specific function> = specific_function %impl.elem0.loc22_28.2, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc22_28.4: <bound method> = bound_method %int_2.loc22, %specific_fn.loc22_28.2 [concrete = constants.%bound_method.b92]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc22_28.2: init %i32 = call %bound_method.loc22_28.4(%int_2.loc22) [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc22_28.4: init %i32 = converted %int_2.loc22, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc22_28.2 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %tuple.elem1.loc22: ref %i32 = tuple_access %t.var, element1
// CHECK:STDOUT:   %.loc22_28.5: init %i32 = initialize_from %.loc22_28.4 to %tuple.elem1.loc22 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc22_28.6: init %tuple.type.d07 = tuple_init (%.loc22_28.3, %.loc22_28.5) to %t.var [concrete = constants.%tuple]
// CHECK:STDOUT:   %.loc22_3: init %tuple.type.d07 = converted %.loc22_28.1, %.loc22_28.6 [concrete = constants.%tuple]
// CHECK:STDOUT:   assign %t.var, %.loc22_3
// CHECK:STDOUT:   %.loc22_19.1: type = splice_block %.loc22_19.3 [concrete = constants.%tuple.type.d07] {
// CHECK:STDOUT:     %int_32.loc22_11: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc22_11: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %int_32.loc22_16: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc22_16: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %.loc22_19.2: %tuple.type.24b = tuple_literal (%i32.loc22_11, %i32.loc22_16)
// CHECK:STDOUT:     %.loc22_19.3: type = converted %.loc22_19.2, constants.%tuple.type.d07 [concrete = constants.%tuple.type.d07]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %t: ref %tuple.type.d07 = bind_name t, %t.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %t0.patt: %pattern_type.fe8 = binding_pattern t0 [concrete]
// CHECK:STDOUT:     %t0.var_patt: %pattern_type.fe8 = var_pattern %t0.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %t0.var: ref %ptr.235 = var %t0.var_patt
// CHECK:STDOUT:   %t.ref.loc23: ref %tuple.type.d07 = name_ref t, %t
// CHECK:STDOUT:   %int_0: Core.IntLiteral = int_value 0 [concrete = constants.%int_0]
// CHECK:STDOUT:   %tuple.elem0.loc23: ref %i32 = tuple_access %t.ref.loc23, element0
// CHECK:STDOUT:   %addr.loc23_18: %ptr.235 = addr_of %tuple.elem0.loc23
// CHECK:STDOUT:   assign %t0.var, %addr.loc23_18
// CHECK:STDOUT:   %.loc23: type = splice_block %ptr.loc23 [concrete = constants.%ptr.235] {
// CHECK:STDOUT:     %int_32.loc23: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc23: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %ptr.loc23: type = ptr_type %i32.loc23 [concrete = constants.%ptr.235]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %t0: ref %ptr.235 = bind_name t0, %t0.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %t1.patt: %pattern_type.fe8 = binding_pattern t1 [concrete]
// CHECK:STDOUT:     %t1.var_patt: %pattern_type.fe8 = var_pattern %t1.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %t1.var: ref %ptr.235 = var %t1.var_patt
// CHECK:STDOUT:   %t.ref.loc24: ref %tuple.type.d07 = name_ref t, %t
// CHECK:STDOUT:   %int_1.loc24: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %tuple.elem1.loc24: ref %i32 = tuple_access %t.ref.loc24, element1
// CHECK:STDOUT:   %addr.loc24_18: %ptr.235 = addr_of %tuple.elem1.loc24
// CHECK:STDOUT:   assign %t1.var, %addr.loc24_18
// CHECK:STDOUT:   %.loc24: type = splice_block %ptr.loc24 [concrete = constants.%ptr.235] {
// CHECK:STDOUT:     %int_32.loc24: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc24: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %ptr.loc24: type = ptr_type %i32.loc24 [concrete = constants.%ptr.235]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %t1: ref %ptr.235 = bind_name t1, %t1.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc24: <bound method> = bound_method %t1.var, constants.%T.as.Destroy.impl.Op.649
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.1: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.649, @T.as.Destroy.impl.Op(constants.%ptr.235) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.829]
// CHECK:STDOUT:   %bound_method.loc24: <bound method> = bound_method %t1.var, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc24_3: %ptr.5d5 = addr_of %t1.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc24: init %empty_tuple.type = call %bound_method.loc24(%addr.loc24_3)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc23: <bound method> = bound_method %t0.var, constants.%T.as.Destroy.impl.Op.649
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.2: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.649, @T.as.Destroy.impl.Op(constants.%ptr.235) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.829]
// CHECK:STDOUT:   %bound_method.loc23: <bound method> = bound_method %t0.var, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc23_3: %ptr.5d5 = addr_of %t0.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc23: init %empty_tuple.type = call %bound_method.loc23(%addr.loc23_3)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc22: <bound method> = bound_method %t.var, constants.%T.as.Destroy.impl.Op.7c6
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.3: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.7c6, @T.as.Destroy.impl.Op(constants.%tuple.type.d07) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.bbb]
// CHECK:STDOUT:   %bound_method.loc22_3: <bound method> = bound_method %t.var, %T.as.Destroy.impl.Op.specific_fn.3
// CHECK:STDOUT:   %addr.loc22: %ptr.261 = addr_of %t.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc22: init %empty_tuple.type = call %bound_method.loc22_3(%addr.loc22)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc20: <bound method> = bound_method %r.var, constants.%T.as.Destroy.impl.Op.649
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.4: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.649, @T.as.Destroy.impl.Op(constants.%ptr.235) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.829]
// CHECK:STDOUT:   %bound_method.loc20: <bound method> = bound_method %r.var, %T.as.Destroy.impl.Op.specific_fn.4
// CHECK:STDOUT:   %addr.loc20_3: %ptr.5d5 = addr_of %r.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc20: init %empty_tuple.type = call %bound_method.loc20(%addr.loc20_3)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc19: <bound method> = bound_method %q.var, constants.%T.as.Destroy.impl.Op.649
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.5: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.649, @T.as.Destroy.impl.Op(constants.%ptr.235) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.829]
// CHECK:STDOUT:   %bound_method.loc19: <bound method> = bound_method %q.var, %T.as.Destroy.impl.Op.specific_fn.5
// CHECK:STDOUT:   %addr.loc19_3: %ptr.5d5 = addr_of %q.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc19: init %empty_tuple.type = call %bound_method.loc19(%addr.loc19_3)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc18: <bound method> = bound_method %p.var, constants.%T.as.Destroy.impl.Op.e22
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.6: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.e22, @T.as.Destroy.impl.Op(constants.%ptr.3ee) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.ce1]
// CHECK:STDOUT:   %bound_method.loc18: <bound method> = bound_method %p.var, %T.as.Destroy.impl.Op.specific_fn.6
// CHECK:STDOUT:   %addr.loc18_3: %ptr.40c = addr_of %p.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc18: init %empty_tuple.type = call %bound_method.loc18(%addr.loc18_3)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc16: <bound method> = bound_method %s.var, constants.%T.as.Destroy.impl.Op.9db
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.7: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.9db, @T.as.Destroy.impl.Op(constants.%struct_type.a.b.501) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.1e0]
// CHECK:STDOUT:   %bound_method.loc16_3: <bound method> = bound_method %s.var, %T.as.Destroy.impl.Op.specific_fn.7
// CHECK:STDOUT:   %addr.loc16: %ptr.3ee = addr_of %s.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc16: init %empty_tuple.type = call %bound_method.loc16_3(%addr.loc16)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
