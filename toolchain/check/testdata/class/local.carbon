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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/local.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/local.carbon

class A {
  fn F() -> i32 {
    class B {
      fn Make() -> Self {
        returned var b: Self = {.n = 1};
        return var;
      }

      var n: i32;
    }

    return B.Make().n;
  }
}

// CHECK:STDOUT: --- local.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %A: type = class_type @A [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.7ce: type = pattern_type %i32 [concrete]
// CHECK:STDOUT:   %A.F.type: type = fn_type @A.F [concrete]
// CHECK:STDOUT:   %A.F: %A.F.type = struct_value () [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type.357: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %B: type = class_type @B [concrete]
// CHECK:STDOUT:   %pattern_type.971: type = pattern_type %B [concrete]
// CHECK:STDOUT:   %B.Make.type: type = fn_type @B.Make [concrete]
// CHECK:STDOUT:   %B.Make: %B.Make.type = struct_value () [concrete]
// CHECK:STDOUT:   %B.elem: type = unbound_element_type %B, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.n.033: type = struct_type {.n: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.54b: <witness> = complete_type_witness %struct_type.n.033 [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %struct_type.n.44a: type = struct_type {.n: Core.IntLiteral} [concrete]
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
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn: <specific function> = specific_function %Core.IntLiteral.as.ImplicitAs.impl.Convert.956, @Core.IntLiteral.as.ImplicitAs.impl.Convert(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_1.5d2: %i32 = int_value 1 [concrete]
// CHECK:STDOUT:   %B.val: %B = struct_value (%int_1.5d2) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.96c: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%B) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.c53: %T.as.Destroy.impl.Op.type.96c = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.bac: type = ptr_type %B [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn: <specific function> = specific_function %T.as.Destroy.impl.Op.c53, @T.as.Destroy.impl.Op(%B) [concrete]
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
// CHECK:STDOUT:     .A = %A.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %A.decl: type = class_decl @A [concrete = constants.%A] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @A {
// CHECK:STDOUT:   %A.F.decl: %A.F.type = fn_decl @A.F [concrete = constants.%A.F] {
// CHECK:STDOUT:     %return.patt: %pattern_type.7ce = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.7ce = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:     %return.param: ref %i32 = out_param call_param0
// CHECK:STDOUT:     %return: ref %i32 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type.357]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%A
// CHECK:STDOUT:   .F = %A.F.decl
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @B {
// CHECK:STDOUT:   %B.Make.decl: %B.Make.type = fn_decl @B.Make [concrete = constants.%B.Make] {
// CHECK:STDOUT:     %return.patt: %pattern_type.971 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.971 = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %Self.ref.loc18: type = name_ref Self, constants.%B [concrete = constants.%B]
// CHECK:STDOUT:     %return.param: ref %B = out_param call_param0
// CHECK:STDOUT:     %return: ref %B = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc23: %B.elem = field_decl n, element0 [concrete]
// CHECK:STDOUT:   %struct_type.n: type = struct_type {.n: %i32} [concrete = constants.%struct_type.n.033]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.n [concrete = constants.%complete_type.54b]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%B
// CHECK:STDOUT:   .Make = %B.Make.decl
// CHECK:STDOUT:   .n = %.loc23
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @A.F() -> %i32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %B.decl: type = class_decl @B [concrete = constants.%B] {} {}
// CHECK:STDOUT:   %B.ref: type = name_ref B, %B.decl [concrete = constants.%B]
// CHECK:STDOUT:   %Make.ref: %B.Make.type = name_ref Make, @B.%B.Make.decl [concrete = constants.%B.Make]
// CHECK:STDOUT:   %.loc26_19.1: ref %B = temporary_storage
// CHECK:STDOUT:   %B.Make.call: init %B = call %Make.ref() to %.loc26_19.1
// CHECK:STDOUT:   %.loc26_19.2: ref %B = temporary %.loc26_19.1, %B.Make.call
// CHECK:STDOUT:   %n.ref: %B.elem = name_ref n, @B.%.loc23 [concrete = @B.%.loc23]
// CHECK:STDOUT:   %.loc26_20.1: ref %i32 = class_element_access %.loc26_19.2, element0
// CHECK:STDOUT:   %.loc26_20.2: %i32 = bind_value %.loc26_20.1
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound: <bound method> = bound_method %.loc26_19.1, constants.%T.as.Destroy.impl.Op.c53
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.c53, @T.as.Destroy.impl.Op(constants.%B) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %.loc26_19.1, %T.as.Destroy.impl.Op.specific_fn
// CHECK:STDOUT:   %addr: %ptr.bac = addr_of %.loc26_19.1
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call: init %empty_tuple.type = call %bound_method(%addr)
// CHECK:STDOUT:   return %.loc26_20.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @B.Make() -> %return.param: %B {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %b.patt: %pattern_type.971 = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.var_patt: %pattern_type.971 = var_pattern %b.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %.loc19_39.1: %struct_type.n.44a = struct_literal (%int_1)
// CHECK:STDOUT:   %impl.elem0: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc19_39.1: <bound method> = bound_method %int_1, %impl.elem0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound]
// CHECK:STDOUT:   %specific_fn: <specific function> = specific_function %impl.elem0, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc19_39.2: <bound method> = bound_method %int_1, %specific_fn [concrete = constants.%bound_method]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call: init %i32 = call %bound_method.loc19_39.2(%int_1) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc19_39.2: init %i32 = converted %int_1, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc19_39.3: ref %i32 = class_element_access %return, element0
// CHECK:STDOUT:   %.loc19_39.4: init %i32 = initialize_from %.loc19_39.2 to %.loc19_39.3 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc19_39.5: init %B = class_init (%.loc19_39.4), %return [concrete = constants.%B.val]
// CHECK:STDOUT:   %.loc19_18: init %B = converted %.loc19_39.1, %.loc19_39.5 [concrete = constants.%B.val]
// CHECK:STDOUT:   assign %return, %.loc19_18
// CHECK:STDOUT:   %Self.ref.loc19: type = name_ref Self, constants.%B [concrete = constants.%B]
// CHECK:STDOUT:   %b: ref %B = bind_name b, %return
// CHECK:STDOUT:   return %b to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
