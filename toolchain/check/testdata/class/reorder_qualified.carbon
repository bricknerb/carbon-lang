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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/reorder_qualified.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/reorder_qualified.carbon

class A {
  class B {
    class C;

    fn BF();
    var b: i32;
  }

  class B.C {
    class D {
      fn F();

      fn DF();
      var d: i32;
    }

    fn D.DF() {
      // A, B, C, and D are complete here.
      var a: A = {.a = 1};
      var b: B = {.b = 2};
      var c: C = {.c = 3};
      var d: D = {.d = 4};

      // Unqualified lookup looks in all of them.
      AF();
      BF();
      CF();
      DF();
    }

    fn CF();
    var c: i32;
  }

  fn AF();
  var a: i32;
}

// CHECK:STDOUT: --- reorder_qualified.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %A: type = class_type @A [concrete]
// CHECK:STDOUT:   %B: type = class_type @B [concrete]
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %B.BF.type: type = fn_type @B.BF [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %B.BF: %B.BF.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %B.elem: type = unbound_element_type %B, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.b.0a3: type = struct_type {.b: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.ba8: <witness> = complete_type_witness %struct_type.b.0a3 [concrete]
// CHECK:STDOUT:   %D: type = class_type @D [concrete]
// CHECK:STDOUT:   %D.F.type: type = fn_type @D.F [concrete]
// CHECK:STDOUT:   %D.F: %D.F.type = struct_value () [concrete]
// CHECK:STDOUT:   %D.DF.type: type = fn_type @D.DF [concrete]
// CHECK:STDOUT:   %D.DF: %D.DF.type = struct_value () [concrete]
// CHECK:STDOUT:   %D.elem: type = unbound_element_type %D, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.d.b7b: type = struct_type {.d: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.860: <witness> = complete_type_witness %struct_type.d.b7b [concrete]
// CHECK:STDOUT:   %C.CF.type: type = fn_type @C.CF [concrete]
// CHECK:STDOUT:   %C.CF: %C.CF.type = struct_value () [concrete]
// CHECK:STDOUT:   %C.elem: type = unbound_element_type %C, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.c.b66: type = struct_type {.c: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.836: <witness> = complete_type_witness %struct_type.c.b66 [concrete]
// CHECK:STDOUT:   %A.AF.type: type = fn_type @A.AF [concrete]
// CHECK:STDOUT:   %A.AF: %A.AF.type = struct_value () [concrete]
// CHECK:STDOUT:   %A.elem: type = unbound_element_type %A, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.a.ba9: type = struct_type {.a: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.fd7: <witness> = complete_type_witness %struct_type.a.ba9 [concrete]
// CHECK:STDOUT:   %pattern_type.c10: type = pattern_type %A [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %struct_type.a.a6c: type = struct_type {.a: Core.IntLiteral} [concrete]
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
// CHECK:STDOUT:   %A.val: %A = struct_value (%int_1.5d2) [concrete]
// CHECK:STDOUT:   %pattern_type.98e: type = pattern_type %B [concrete]
// CHECK:STDOUT:   %int_2.ecc: Core.IntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %struct_type.b.a15: type = struct_type {.b: Core.IntLiteral} [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ef9: <bound method> = bound_method %int_2.ecc, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %bound_method.b92: <bound method> = bound_method %int_2.ecc, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_2.ef8: %i32 = int_value 2 [concrete]
// CHECK:STDOUT:   %B.val: %B = struct_value (%int_2.ef8) [concrete]
// CHECK:STDOUT:   %pattern_type.6d8: type = pattern_type %C [concrete]
// CHECK:STDOUT:   %int_3.1ba: Core.IntLiteral = int_value 3 [concrete]
// CHECK:STDOUT:   %struct_type.c.5b8: type = struct_type {.c: Core.IntLiteral} [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.b30: <bound method> = bound_method %int_3.1ba, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %bound_method.047: <bound method> = bound_method %int_3.1ba, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_3.822: %i32 = int_value 3 [concrete]
// CHECK:STDOUT:   %C.val: %C = struct_value (%int_3.822) [concrete]
// CHECK:STDOUT:   %pattern_type.d58: type = pattern_type %D [concrete]
// CHECK:STDOUT:   %int_4.0c1: Core.IntLiteral = int_value 4 [concrete]
// CHECK:STDOUT:   %struct_type.d.3ea: type = struct_type {.d: Core.IntLiteral} [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ac3: <bound method> = bound_method %int_4.0c1, %Core.IntLiteral.as.ImplicitAs.impl.Convert.956 [concrete]
// CHECK:STDOUT:   %bound_method.1da: <bound method> = bound_method %int_4.0c1, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_4.940: %i32 = int_value 4 [concrete]
// CHECK:STDOUT:   %D.val: %D = struct_value (%int_4.940) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.7e3: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%D) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.47c: %T.as.Destroy.impl.Op.type.7e3 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.321: type = ptr_type %D [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.ceb: <specific function> = specific_function %T.as.Destroy.impl.Op.47c, @T.as.Destroy.impl.Op(%D) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.2eb: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%C) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.f77: %T.as.Destroy.impl.Op.type.2eb = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.388: type = ptr_type %C [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.c65: <specific function> = specific_function %T.as.Destroy.impl.Op.f77, @T.as.Destroy.impl.Op(%C) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.266: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%B) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.4a4: %T.as.Destroy.impl.Op.type.266 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.01b: type = ptr_type %B [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.70c: <specific function> = specific_function %T.as.Destroy.impl.Op.4a4, @T.as.Destroy.impl.Op(%B) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.b96: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%A) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.885: %T.as.Destroy.impl.Op.type.b96 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.6db: type = ptr_type %A [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.716: <specific function> = specific_function %T.as.Destroy.impl.Op.885, @T.as.Destroy.impl.Op(%A) [concrete]
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
// CHECK:STDOUT:   %B.decl: type = class_decl @B [concrete = constants.%B] {} {}
// CHECK:STDOUT:   %C.decl: type = class_decl @C [concrete = constants.%C] {} {}
// CHECK:STDOUT:   %A.AF.decl: %A.AF.type = fn_decl @A.AF [concrete = constants.%A.AF] {} {}
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc50: %A.elem = field_decl a, element0 [concrete]
// CHECK:STDOUT:   %struct_type.a: type = struct_type {.a: %i32} [concrete = constants.%struct_type.a.ba9]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.a [concrete = constants.%complete_type.fd7]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%A
// CHECK:STDOUT:   .B = %B.decl
// CHECK:STDOUT:   .AF = %A.AF.decl
// CHECK:STDOUT:   .a = %.loc50
// CHECK:STDOUT:   .A = <poisoned>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @B {
// CHECK:STDOUT:   %C.decl: type = class_decl @C [concrete = constants.%C] {} {}
// CHECK:STDOUT:   %B.BF.decl: %B.BF.type = fn_decl @B.BF [concrete = constants.%B.BF] {} {}
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc20: %B.elem = field_decl b, element0 [concrete]
// CHECK:STDOUT:   %struct_type.b: type = struct_type {.b: %i32} [concrete = constants.%struct_type.b.0a3]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.b [concrete = constants.%complete_type.ba8]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%B
// CHECK:STDOUT:   .C = %C.decl
// CHECK:STDOUT:   .BF = %B.BF.decl
// CHECK:STDOUT:   .b = %.loc20
// CHECK:STDOUT:   .A = <poisoned>
// CHECK:STDOUT:   .B = <poisoned>
// CHECK:STDOUT:   .AF = <poisoned>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %D.decl: type = class_decl @D [concrete = constants.%D] {} {}
// CHECK:STDOUT:   %D.DF.decl: %D.DF.type = fn_decl @D.DF [concrete = constants.%D.DF] {} {}
// CHECK:STDOUT:   %C.CF.decl: %C.CF.type = fn_decl @C.CF [concrete = constants.%C.CF] {} {}
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc46: %C.elem = field_decl c, element0 [concrete]
// CHECK:STDOUT:   %struct_type.c: type = struct_type {.c: %i32} [concrete = constants.%struct_type.c.b66]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.c [concrete = constants.%complete_type.836]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   .D = %D.decl
// CHECK:STDOUT:   .CF = %C.CF.decl
// CHECK:STDOUT:   .c = %.loc46
// CHECK:STDOUT:   .A = <poisoned>
// CHECK:STDOUT:   .B = <poisoned>
// CHECK:STDOUT:   .C = <poisoned>
// CHECK:STDOUT:   .AF = <poisoned>
// CHECK:STDOUT:   .BF = <poisoned>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @D {
// CHECK:STDOUT:   %D.F.decl: %D.F.type = fn_decl @D.F [concrete = constants.%D.F] {} {}
// CHECK:STDOUT:   %D.DF.decl: %D.DF.type = fn_decl @D.DF [concrete = constants.%D.DF] {} {}
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc28: %D.elem = field_decl d, element0 [concrete]
// CHECK:STDOUT:   %struct_type.d: type = struct_type {.d: %i32} [concrete = constants.%struct_type.d.b7b]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.d [concrete = constants.%complete_type.860]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%D
// CHECK:STDOUT:   .F = %D.F.decl
// CHECK:STDOUT:   .DF = %D.DF.decl
// CHECK:STDOUT:   .d = %.loc28
// CHECK:STDOUT:   .A = <poisoned>
// CHECK:STDOUT:   .B = <poisoned>
// CHECK:STDOUT:   .C = <poisoned>
// CHECK:STDOUT:   .D = <poisoned>
// CHECK:STDOUT:   .AF = <poisoned>
// CHECK:STDOUT:   .BF = <poisoned>
// CHECK:STDOUT:   .CF = <poisoned>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @B.BF();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @D.F();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @D.DF() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %a.patt: %pattern_type.c10 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.var_patt: %pattern_type.c10 = var_pattern %a.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a.var: ref %A = var %a.var_patt
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %.loc33_25.1: %struct_type.a.a6c = struct_literal (%int_1)
// CHECK:STDOUT:   %impl.elem0.loc33: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc33_25.1: <bound method> = bound_method %int_1, %impl.elem0.loc33 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ab5]
// CHECK:STDOUT:   %specific_fn.loc33: <specific function> = specific_function %impl.elem0.loc33, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc33_25.2: <bound method> = bound_method %int_1, %specific_fn.loc33 [concrete = constants.%bound_method.9a1]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc33: init %i32 = call %bound_method.loc33_25.2(%int_1) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc33_25.2: init %i32 = converted %int_1, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc33 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc33_25.3: ref %i32 = class_element_access %a.var, element0
// CHECK:STDOUT:   %.loc33_25.4: init %i32 = initialize_from %.loc33_25.2 to %.loc33_25.3 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc33_25.5: init %A = class_init (%.loc33_25.4), %a.var [concrete = constants.%A.val]
// CHECK:STDOUT:   %.loc33_7: init %A = converted %.loc33_25.1, %.loc33_25.5 [concrete = constants.%A.val]
// CHECK:STDOUT:   assign %a.var, %.loc33_7
// CHECK:STDOUT:   %A.ref: type = name_ref A, file.%A.decl [concrete = constants.%A]
// CHECK:STDOUT:   %a: ref %A = bind_name a, %a.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %b.patt: %pattern_type.98e = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.var_patt: %pattern_type.98e = var_pattern %b.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b.var: ref %B = var %b.var_patt
// CHECK:STDOUT:   %int_2: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %.loc34_25.1: %struct_type.b.a15 = struct_literal (%int_2)
// CHECK:STDOUT:   %impl.elem0.loc34: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc34_25.1: <bound method> = bound_method %int_2, %impl.elem0.loc34 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ef9]
// CHECK:STDOUT:   %specific_fn.loc34: <specific function> = specific_function %impl.elem0.loc34, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc34_25.2: <bound method> = bound_method %int_2, %specific_fn.loc34 [concrete = constants.%bound_method.b92]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc34: init %i32 = call %bound_method.loc34_25.2(%int_2) [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc34_25.2: init %i32 = converted %int_2, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc34 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc34_25.3: ref %i32 = class_element_access %b.var, element0
// CHECK:STDOUT:   %.loc34_25.4: init %i32 = initialize_from %.loc34_25.2 to %.loc34_25.3 [concrete = constants.%int_2.ef8]
// CHECK:STDOUT:   %.loc34_25.5: init %B = class_init (%.loc34_25.4), %b.var [concrete = constants.%B.val]
// CHECK:STDOUT:   %.loc34_7: init %B = converted %.loc34_25.1, %.loc34_25.5 [concrete = constants.%B.val]
// CHECK:STDOUT:   assign %b.var, %.loc34_7
// CHECK:STDOUT:   %B.ref: type = name_ref B, @A.%B.decl [concrete = constants.%B]
// CHECK:STDOUT:   %b: ref %B = bind_name b, %b.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %c.patt: %pattern_type.6d8 = binding_pattern c [concrete]
// CHECK:STDOUT:     %c.var_patt: %pattern_type.6d8 = var_pattern %c.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c.var: ref %C = var %c.var_patt
// CHECK:STDOUT:   %int_3: Core.IntLiteral = int_value 3 [concrete = constants.%int_3.1ba]
// CHECK:STDOUT:   %.loc35_25.1: %struct_type.c.5b8 = struct_literal (%int_3)
// CHECK:STDOUT:   %impl.elem0.loc35: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc35_25.1: <bound method> = bound_method %int_3, %impl.elem0.loc35 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.b30]
// CHECK:STDOUT:   %specific_fn.loc35: <specific function> = specific_function %impl.elem0.loc35, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc35_25.2: <bound method> = bound_method %int_3, %specific_fn.loc35 [concrete = constants.%bound_method.047]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc35: init %i32 = call %bound_method.loc35_25.2(%int_3) [concrete = constants.%int_3.822]
// CHECK:STDOUT:   %.loc35_25.2: init %i32 = converted %int_3, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc35 [concrete = constants.%int_3.822]
// CHECK:STDOUT:   %.loc35_25.3: ref %i32 = class_element_access %c.var, element0
// CHECK:STDOUT:   %.loc35_25.4: init %i32 = initialize_from %.loc35_25.2 to %.loc35_25.3 [concrete = constants.%int_3.822]
// CHECK:STDOUT:   %.loc35_25.5: init %C = class_init (%.loc35_25.4), %c.var [concrete = constants.%C.val]
// CHECK:STDOUT:   %.loc35_7: init %C = converted %.loc35_25.1, %.loc35_25.5 [concrete = constants.%C.val]
// CHECK:STDOUT:   assign %c.var, %.loc35_7
// CHECK:STDOUT:   %C.ref: type = name_ref C, @B.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:   %c: ref %C = bind_name c, %c.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %d.patt: %pattern_type.d58 = binding_pattern d [concrete]
// CHECK:STDOUT:     %d.var_patt: %pattern_type.d58 = var_pattern %d.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %d.var: ref %D = var %d.var_patt
// CHECK:STDOUT:   %int_4: Core.IntLiteral = int_value 4 [concrete = constants.%int_4.0c1]
// CHECK:STDOUT:   %.loc36_25.1: %struct_type.d.3ea = struct_literal (%int_4)
// CHECK:STDOUT:   %impl.elem0.loc36: %.9c3 = impl_witness_access constants.%ImplicitAs.impl_witness.c75, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.956]
// CHECK:STDOUT:   %bound_method.loc36_25.1: <bound method> = bound_method %int_4, %impl.elem0.loc36 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.ac3]
// CHECK:STDOUT:   %specific_fn.loc36: <specific function> = specific_function %impl.elem0.loc36, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc36_25.2: <bound method> = bound_method %int_4, %specific_fn.loc36 [concrete = constants.%bound_method.1da]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc36: init %i32 = call %bound_method.loc36_25.2(%int_4) [concrete = constants.%int_4.940]
// CHECK:STDOUT:   %.loc36_25.2: init %i32 = converted %int_4, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc36 [concrete = constants.%int_4.940]
// CHECK:STDOUT:   %.loc36_25.3: ref %i32 = class_element_access %d.var, element0
// CHECK:STDOUT:   %.loc36_25.4: init %i32 = initialize_from %.loc36_25.2 to %.loc36_25.3 [concrete = constants.%int_4.940]
// CHECK:STDOUT:   %.loc36_25.5: init %D = class_init (%.loc36_25.4), %d.var [concrete = constants.%D.val]
// CHECK:STDOUT:   %.loc36_7: init %D = converted %.loc36_25.1, %.loc36_25.5 [concrete = constants.%D.val]
// CHECK:STDOUT:   assign %d.var, %.loc36_7
// CHECK:STDOUT:   %D.ref: type = name_ref D, @C.%D.decl [concrete = constants.%D]
// CHECK:STDOUT:   %d: ref %D = bind_name d, %d.var
// CHECK:STDOUT:   %AF.ref: %A.AF.type = name_ref AF, @A.%A.AF.decl [concrete = constants.%A.AF]
// CHECK:STDOUT:   %A.AF.call: init %empty_tuple.type = call %AF.ref()
// CHECK:STDOUT:   %BF.ref: %B.BF.type = name_ref BF, @B.%B.BF.decl [concrete = constants.%B.BF]
// CHECK:STDOUT:   %B.BF.call: init %empty_tuple.type = call %BF.ref()
// CHECK:STDOUT:   %CF.ref: %C.CF.type = name_ref CF, @C.%C.CF.decl [concrete = constants.%C.CF]
// CHECK:STDOUT:   %C.CF.call: init %empty_tuple.type = call %CF.ref()
// CHECK:STDOUT:   %DF.ref: %D.DF.type = name_ref DF, @D.%D.DF.decl [concrete = constants.%D.DF]
// CHECK:STDOUT:   %D.DF.call: init %empty_tuple.type = call %DF.ref()
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc36: <bound method> = bound_method %d.var, constants.%T.as.Destroy.impl.Op.47c
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.1: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.47c, @T.as.Destroy.impl.Op(constants.%D) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.ceb]
// CHECK:STDOUT:   %bound_method.loc36_7: <bound method> = bound_method %d.var, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc36: %ptr.321 = addr_of %d.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc36: init %empty_tuple.type = call %bound_method.loc36_7(%addr.loc36)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc35: <bound method> = bound_method %c.var, constants.%T.as.Destroy.impl.Op.f77
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.2: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.f77, @T.as.Destroy.impl.Op(constants.%C) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.c65]
// CHECK:STDOUT:   %bound_method.loc35_7: <bound method> = bound_method %c.var, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc35: %ptr.388 = addr_of %c.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc35: init %empty_tuple.type = call %bound_method.loc35_7(%addr.loc35)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc34: <bound method> = bound_method %b.var, constants.%T.as.Destroy.impl.Op.4a4
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.3: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.4a4, @T.as.Destroy.impl.Op(constants.%B) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.70c]
// CHECK:STDOUT:   %bound_method.loc34_7: <bound method> = bound_method %b.var, %T.as.Destroy.impl.Op.specific_fn.3
// CHECK:STDOUT:   %addr.loc34: %ptr.01b = addr_of %b.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc34: init %empty_tuple.type = call %bound_method.loc34_7(%addr.loc34)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc33: <bound method> = bound_method %a.var, constants.%T.as.Destroy.impl.Op.885
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.4: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.885, @T.as.Destroy.impl.Op(constants.%A) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.716]
// CHECK:STDOUT:   %bound_method.loc33_7: <bound method> = bound_method %a.var, %T.as.Destroy.impl.Op.specific_fn.4
// CHECK:STDOUT:   %addr.loc33: %ptr.6db = addr_of %a.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc33: init %empty_tuple.type = call %bound_method.loc33_7(%addr.loc33)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @C.CF();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @A.AF();
// CHECK:STDOUT:
