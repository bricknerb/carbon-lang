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
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/import.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/import.carbon

// --- a.carbon

library "[[@TEST_NAME]]";

class Empty {
}

class Field {
  var x: i32;
}

class ForwardDeclared;

class ForwardDeclared {
  fn F[self: Self]();
  fn G[addr self: Self*]();
}

class Incomplete;

// --- b.carbon

library "[[@TEST_NAME]]";

import library "a";

fn Run() {
  var a: Empty = {};

  var b: Field = {.x = 1};
  b.x = 2;

  var c: ForwardDeclared = {};
  c.F();
  c.G();

  var d: ForwardDeclared* = &c;

  var e: Incomplete*;
}

// CHECK:STDOUT: --- a.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Empty: type = class_type @Empty [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type.357: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %Field: type = class_type @Field [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = generic_class_type @Int [concrete]
// CHECK:STDOUT:   %Int.generic: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %Field.elem: type = unbound_element_type %Field, %i32 [concrete]
// CHECK:STDOUT:   %struct_type.x: type = struct_type {.x: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.1ec: <witness> = complete_type_witness %struct_type.x [concrete]
// CHECK:STDOUT:   %ForwardDeclared: type = class_type @ForwardDeclared [concrete]
// CHECK:STDOUT:   %pattern_type.1b8: type = pattern_type %ForwardDeclared [concrete]
// CHECK:STDOUT:   %ForwardDeclared.F.type: type = fn_type @ForwardDeclared.F [concrete]
// CHECK:STDOUT:   %ForwardDeclared.F: %ForwardDeclared.F.type = struct_value () [concrete]
// CHECK:STDOUT:   %ptr: type = ptr_type %ForwardDeclared [concrete]
// CHECK:STDOUT:   %pattern_type.ebb: type = pattern_type %ptr [concrete]
// CHECK:STDOUT:   %pattern_type.f6d: type = pattern_type auto [concrete]
// CHECK:STDOUT:   %ForwardDeclared.G.type: type = fn_type @ForwardDeclared.G [concrete]
// CHECK:STDOUT:   %ForwardDeclared.G: %ForwardDeclared.G.type = struct_value () [concrete]
// CHECK:STDOUT:   %Incomplete: type = class_type @Incomplete [concrete]
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
// CHECK:STDOUT:     .Empty = %Empty.decl
// CHECK:STDOUT:     .Field = %Field.decl
// CHECK:STDOUT:     .ForwardDeclared = %ForwardDeclared.decl.loc11
// CHECK:STDOUT:     .Incomplete = %Incomplete.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Empty.decl: type = class_decl @Empty [concrete = constants.%Empty] {} {}
// CHECK:STDOUT:   %Field.decl: type = class_decl @Field [concrete = constants.%Field] {} {}
// CHECK:STDOUT:   %ForwardDeclared.decl.loc11: type = class_decl @ForwardDeclared [concrete = constants.%ForwardDeclared] {} {}
// CHECK:STDOUT:   %ForwardDeclared.decl.loc13: type = class_decl @ForwardDeclared [concrete = constants.%ForwardDeclared] {} {}
// CHECK:STDOUT:   %Incomplete.decl: type = class_decl @Incomplete [concrete = constants.%Incomplete] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Empty {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type.357]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Empty
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Field {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %.loc8: %Field.elem = field_decl x, element0 [concrete]
// CHECK:STDOUT:   %struct_type.x: type = struct_type {.x: %i32} [concrete = constants.%struct_type.x]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.x [concrete = constants.%complete_type.1ec]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Field
// CHECK:STDOUT:   .x = %.loc8
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @ForwardDeclared {
// CHECK:STDOUT:   %ForwardDeclared.F.decl: %ForwardDeclared.F.type = fn_decl @ForwardDeclared.F [concrete = constants.%ForwardDeclared.F] {
// CHECK:STDOUT:     %self.patt: %pattern_type.1b8 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.1b8 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ForwardDeclared = value_param call_param0
// CHECK:STDOUT:     %Self.ref: type = name_ref Self, constants.%ForwardDeclared [concrete = constants.%ForwardDeclared]
// CHECK:STDOUT:     %self: %ForwardDeclared = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %ForwardDeclared.G.decl: %ForwardDeclared.G.type = fn_decl @ForwardDeclared.G [concrete = constants.%ForwardDeclared.G] {
// CHECK:STDOUT:     %self.patt: %pattern_type.ebb = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.ebb = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.loc15_8: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ptr = value_param call_param0
// CHECK:STDOUT:     %.loc15_23: type = splice_block %ptr [concrete = constants.%ptr] {
// CHECK:STDOUT:       %Self.ref: type = name_ref Self, constants.%ForwardDeclared [concrete = constants.%ForwardDeclared]
// CHECK:STDOUT:       %ptr: type = ptr_type %Self.ref [concrete = constants.%ptr]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %self: %ptr = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type.357]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%ForwardDeclared
// CHECK:STDOUT:   .F = %ForwardDeclared.F.decl
// CHECK:STDOUT:   .G = %ForwardDeclared.G.decl
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Incomplete;
// CHECK:STDOUT:
// CHECK:STDOUT: fn @ForwardDeclared.F(%self.param: %ForwardDeclared);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @ForwardDeclared.G(%self.param: %ptr);
// CHECK:STDOUT:
// CHECK:STDOUT: --- b.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Run.type: type = fn_type @Run [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %Run: %Run.type = struct_value () [concrete]
// CHECK:STDOUT:   %Empty: type = class_type @Empty [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type.357: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %pattern_type.2f0: type = pattern_type %Empty [concrete]
// CHECK:STDOUT:   %Empty.val: %Empty = struct_value () [concrete]
// CHECK:STDOUT:   %Field: type = class_type @Field [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %struct_type.x.767: type = struct_type {.x: %i32} [concrete]
// CHECK:STDOUT:   %complete_type.c07: <witness> = complete_type_witness %struct_type.x.767 [concrete]
// CHECK:STDOUT:   %pattern_type.f46: type = pattern_type %Field [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %struct_type.x.c96: type = struct_type {.x: Core.IntLiteral} [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.cc7: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.cc7 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.9ba: type = facet_type <@ImplicitAs, @ImplicitAs(%i32)> [concrete]
// CHECK:STDOUT:   %ImplicitAs.Convert.type.6da: type = fn_type @ImplicitAs.Convert, @ImplicitAs(%i32) [concrete]
// CHECK:STDOUT:   %To: Core.IntLiteral = bind_symbolic_name To, 0 [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0b2: type = fn_type @Core.IntLiteral.as.ImplicitAs.impl.Convert, @Core.IntLiteral.as.ImplicitAs.impl(%To) [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.6d7: %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0b2 = struct_value () [symbolic]
// CHECK:STDOUT:   %ImplicitAs.impl_witness.e34: <witness> = impl_witness imports.%ImplicitAs.impl_witness_table.e36, @Core.IntLiteral.as.ImplicitAs.impl(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.ed5: type = fn_type @Core.IntLiteral.as.ImplicitAs.impl.Convert, @Core.IntLiteral.as.ImplicitAs.impl(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.16d: %Core.IntLiteral.as.ImplicitAs.impl.Convert.type.ed5 = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.facet: %ImplicitAs.type.9ba = facet_value Core.IntLiteral, (%ImplicitAs.impl_witness.e34) [concrete]
// CHECK:STDOUT:   %.d6a: type = fn_type_with_self_type %ImplicitAs.Convert.type.6da, %ImplicitAs.facet [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.43e: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.ImplicitAs.impl.Convert.16d [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn: <specific function> = specific_function %Core.IntLiteral.as.ImplicitAs.impl.Convert.16d, @Core.IntLiteral.as.ImplicitAs.impl.Convert(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.947: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_1.47b: %i32 = int_value 1 [concrete]
// CHECK:STDOUT:   %Field.val: %Field = struct_value (%int_1.47b) [concrete]
// CHECK:STDOUT:   %Field.elem: type = unbound_element_type %Field, %i32 [concrete]
// CHECK:STDOUT:   %int_2.ecc: Core.IntLiteral = int_value 2 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.918: <bound method> = bound_method %int_2.ecc, %Core.IntLiteral.as.ImplicitAs.impl.Convert.16d [concrete]
// CHECK:STDOUT:   %bound_method.c74: <bound method> = bound_method %int_2.ecc, %Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn [concrete]
// CHECK:STDOUT:   %int_2.d0d: %i32 = int_value 2 [concrete]
// CHECK:STDOUT:   %ForwardDeclared.7b34f2.1: type = class_type @ForwardDeclared.1 [concrete]
// CHECK:STDOUT:   %pattern_type.1b8: type = pattern_type %ForwardDeclared.7b34f2.1 [concrete]
// CHECK:STDOUT:   %ForwardDeclared.val: %ForwardDeclared.7b34f2.1 = struct_value () [concrete]
// CHECK:STDOUT:   %ForwardDeclared.F.type: type = fn_type @ForwardDeclared.F [concrete]
// CHECK:STDOUT:   %ForwardDeclared.F: %ForwardDeclared.F.type = struct_value () [concrete]
// CHECK:STDOUT:   %ForwardDeclared.G.type: type = fn_type @ForwardDeclared.G [concrete]
// CHECK:STDOUT:   %ForwardDeclared.G: %ForwardDeclared.G.type = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.6cf: type = ptr_type %ForwardDeclared.7b34f2.1 [concrete]
// CHECK:STDOUT:   %pattern_type.ebb: type = pattern_type %ptr.6cf [concrete]
// CHECK:STDOUT:   %Incomplete: type = class_type @Incomplete [concrete]
// CHECK:STDOUT:   %ptr.c62: type = ptr_type %Incomplete [concrete]
// CHECK:STDOUT:   %pattern_type.275: type = pattern_type %ptr.c62 [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.7f0: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%ptr.c62) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.90d: %T.as.Destroy.impl.Op.type.7f0 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.c22: type = ptr_type %ptr.c62 [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.b24: <specific function> = specific_function %T.as.Destroy.impl.Op.90d, @T.as.Destroy.impl.Op(%ptr.c62) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.d2a: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%ptr.6cf) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.3b0: %T.as.Destroy.impl.Op.type.d2a = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.df0: type = ptr_type %ptr.6cf [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.3de: <specific function> = specific_function %T.as.Destroy.impl.Op.3b0, @T.as.Destroy.impl.Op(%ptr.6cf) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.308: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%ForwardDeclared.7b34f2.1) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.9e2: %T.as.Destroy.impl.Op.type.308 = struct_value () [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.09a: <specific function> = specific_function %T.as.Destroy.impl.Op.9e2, @T.as.Destroy.impl.Op(%ForwardDeclared.7b34f2.1) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.47c: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%Field) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.576: %T.as.Destroy.impl.Op.type.47c = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.d8b: type = ptr_type %Field [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.29d: <specific function> = specific_function %T.as.Destroy.impl.Op.576, @T.as.Destroy.impl.Op(%Field) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.b74: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%Empty) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.6e4: %T.as.Destroy.impl.Op.type.b74 = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.961: type = ptr_type %Empty [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.1a5: <specific function> = specific_function %T.as.Destroy.impl.Op.6e4, @T.as.Destroy.impl.Op(%Empty) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.Empty: type = import_ref Main//a, Empty, loaded [concrete = constants.%Empty]
// CHECK:STDOUT:   %Main.Field: type = import_ref Main//a, Field, loaded [concrete = constants.%Field]
// CHECK:STDOUT:   %Main.ForwardDeclared: type = import_ref Main//a, ForwardDeclared, loaded [concrete = constants.%ForwardDeclared.7b34f2.1]
// CHECK:STDOUT:   %Main.Incomplete: type = import_ref Main//a, Incomplete, loaded [concrete = constants.%Incomplete]
// CHECK:STDOUT:   %Core.ece: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .ImplicitAs = %Core.ImplicitAs
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Main.import_ref.8f24d3.1: <witness> = import_ref Main//a, loc5_1, loaded [concrete = constants.%complete_type.357]
// CHECK:STDOUT:   %Main.import_ref.fd7 = import_ref Main//a, inst18 [no loc], unloaded
// CHECK:STDOUT:   %Main.import_ref.709: <witness> = import_ref Main//a, loc9_1, loaded [concrete = constants.%complete_type.c07]
// CHECK:STDOUT:   %Main.import_ref.845 = import_ref Main//a, inst24 [no loc], unloaded
// CHECK:STDOUT:   %Main.import_ref.4d2: %Field.elem = import_ref Main//a, loc8_8, loaded [concrete = %.d33]
// CHECK:STDOUT:   %Core.ImplicitAs: %ImplicitAs.type.cc7 = import_ref Core//prelude/parts/as, ImplicitAs, loaded [concrete = constants.%ImplicitAs.generic]
// CHECK:STDOUT:   %Core.import_ref.a86: @Core.IntLiteral.as.ImplicitAs.impl.%Core.IntLiteral.as.ImplicitAs.impl.Convert.type (%Core.IntLiteral.as.ImplicitAs.impl.Convert.type.0b2) = import_ref Core//prelude/parts/int, loc16_39, loaded [symbolic = @Core.IntLiteral.as.ImplicitAs.impl.%Core.IntLiteral.as.ImplicitAs.impl.Convert (constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.6d7)]
// CHECK:STDOUT:   %ImplicitAs.impl_witness_table.e36 = impl_witness_table (%Core.import_ref.a86), @Core.IntLiteral.as.ImplicitAs.impl [concrete]
// CHECK:STDOUT:   %.d33: %Field.elem = field_decl x, element0 [concrete]
// CHECK:STDOUT:   %Main.import_ref.8f24d3.2: <witness> = import_ref Main//a, loc16_1, loaded [concrete = constants.%complete_type.357]
// CHECK:STDOUT:   %Main.import_ref.39e731.1 = import_ref Main//a, inst59 [no loc], unloaded
// CHECK:STDOUT:   %Main.import_ref.760: %ForwardDeclared.F.type = import_ref Main//a, loc14_21, loaded [concrete = constants.%ForwardDeclared.F]
// CHECK:STDOUT:   %Main.import_ref.26e: %ForwardDeclared.G.type = import_ref Main//a, loc15_27, loaded [concrete = constants.%ForwardDeclared.G]
// CHECK:STDOUT:   %Main.import_ref.8f24d3.3: <witness> = import_ref Main//a, loc16_1, loaded [concrete = constants.%complete_type.357]
// CHECK:STDOUT:   %Main.import_ref.39e731.2 = import_ref Main//a, inst59 [no loc], unloaded
// CHECK:STDOUT:   %Main.import_ref.42a = import_ref Main//a, loc14_21, unloaded
// CHECK:STDOUT:   %Main.import_ref.67a = import_ref Main//a, loc15_27, unloaded
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Empty = imports.%Main.Empty
// CHECK:STDOUT:     .Field = imports.%Main.Field
// CHECK:STDOUT:     .ForwardDeclared = imports.%Main.ForwardDeclared
// CHECK:STDOUT:     .Incomplete = imports.%Main.Incomplete
// CHECK:STDOUT:     .Core = imports.%Core.ece
// CHECK:STDOUT:     .Run = %Run.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   %Run.decl: %Run.type = fn_decl @Run [concrete = constants.%Run] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Empty [from "a.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.8f24d3.1
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.fd7
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Field [from "a.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.709
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.845
// CHECK:STDOUT:   .x = imports.%Main.import_ref.4d2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @ForwardDeclared.1 [from "a.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.8f24d3.2
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.39e731.1
// CHECK:STDOUT:   .F = imports.%Main.import_ref.760
// CHECK:STDOUT:   .G = imports.%Main.import_ref.26e
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @ForwardDeclared.2 [from "a.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.8f24d3.3
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.39e731.2
// CHECK:STDOUT:   .F = imports.%Main.import_ref.42a
// CHECK:STDOUT:   .G = imports.%Main.import_ref.67a
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Incomplete [from "a.carbon"];
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Run() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %a.patt: %pattern_type.2f0 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.var_patt: %pattern_type.2f0 = var_pattern %a.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a.var: ref %Empty = var %a.var_patt
// CHECK:STDOUT:   %.loc7_19.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc7_19.2: init %Empty = class_init (), %a.var [concrete = constants.%Empty.val]
// CHECK:STDOUT:   %.loc7_3: init %Empty = converted %.loc7_19.1, %.loc7_19.2 [concrete = constants.%Empty.val]
// CHECK:STDOUT:   assign %a.var, %.loc7_3
// CHECK:STDOUT:   %Empty.ref: type = name_ref Empty, imports.%Main.Empty [concrete = constants.%Empty]
// CHECK:STDOUT:   %a: ref %Empty = bind_name a, %a.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %b.patt: %pattern_type.f46 = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.var_patt: %pattern_type.f46 = var_pattern %b.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b.var: ref %Field = var %b.var_patt
// CHECK:STDOUT:   %int_1: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %.loc9_25.1: %struct_type.x.c96 = struct_literal (%int_1)
// CHECK:STDOUT:   %impl.elem0.loc9: %.d6a = impl_witness_access constants.%ImplicitAs.impl_witness.e34, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.16d]
// CHECK:STDOUT:   %bound_method.loc9_25.1: <bound method> = bound_method %int_1, %impl.elem0.loc9 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.43e]
// CHECK:STDOUT:   %specific_fn.loc9: <specific function> = specific_function %impl.elem0.loc9, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc9_25.2: <bound method> = bound_method %int_1, %specific_fn.loc9 [concrete = constants.%bound_method.947]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc9: init %i32 = call %bound_method.loc9_25.2(%int_1) [concrete = constants.%int_1.47b]
// CHECK:STDOUT:   %.loc9_25.2: init %i32 = converted %int_1, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc9 [concrete = constants.%int_1.47b]
// CHECK:STDOUT:   %.loc9_25.3: ref %i32 = class_element_access %b.var, element0
// CHECK:STDOUT:   %.loc9_25.4: init %i32 = initialize_from %.loc9_25.2 to %.loc9_25.3 [concrete = constants.%int_1.47b]
// CHECK:STDOUT:   %.loc9_25.5: init %Field = class_init (%.loc9_25.4), %b.var [concrete = constants.%Field.val]
// CHECK:STDOUT:   %.loc9_3: init %Field = converted %.loc9_25.1, %.loc9_25.5 [concrete = constants.%Field.val]
// CHECK:STDOUT:   assign %b.var, %.loc9_3
// CHECK:STDOUT:   %Field.ref: type = name_ref Field, imports.%Main.Field [concrete = constants.%Field]
// CHECK:STDOUT:   %b: ref %Field = bind_name b, %b.var
// CHECK:STDOUT:   %b.ref: ref %Field = name_ref b, %b
// CHECK:STDOUT:   %x.ref: %Field.elem = name_ref x, imports.%Main.import_ref.4d2 [concrete = imports.%.d33]
// CHECK:STDOUT:   %.loc10_4: ref %i32 = class_element_access %b.ref, element0
// CHECK:STDOUT:   %int_2: Core.IntLiteral = int_value 2 [concrete = constants.%int_2.ecc]
// CHECK:STDOUT:   %impl.elem0.loc10: %.d6a = impl_witness_access constants.%ImplicitAs.impl_witness.e34, element0 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.16d]
// CHECK:STDOUT:   %bound_method.loc10_7.1: <bound method> = bound_method %int_2, %impl.elem0.loc10 [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.bound.918]
// CHECK:STDOUT:   %specific_fn.loc10: <specific function> = specific_function %impl.elem0.loc10, @Core.IntLiteral.as.ImplicitAs.impl.Convert(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.ImplicitAs.impl.Convert.specific_fn]
// CHECK:STDOUT:   %bound_method.loc10_7.2: <bound method> = bound_method %int_2, %specific_fn.loc10 [concrete = constants.%bound_method.c74]
// CHECK:STDOUT:   %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc10: init %i32 = call %bound_method.loc10_7.2(%int_2) [concrete = constants.%int_2.d0d]
// CHECK:STDOUT:   %.loc10_7: init %i32 = converted %int_2, %Core.IntLiteral.as.ImplicitAs.impl.Convert.call.loc10 [concrete = constants.%int_2.d0d]
// CHECK:STDOUT:   assign %.loc10_4, %.loc10_7
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %c.patt: %pattern_type.1b8 = binding_pattern c [concrete]
// CHECK:STDOUT:     %c.var_patt: %pattern_type.1b8 = var_pattern %c.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c.var: ref %ForwardDeclared.7b34f2.1 = var %c.var_patt
// CHECK:STDOUT:   %.loc12_29.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc12_29.2: init %ForwardDeclared.7b34f2.1 = class_init (), %c.var [concrete = constants.%ForwardDeclared.val]
// CHECK:STDOUT:   %.loc12_3: init %ForwardDeclared.7b34f2.1 = converted %.loc12_29.1, %.loc12_29.2 [concrete = constants.%ForwardDeclared.val]
// CHECK:STDOUT:   assign %c.var, %.loc12_3
// CHECK:STDOUT:   %ForwardDeclared.ref.loc12: type = name_ref ForwardDeclared, imports.%Main.ForwardDeclared [concrete = constants.%ForwardDeclared.7b34f2.1]
// CHECK:STDOUT:   %c: ref %ForwardDeclared.7b34f2.1 = bind_name c, %c.var
// CHECK:STDOUT:   %c.ref.loc13: ref %ForwardDeclared.7b34f2.1 = name_ref c, %c
// CHECK:STDOUT:   %F.ref: %ForwardDeclared.F.type = name_ref F, imports.%Main.import_ref.760 [concrete = constants.%ForwardDeclared.F]
// CHECK:STDOUT:   %ForwardDeclared.F.bound: <bound method> = bound_method %c.ref.loc13, %F.ref
// CHECK:STDOUT:   %.loc13: %ForwardDeclared.7b34f2.1 = bind_value %c.ref.loc13
// CHECK:STDOUT:   %ForwardDeclared.F.call: init %empty_tuple.type = call %ForwardDeclared.F.bound(%.loc13)
// CHECK:STDOUT:   %c.ref.loc14: ref %ForwardDeclared.7b34f2.1 = name_ref c, %c
// CHECK:STDOUT:   %G.ref: %ForwardDeclared.G.type = name_ref G, imports.%Main.import_ref.26e [concrete = constants.%ForwardDeclared.G]
// CHECK:STDOUT:   %ForwardDeclared.G.bound: <bound method> = bound_method %c.ref.loc14, %G.ref
// CHECK:STDOUT:   %addr.loc14: %ptr.6cf = addr_of %c.ref.loc14
// CHECK:STDOUT:   %ForwardDeclared.G.call: init %empty_tuple.type = call %ForwardDeclared.G.bound(%addr.loc14)
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %d.patt: %pattern_type.ebb = binding_pattern d [concrete]
// CHECK:STDOUT:     %d.var_patt: %pattern_type.ebb = var_pattern %d.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %d.var: ref %ptr.6cf = var %d.var_patt
// CHECK:STDOUT:   %c.ref.loc16: ref %ForwardDeclared.7b34f2.1 = name_ref c, %c
// CHECK:STDOUT:   %addr.loc16_29: %ptr.6cf = addr_of %c.ref.loc16
// CHECK:STDOUT:   assign %d.var, %addr.loc16_29
// CHECK:STDOUT:   %.loc16: type = splice_block %ptr.loc16 [concrete = constants.%ptr.6cf] {
// CHECK:STDOUT:     %ForwardDeclared.ref.loc16: type = name_ref ForwardDeclared, imports.%Main.ForwardDeclared [concrete = constants.%ForwardDeclared.7b34f2.1]
// CHECK:STDOUT:     %ptr.loc16: type = ptr_type %ForwardDeclared.ref.loc16 [concrete = constants.%ptr.6cf]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %d: ref %ptr.6cf = bind_name d, %d.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %e.patt: %pattern_type.275 = binding_pattern e [concrete]
// CHECK:STDOUT:     %e.var_patt: %pattern_type.275 = var_pattern %e.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %e.var: ref %ptr.c62 = var %e.var_patt
// CHECK:STDOUT:   %.loc18: type = splice_block %ptr.loc18 [concrete = constants.%ptr.c62] {
// CHECK:STDOUT:     %Incomplete.ref: type = name_ref Incomplete, imports.%Main.Incomplete [concrete = constants.%Incomplete]
// CHECK:STDOUT:     %ptr.loc18: type = ptr_type %Incomplete.ref [concrete = constants.%ptr.c62]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %e: ref %ptr.c62 = bind_name e, %e.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc18: <bound method> = bound_method %e.var, constants.%T.as.Destroy.impl.Op.90d
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.1: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.90d, @T.as.Destroy.impl.Op(constants.%ptr.c62) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.b24]
// CHECK:STDOUT:   %bound_method.loc18: <bound method> = bound_method %e.var, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc18: %ptr.c22 = addr_of %e.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc18: init %empty_tuple.type = call %bound_method.loc18(%addr.loc18)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc16: <bound method> = bound_method %d.var, constants.%T.as.Destroy.impl.Op.3b0
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.2: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.3b0, @T.as.Destroy.impl.Op(constants.%ptr.6cf) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.3de]
// CHECK:STDOUT:   %bound_method.loc16: <bound method> = bound_method %d.var, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc16_3: %ptr.df0 = addr_of %d.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc16: init %empty_tuple.type = call %bound_method.loc16(%addr.loc16_3)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc12: <bound method> = bound_method %c.var, constants.%T.as.Destroy.impl.Op.9e2
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.3: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.9e2, @T.as.Destroy.impl.Op(constants.%ForwardDeclared.7b34f2.1) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.09a]
// CHECK:STDOUT:   %bound_method.loc12: <bound method> = bound_method %c.var, %T.as.Destroy.impl.Op.specific_fn.3
// CHECK:STDOUT:   %addr.loc12: %ptr.6cf = addr_of %c.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc12: init %empty_tuple.type = call %bound_method.loc12(%addr.loc12)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc9: <bound method> = bound_method %b.var, constants.%T.as.Destroy.impl.Op.576
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.4: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.576, @T.as.Destroy.impl.Op(constants.%Field) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.29d]
// CHECK:STDOUT:   %bound_method.loc9_3: <bound method> = bound_method %b.var, %T.as.Destroy.impl.Op.specific_fn.4
// CHECK:STDOUT:   %addr.loc9: %ptr.d8b = addr_of %b.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc9: init %empty_tuple.type = call %bound_method.loc9_3(%addr.loc9)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc7: <bound method> = bound_method %a.var, constants.%T.as.Destroy.impl.Op.6e4
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.5: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.6e4, @T.as.Destroy.impl.Op(constants.%Empty) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn.1a5]
// CHECK:STDOUT:   %bound_method.loc7: <bound method> = bound_method %a.var, %T.as.Destroy.impl.Op.specific_fn.5
// CHECK:STDOUT:   %addr.loc7: %ptr.961 = addr_of %a.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc7: init %empty_tuple.type = call %bound_method.loc7(%addr.loc7)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @ForwardDeclared.F [from "a.carbon"];
// CHECK:STDOUT:
// CHECK:STDOUT: fn @ForwardDeclared.G [from "a.carbon"];
// CHECK:STDOUT:
