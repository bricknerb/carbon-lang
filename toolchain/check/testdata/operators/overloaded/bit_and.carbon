// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/operators/overloaded/bit_and.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/operators/overloaded/bit_and.carbon

// This file was generated from binary_op.carbon.tmpl. Run make_tests.sh to regenerate.

package User;

class C {};

impl C as Core.BitAnd {
  fn Op[self: C](other: C) -> C {
    return {};
  }
}
impl C as Core.BitAndAssign {
  fn Op[addr self: C*](other: C) {}
}

fn TestOp(a: C, b: C) -> C {
  return a & b;
}

fn TestAssign(a: C*, b: C) {
  *a &= b;
}

// CHECK:STDOUT: --- bit_and.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %BitAnd.type: type = facet_type <@BitAnd> [concrete]
// CHECK:STDOUT:   %Op.type.27a: type = fn_type @Op.1 [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %BitAnd.impl_witness: <witness> = impl_witness file.%BitAnd.impl_witness_table [concrete]
// CHECK:STDOUT:   %pattern_type.c48: type = pattern_type %C [concrete]
// CHECK:STDOUT:   %Op.type.45e: type = fn_type @Op.2 [concrete]
// CHECK:STDOUT:   %Op.c43: %Op.type.45e = struct_value () [concrete]
// CHECK:STDOUT:   %BitAnd.facet: %BitAnd.type = facet_value %C, (%BitAnd.impl_witness) [concrete]
// CHECK:STDOUT:   %C.val: %C = struct_value () [concrete]
// CHECK:STDOUT:   %BitAndAssign.type: type = facet_type <@BitAndAssign> [concrete]
// CHECK:STDOUT:   %Op.type.93f: type = fn_type @Op.3 [concrete]
// CHECK:STDOUT:   %pattern_type.f6d: type = pattern_type auto [concrete]
// CHECK:STDOUT:   %BitAndAssign.impl_witness: <witness> = impl_witness file.%BitAndAssign.impl_witness_table [concrete]
// CHECK:STDOUT:   %ptr.019: type = ptr_type %C [concrete]
// CHECK:STDOUT:   %pattern_type.44a: type = pattern_type %ptr.019 [concrete]
// CHECK:STDOUT:   %Op.type.969: type = fn_type @Op.4 [concrete]
// CHECK:STDOUT:   %Op.747: %Op.type.969 = struct_value () [concrete]
// CHECK:STDOUT:   %BitAndAssign.facet: %BitAndAssign.type = facet_value %C, (%BitAndAssign.impl_witness) [concrete]
// CHECK:STDOUT:   %TestOp.type: type = fn_type @TestOp [concrete]
// CHECK:STDOUT:   %TestOp: %TestOp.type = struct_value () [concrete]
// CHECK:STDOUT:   %.670: type = fn_type_with_self_type %Op.type.27a, %BitAnd.facet [concrete]
// CHECK:STDOUT:   %TestAssign.type: type = fn_type @TestAssign [concrete]
// CHECK:STDOUT:   %TestAssign: %TestAssign.type = struct_value () [concrete]
// CHECK:STDOUT:   %.c09: type = fn_type_with_self_type %Op.type.93f, %BitAndAssign.facet [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .BitAnd = %Core.BitAnd
// CHECK:STDOUT:     .BitAndAssign = %Core.BitAndAssign
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.BitAnd: type = import_ref Core//prelude/operators/bitwise, BitAnd, loaded [concrete = constants.%BitAnd.type]
// CHECK:STDOUT:   %Core.BitAndAssign: type = import_ref Core//prelude/operators/bitwise, BitAndAssign, loaded [concrete = constants.%BitAndAssign.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .TestOp = %TestOp.decl
// CHECK:STDOUT:     .TestAssign = %TestAssign.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %C.decl: type = class_decl @C [concrete = constants.%C] {} {}
// CHECK:STDOUT:   impl_decl @impl.c1a [concrete] {} {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %Core.ref: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %BitAnd.ref: type = name_ref BitAnd, imports.%Core.BitAnd [concrete = constants.%BitAnd.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %BitAnd.impl_witness_table = impl_witness_table (@impl.c1a.%Op.decl), @impl.c1a [concrete]
// CHECK:STDOUT:   %BitAnd.impl_witness: <witness> = impl_witness %BitAnd.impl_witness_table [concrete = constants.%BitAnd.impl_witness]
// CHECK:STDOUT:   impl_decl @impl.4ac [concrete] {} {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %Core.ref: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %BitAndAssign.ref: type = name_ref BitAndAssign, imports.%Core.BitAndAssign [concrete = constants.%BitAndAssign.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %BitAndAssign.impl_witness_table = impl_witness_table (@impl.4ac.%Op.decl), @impl.4ac [concrete]
// CHECK:STDOUT:   %BitAndAssign.impl_witness: <witness> = impl_witness %BitAndAssign.impl_witness_table [concrete = constants.%BitAndAssign.impl_witness]
// CHECK:STDOUT:   %TestOp.decl: %TestOp.type = fn_decl @TestOp [concrete = constants.%TestOp] {
// CHECK:STDOUT:     %a.patt: %pattern_type.c48 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.c48 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %b.patt: %pattern_type.c48 = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.param_patt: %pattern_type.c48 = value_param_pattern %b.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.c48 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.c48 = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref.loc29_26: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %a.param: %C = value_param call_param0
// CHECK:STDOUT:     %C.ref.loc29_14: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %a: %C = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %C = value_param call_param1
// CHECK:STDOUT:     %C.ref.loc29_20: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %b: %C = bind_name b, %b.param
// CHECK:STDOUT:     %return.param: ref %C = out_param call_param2
// CHECK:STDOUT:     %return: ref %C = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %TestAssign.decl: %TestAssign.type = fn_decl @TestAssign [concrete = constants.%TestAssign] {
// CHECK:STDOUT:     %a.patt: %pattern_type.44a = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.44a = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %b.patt: %pattern_type.c48 = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.param_patt: %pattern_type.c48 = value_param_pattern %b.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %a.param: %ptr.019 = value_param call_param0
// CHECK:STDOUT:     %.loc33: type = splice_block %ptr [concrete = constants.%ptr.019] {
// CHECK:STDOUT:       %C.ref.loc33_18: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:       %ptr: type = ptr_type %C.ref.loc33_18 [concrete = constants.%ptr.019]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %a: %ptr.019 = bind_name a, %a.param
// CHECK:STDOUT:     %b.param: %C = value_param call_param1
// CHECK:STDOUT:     %C.ref.loc33_25: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %b: %C = bind_name b, %b.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: impl @impl.c1a: %C.ref as %BitAnd.ref {
// CHECK:STDOUT:   %Op.decl: %Op.type.45e = fn_decl @Op.2 [concrete = constants.%Op.c43] {
// CHECK:STDOUT:     %self.patt: %pattern_type.c48 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.c48 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %other.patt: %pattern_type.c48 = binding_pattern other [concrete]
// CHECK:STDOUT:     %other.param_patt: %pattern_type.c48 = value_param_pattern %other.patt, call_param1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.c48 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.c48 = out_param_pattern %return.patt, call_param2 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref.loc21_31: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %self.param: %C = value_param call_param0
// CHECK:STDOUT:     %C.ref.loc21_15: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %self: %C = bind_name self, %self.param
// CHECK:STDOUT:     %other.param: %C = value_param call_param1
// CHECK:STDOUT:     %C.ref.loc21_25: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %other: %C = bind_name other, %other.param
// CHECK:STDOUT:     %return.param: ref %C = out_param call_param2
// CHECK:STDOUT:     %return: ref %C = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .C = <poisoned>
// CHECK:STDOUT:   .Op = %Op.decl
// CHECK:STDOUT:   witness = file.%BitAnd.impl_witness
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: impl @impl.4ac: %C.ref as %BitAndAssign.ref {
// CHECK:STDOUT:   %Op.decl: %Op.type.969 = fn_decl @Op.4 [concrete = constants.%Op.747] {
// CHECK:STDOUT:     %self.patt: %pattern_type.44a = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.44a = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.loc26_9: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:     %other.patt: %pattern_type.c48 = binding_pattern other [concrete]
// CHECK:STDOUT:     %other.param_patt: %pattern_type.c48 = value_param_pattern %other.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ptr.019 = value_param call_param0
// CHECK:STDOUT:     %.loc26_21: type = splice_block %ptr [concrete = constants.%ptr.019] {
// CHECK:STDOUT:       %C.ref.loc26_20: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:       %ptr: type = ptr_type %C.ref.loc26_20 [concrete = constants.%ptr.019]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %self: %ptr.019 = bind_name self, %self.param
// CHECK:STDOUT:     %other.param: %C = value_param call_param1
// CHECK:STDOUT:     %C.ref.loc26_31: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %other: %C = bind_name other, %other.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .C = <poisoned>
// CHECK:STDOUT:   .Op = %Op.decl
// CHECK:STDOUT:   witness = file.%BitAndAssign.impl_witness
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Op.2(%self.param: %C, %other.param: %C) -> %return.param: %C {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %.loc22_13.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc22_13.2: init %C = class_init (), %return [concrete = constants.%C.val]
// CHECK:STDOUT:   %.loc22_14: init %C = converted %.loc22_13.1, %.loc22_13.2 [concrete = constants.%C.val]
// CHECK:STDOUT:   return %.loc22_14 to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Op.4(%self.param: %ptr.019, %other.param: %C) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestOp(%a.param: %C, %b.param: %C) -> %return.param: %C {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a.ref: %C = name_ref a, %a
// CHECK:STDOUT:   %b.ref: %C = name_ref b, %b
// CHECK:STDOUT:   %impl.elem0: %.670 = impl_witness_access constants.%BitAnd.impl_witness, element0 [concrete = constants.%Op.c43]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %a.ref, %impl.elem0
// CHECK:STDOUT:   %.loc29: ref %C = splice_block %return {}
// CHECK:STDOUT:   %Op.call: init %C = call %bound_method(%a.ref, %b.ref) to %.loc29
// CHECK:STDOUT:   return %Op.call to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestAssign(%a.param: %ptr.019, %b.param: %C) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a.ref: %ptr.019 = name_ref a, %a
// CHECK:STDOUT:   %.loc34: ref %C = deref %a.ref
// CHECK:STDOUT:   %b.ref: %C = name_ref b, %b
// CHECK:STDOUT:   %impl.elem0: %.c09 = impl_witness_access constants.%BitAndAssign.impl_witness, element0 [concrete = constants.%Op.747]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %.loc34, %impl.elem0
// CHECK:STDOUT:   %addr: %ptr.019 = addr_of %.loc34
// CHECK:STDOUT:   %Op.call: init %empty_tuple.type = call %bound_method(%addr, %b.ref)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
