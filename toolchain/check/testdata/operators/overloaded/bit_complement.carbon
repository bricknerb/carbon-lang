// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/operators/overloaded/bit_complement.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/operators/overloaded/bit_complement.carbon

// This file was generated from unary_op.carbon.tmpl. Run make_tests.sh to regenerate.

package User;

class C {};

impl C as Core.BitComplement {
  fn Op[self: C]() -> C {
    return {};
  }
}

fn TestOp(a: C) -> C {
  return ^a;
}

// CHECK:STDOUT: --- bit_complement.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %BitComplement.type: type = facet_type <@BitComplement> [concrete]
// CHECK:STDOUT:   %Op.type.f25: type = fn_type @Op.1 [concrete]
// CHECK:STDOUT:   %BitComplement.impl_witness: <witness> = impl_witness file.%BitComplement.impl_witness_table [concrete]
// CHECK:STDOUT:   %pattern_type.c48: type = pattern_type %C [concrete]
// CHECK:STDOUT:   %Op.type.544: type = fn_type @Op.2 [concrete]
// CHECK:STDOUT:   %Op.bf2: %Op.type.544 = struct_value () [concrete]
// CHECK:STDOUT:   %BitComplement.facet: %BitComplement.type = facet_value %C, (%BitComplement.impl_witness) [concrete]
// CHECK:STDOUT:   %C.val: %C = struct_value () [concrete]
// CHECK:STDOUT:   %TestOp.type: type = fn_type @TestOp [concrete]
// CHECK:STDOUT:   %TestOp: %TestOp.type = struct_value () [concrete]
// CHECK:STDOUT:   %.669: type = fn_type_with_self_type %Op.type.f25, %BitComplement.facet [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .BitComplement = %Core.BitComplement
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.BitComplement: type = import_ref Core//prelude/operators/bitwise, BitComplement, loaded [concrete = constants.%BitComplement.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .TestOp = %TestOp.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %C.decl: type = class_decl @C [concrete = constants.%C] {} {}
// CHECK:STDOUT:   impl_decl @impl.5a3 [concrete] {} {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %Core.ref: <namespace> = name_ref Core, imports.%Core [concrete = imports.%Core]
// CHECK:STDOUT:     %BitComplement.ref: type = name_ref BitComplement, imports.%Core.BitComplement [concrete = constants.%BitComplement.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %BitComplement.impl_witness_table = impl_witness_table (@impl.5a3.%Op.decl), @impl.5a3 [concrete]
// CHECK:STDOUT:   %BitComplement.impl_witness: <witness> = impl_witness %BitComplement.impl_witness_table [concrete = constants.%BitComplement.impl_witness]
// CHECK:STDOUT:   %TestOp.decl: %TestOp.type = fn_decl @TestOp [concrete = constants.%TestOp] {
// CHECK:STDOUT:     %a.patt: %pattern_type.c48 = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.c48 = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.c48 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.c48 = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref.loc26_20: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %a.param: %C = value_param call_param0
// CHECK:STDOUT:     %C.ref.loc26_14: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %a: %C = bind_name a, %a.param
// CHECK:STDOUT:     %return.param: ref %C = out_param call_param1
// CHECK:STDOUT:     %return: ref %C = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: impl @impl.5a3: %C.ref as %BitComplement.ref {
// CHECK:STDOUT:   %Op.decl: %Op.type.544 = fn_decl @Op.2 [concrete = constants.%Op.bf2] {
// CHECK:STDOUT:     %self.patt: %pattern_type.c48 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.c48 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.c48 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.c48 = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref.loc21_23: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %self.param: %C = value_param call_param0
// CHECK:STDOUT:     %C.ref.loc21_15: type = name_ref C, file.%C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %self: %C = bind_name self, %self.param
// CHECK:STDOUT:     %return.param: ref %C = out_param call_param1
// CHECK:STDOUT:     %return: ref %C = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .C = <poisoned>
// CHECK:STDOUT:   .Op = %Op.decl
// CHECK:STDOUT:   witness = file.%BitComplement.impl_witness
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
// CHECK:STDOUT: fn @Op.2(%self.param: %C) -> %return.param: %C {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %.loc22_13.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc22_13.2: init %C = class_init (), %return [concrete = constants.%C.val]
// CHECK:STDOUT:   %.loc22_14: init %C = converted %.loc22_13.1, %.loc22_13.2 [concrete = constants.%C.val]
// CHECK:STDOUT:   return %.loc22_14 to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestOp(%a.param: %C) -> %return.param: %C {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a.ref: %C = name_ref a, %a
// CHECK:STDOUT:   %impl.elem0: %.669 = impl_witness_access constants.%BitComplement.impl_witness, element0 [concrete = constants.%Op.bf2]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %a.ref, %impl.elem0
// CHECK:STDOUT:   %.loc26: ref %C = splice_block %return {}
// CHECK:STDOUT:   %Op.call: init %C = call %bound_method(%a.ref) to %.loc26
// CHECK:STDOUT:   return %Op.call to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
