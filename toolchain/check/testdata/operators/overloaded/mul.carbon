// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/full.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/operators/overloaded/mul.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/operators/overloaded/mul.carbon

// This file was generated from binary_op.carbon.tmpl. Run make_tests.sh to regenerate.

package User;

class C {};

impl C as Core.MulWith(C) where .Result = C {
  fn Op[self: C](other: C) -> C {
    return {};
  }
}
impl C as Core.MulAssignWith(C) {
  fn Op[addr self: C*](other: C) {}
}

fn TestOp(a: C, b: C) -> C {
  //@dump-sem-ir-begin
  return a * b;
  //@dump-sem-ir-end
}

fn TestAssign(a: C*, b: C) {
  //@dump-sem-ir-begin
  *a *= b;
  //@dump-sem-ir-end
}

// CHECK:STDOUT: --- mul.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %MulWith.type.3ed: type = facet_type <@MulWith, @MulWith(%C)> [concrete]
// CHECK:STDOUT:   %MulWith.Op.type.2c9: type = fn_type @MulWith.Op, @MulWith(%C) [concrete]
// CHECK:STDOUT:   %MulWith.impl_witness: <witness> = impl_witness file.%MulWith.impl_witness_table [concrete]
// CHECK:STDOUT:   %C.as.MulWith.impl.Op.type: type = fn_type @C.as.MulWith.impl.Op [concrete]
// CHECK:STDOUT:   %C.as.MulWith.impl.Op: %C.as.MulWith.impl.Op.type = struct_value () [concrete]
// CHECK:STDOUT:   %MulWith.facet.6c2: %MulWith.type.3ed = facet_value %C, (%MulWith.impl_witness) [concrete]
// CHECK:STDOUT:   %MulAssignWith.type.a15: type = facet_type <@MulAssignWith, @MulAssignWith(%C)> [concrete]
// CHECK:STDOUT:   %MulAssignWith.Op.type.5c8: type = fn_type @MulAssignWith.Op, @MulAssignWith(%C) [concrete]
// CHECK:STDOUT:   %MulAssignWith.impl_witness: <witness> = impl_witness file.%MulAssignWith.impl_witness_table [concrete]
// CHECK:STDOUT:   %ptr.019: type = ptr_type %C [concrete]
// CHECK:STDOUT:   %C.as.MulAssignWith.impl.Op.type: type = fn_type @C.as.MulAssignWith.impl.Op [concrete]
// CHECK:STDOUT:   %C.as.MulAssignWith.impl.Op: %C.as.MulAssignWith.impl.Op.type = struct_value () [concrete]
// CHECK:STDOUT:   %MulAssignWith.facet: %MulAssignWith.type.a15 = facet_value %C, (%MulAssignWith.impl_witness) [concrete]
// CHECK:STDOUT:   %.6ce: type = fn_type_with_self_type %MulWith.Op.type.2c9, %MulWith.facet.6c2 [concrete]
// CHECK:STDOUT:   %.5e8: type = fn_type_with_self_type %MulAssignWith.Op.type.5c8, %MulAssignWith.facet [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestOp(%a.param: %C, %b.param: %C) -> %return.param: %C {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a.ref: %C = name_ref a, %a
// CHECK:STDOUT:   %b.ref: %C = name_ref b, %b
// CHECK:STDOUT:   %impl.elem1: %.6ce = impl_witness_access constants.%MulWith.impl_witness, element1 [concrete = constants.%C.as.MulWith.impl.Op]
// CHECK:STDOUT:   %bound_method.loc32: <bound method> = bound_method %a.ref, %impl.elem1
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %C.as.MulWith.impl.Op.call: init %C = call %bound_method.loc32(%a.ref, %b.ref) to %.loc30
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   return %C.as.MulWith.impl.Op.call to %return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @TestAssign(%a.param: %ptr.019, %b.param: %C) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a.ref: %ptr.019 = name_ref a, %a
// CHECK:STDOUT:   %.loc38: ref %C = deref %a.ref
// CHECK:STDOUT:   %b.ref: %C = name_ref b, %b
// CHECK:STDOUT:   %impl.elem0: %.5e8 = impl_witness_access constants.%MulAssignWith.impl_witness, element0 [concrete = constants.%C.as.MulAssignWith.impl.Op]
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %.loc38, %impl.elem0
// CHECK:STDOUT:   %addr: %ptr.019 = addr_of %.loc38
// CHECK:STDOUT:   %C.as.MulAssignWith.impl.Op.call: init %empty_tuple.type = call %bound_method(%addr, %b.ref)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
