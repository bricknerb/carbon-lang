// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/destroy.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/tuple/tuple_pattern.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/tuple/tuple_pattern.carbon


// --- basic.carbon

library "[[@TEST_NAME]]";

fn F() {
  //@dump-sem-ir-begin
  let (x: {}, y: {}) = ({}, {});
  var (a: {}, b: {});
  var (c: {}, d: {}) = ({}, {});
  //@dump-sem-ir-end
}

// --- variable.carbon

library "[[@TEST_NAME]]";

fn F() {
  //@dump-sem-ir-begin
  var tuple: ({}, {}) = ({}, {});
  var (x: {}, y: {}) = tuple;
  //@dump-sem-ir-end
}

fn G() {
  //@dump-sem-ir-begin
  let tuple: ({}, {}) = ({}, {});
  var (x: {}, y: {}) = tuple;
  //@dump-sem-ir-end
}

fn MakeTuple() -> ({}, {});

fn H() {
  //@dump-sem-ir-begin
  var (x: {}, y: {}) = MakeTuple();
  //@dump-sem-ir-end
}

// --- nested.carbon

library "[[@TEST_NAME]]";

fn F() {
  //@dump-sem-ir-begin
  let (x: {}, (y: {}, z: {})) = ({}, ({}, {}));
  //@dump-sem-ir-end
}

// --- package_scope.carbon

library "[[@TEST_NAME]]";

//@dump-sem-ir-begin
let (x: {}, y: {}) = ({}, {});
//@dump-sem-ir-end

// --- fail_in_interface.carbon

library "[[@TEST_NAME]]";

interface I {
  // CHECK:STDERR: fail_in_interface.carbon:[[@LINE+12]]:8: error: found runtime binding pattern in associated constant declaration; expected a `:!` binding [ExpectedSymbolicBindingInAssociatedConstant]
  // CHECK:STDERR:   let (x: {}, y: {});
  // CHECK:STDERR:        ^~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_in_interface.carbon:[[@LINE+8]]:15: error: found runtime binding pattern in associated constant declaration; expected a `:!` binding [ExpectedSymbolicBindingInAssociatedConstant]
  // CHECK:STDERR:   let (x: {}, y: {});
  // CHECK:STDERR:               ^~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_in_interface.carbon:[[@LINE+4]]:7: error: found tuple pattern in associated constant declaration; expected symbolic binding pattern [ExpectedSingleBindingInAssociatedConstant]
  // CHECK:STDERR:   let (x: {}, y: {});
  // CHECK:STDERR:       ^~~~~~~~~~~~~~
  // CHECK:STDERR:
  let (x: {}, y: {});
}

// --- fail_in_class.carbon

library "[[@TEST_NAME]]";

class C {
  // CHECK:STDERR: fail_in_class.carbon:[[@LINE+8]]:7: error: expected identifier in field declaration [ExpectedFieldIdentifier]
  // CHECK:STDERR:   var (x: {}, y: {});
  // CHECK:STDERR:       ^
  // CHECK:STDERR:
  // CHECK:STDERR: fail_in_class.carbon:[[@LINE+4]]:3: error: semantics TODO: `handle invalid parse trees in `check`` [SemanticsTodo]
  // CHECK:STDERR:   var (x: {}, y: {});
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var (x: {}, y: {});
}

// --- fail_initializer_mismatch.carbon

library "[[@TEST_NAME]]";

// CHECK:STDERR: fail_initializer_mismatch.carbon:[[@LINE+4]]:5: error: tuple pattern expects 2 elements, but tuple literal has 1 [TuplePatternSizeDoesntMatchLiteral]
// CHECK:STDERR: let (x: {}, y: {}) = ({},);
// CHECK:STDERR:     ^~~~~~~~~~~~~~
// CHECK:STDERR:
let (x: {}, y: {}) = ({},);

// CHECK:STDERR: fail_initializer_mismatch.carbon:[[@LINE+4]]:5: error: tuple pattern expects 2 elements, but tuple literal has 3 [TuplePatternSizeDoesntMatchLiteral]
// CHECK:STDERR: let (a: {}, b: {}) = ({}, {}, {});
// CHECK:STDERR:     ^~~~~~~~~~~~~~
// CHECK:STDERR:
let (a: {}, b: {}) = ({}, {}, {});

// CHECK:STDOUT: --- basic.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %pattern_type.a96: type = pattern_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %tuple.type.b6b: type = tuple_type (%empty_struct_type, %empty_struct_type) [concrete]
// CHECK:STDOUT:   %pattern_type.de4: type = pattern_type %tuple.type.b6b [concrete]
// CHECK:STDOUT:   %empty_struct: %empty_struct_type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple: %tuple.type.b6b = tuple_value (%empty_struct, %empty_struct) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.72a: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%tuple.type.b6b) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.973: %T.as.Destroy.impl.Op.type.72a = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.8fc: type = ptr_type %tuple.type.b6b [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.a96 = binding_pattern x [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.a96 = binding_pattern y [concrete]
// CHECK:STDOUT:     %.loc6_20: %pattern_type.de4 = tuple_pattern (%x.patt, %y.patt) [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.loc6_26.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_30.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_31: %tuple.type.b6b = tuple_literal (%.loc6_26.1, %.loc6_30.1)
// CHECK:STDOUT:   %.loc6_12.1: type = splice_block %.loc6_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc6_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_12.3: type = converted %.loc6_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc6_26: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_26.2: %empty_struct_type = converted %.loc6_26.1, %empty_struct.loc6_26 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %x: %empty_struct_type = bind_name x, %.loc6_26.2
// CHECK:STDOUT:   %.loc6_19.1: type = splice_block %.loc6_19.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc6_19.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_19.3: type = converted %.loc6_19.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc6_30: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_30.2: %empty_struct_type = converted %.loc6_30.1, %empty_struct.loc6_30 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %y: %empty_struct_type = bind_name y, %.loc6_30.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %a.patt: %pattern_type.a96 = binding_pattern a [concrete]
// CHECK:STDOUT:     %b.patt: %pattern_type.a96 = binding_pattern b [concrete]
// CHECK:STDOUT:     %.loc7_20: %pattern_type.de4 = tuple_pattern (%a.patt, %b.patt) [concrete]
// CHECK:STDOUT:     %.var_patt.loc7: %pattern_type.de4 = var_pattern %.loc7_20 [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.var.loc7: ref %tuple.type.b6b = var %.var_patt.loc7
// CHECK:STDOUT:   %tuple.elem0.loc7: ref %empty_struct_type = tuple_access %.var.loc7, element0
// CHECK:STDOUT:   %tuple.elem1.loc7: ref %empty_struct_type = tuple_access %.var.loc7, element1
// CHECK:STDOUT:   %.loc7_12.1: type = splice_block %.loc7_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc7_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc7_12.3: type = converted %.loc7_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a: ref %empty_struct_type = bind_name a, %tuple.elem0.loc7
// CHECK:STDOUT:   %.loc7_19.1: type = splice_block %.loc7_19.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc7_19.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc7_19.3: type = converted %.loc7_19.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b: ref %empty_struct_type = bind_name b, %tuple.elem1.loc7
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %c.patt: %pattern_type.a96 = binding_pattern c [concrete]
// CHECK:STDOUT:     %d.patt: %pattern_type.a96 = binding_pattern d [concrete]
// CHECK:STDOUT:     %.loc8_20: %pattern_type.de4 = tuple_pattern (%c.patt, %d.patt) [concrete]
// CHECK:STDOUT:     %.var_patt.loc8: %pattern_type.de4 = var_pattern %.loc8_20 [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.var.loc8: ref %tuple.type.b6b = var %.var_patt.loc8
// CHECK:STDOUT:   %.loc8_26.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc8_30.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc8_31.1: %tuple.type.b6b = tuple_literal (%.loc8_26.1, %.loc8_30.1)
// CHECK:STDOUT:   %tuple.elem0.loc8_31: ref %empty_struct_type = tuple_access %.var.loc8, element0
// CHECK:STDOUT:   %.loc8_26.2: init %empty_struct_type = struct_init () to %tuple.elem0.loc8_31 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc8_31.2: init %empty_struct_type = converted %.loc8_26.1, %.loc8_26.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %tuple.elem1.loc8_31: ref %empty_struct_type = tuple_access %.var.loc8, element1
// CHECK:STDOUT:   %.loc8_30.2: init %empty_struct_type = struct_init () to %tuple.elem1.loc8_31 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc8_31.3: init %empty_struct_type = converted %.loc8_30.1, %.loc8_30.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc8_31.4: init %tuple.type.b6b = tuple_init (%.loc8_31.2, %.loc8_31.3) to %.var.loc8 [concrete = constants.%tuple]
// CHECK:STDOUT:   %.loc8_3: init %tuple.type.b6b = converted %.loc8_31.1, %.loc8_31.4 [concrete = constants.%tuple]
// CHECK:STDOUT:   assign %.var.loc8, %.loc8_3
// CHECK:STDOUT:   %tuple.elem0.loc8_3: ref %empty_struct_type = tuple_access %.var.loc8, element0
// CHECK:STDOUT:   %tuple.elem1.loc8_3: ref %empty_struct_type = tuple_access %.var.loc8, element1
// CHECK:STDOUT:   %.loc8_12.1: type = splice_block %.loc8_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc8_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc8_12.3: type = converted %.loc8_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c: ref %empty_struct_type = bind_name c, %tuple.elem0.loc8_3
// CHECK:STDOUT:   %.loc8_19.1: type = splice_block %.loc8_19.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc8_19.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc8_19.3: type = converted %.loc8_19.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %d: ref %empty_struct_type = bind_name d, %tuple.elem1.loc8_3
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc8: <bound method> = bound_method %.var.loc8, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method.loc8: <bound method> = bound_method %.var.loc8, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc8: %ptr.8fc = addr_of %.var.loc8
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc8: init %empty_tuple.type = call %bound_method.loc8(%addr.loc8)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc7: <bound method> = bound_method %.var.loc7, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method.loc7: <bound method> = bound_method %.var.loc7, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc7: %ptr.8fc = addr_of %.var.loc7
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc7: init %empty_tuple.type = call %bound_method.loc7(%addr.loc7)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- variable.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %tuple.type.b6b: type = tuple_type (%empty_struct_type, %empty_struct_type) [concrete]
// CHECK:STDOUT:   %pattern_type.de4: type = pattern_type %tuple.type.b6b [concrete]
// CHECK:STDOUT:   %empty_struct: %empty_struct_type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple: %tuple.type.b6b = tuple_value (%empty_struct, %empty_struct) [concrete]
// CHECK:STDOUT:   %pattern_type.a96: type = pattern_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.72a: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%tuple.type.b6b) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.973: %T.as.Destroy.impl.Op.type.72a = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.8fc: type = ptr_type %tuple.type.b6b [concrete]
// CHECK:STDOUT:   %MakeTuple.type: type = fn_type @MakeTuple [concrete]
// CHECK:STDOUT:   %MakeTuple: %MakeTuple.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %tuple.patt: %pattern_type.de4 = binding_pattern tuple [concrete]
// CHECK:STDOUT:     %tuple.var_patt: %pattern_type.de4 = var_pattern %tuple.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %tuple.var: ref %tuple.type.b6b = var %tuple.var_patt
// CHECK:STDOUT:   %.loc6_27.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_31.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_32.1: %tuple.type.b6b = tuple_literal (%.loc6_27.1, %.loc6_31.1)
// CHECK:STDOUT:   %tuple.elem0.loc6: ref %empty_struct_type = tuple_access %tuple.var, element0
// CHECK:STDOUT:   %.loc6_27.2: init %empty_struct_type = struct_init () to %tuple.elem0.loc6 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_32.2: init %empty_struct_type = converted %.loc6_27.1, %.loc6_27.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %tuple.elem1.loc6: ref %empty_struct_type = tuple_access %tuple.var, element1
// CHECK:STDOUT:   %.loc6_31.2: init %empty_struct_type = struct_init () to %tuple.elem1.loc6 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_32.3: init %empty_struct_type = converted %.loc6_31.1, %.loc6_31.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_32.4: init %tuple.type.b6b = tuple_init (%.loc6_32.2, %.loc6_32.3) to %tuple.var [concrete = constants.%tuple]
// CHECK:STDOUT:   %.loc6_3: init %tuple.type.b6b = converted %.loc6_32.1, %.loc6_32.4 [concrete = constants.%tuple]
// CHECK:STDOUT:   assign %tuple.var, %.loc6_3
// CHECK:STDOUT:   %.loc6_21.1: type = splice_block %.loc6_21.5 [concrete = constants.%tuple.type.b6b] {
// CHECK:STDOUT:     %.loc6_16: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_20: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_21.2: %tuple.type.b6b = tuple_literal (%.loc6_16, %.loc6_20)
// CHECK:STDOUT:     %.loc6_21.3: type = converted %.loc6_16, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %.loc6_21.4: type = converted %.loc6_20, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %.loc6_21.5: type = converted %.loc6_21.2, constants.%tuple.type.b6b [concrete = constants.%tuple.type.b6b]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %tuple: ref %tuple.type.b6b = bind_name tuple, %tuple.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.a96 = binding_pattern x [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.a96 = binding_pattern y [concrete]
// CHECK:STDOUT:     %.loc7_20: %pattern_type.de4 = tuple_pattern (%x.patt, %y.patt) [concrete]
// CHECK:STDOUT:     %.var_patt: %pattern_type.de4 = var_pattern %.loc7_20 [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.var: ref %tuple.type.b6b = var %.var_patt
// CHECK:STDOUT:   %tuple.ref: ref %tuple.type.b6b = name_ref tuple, %tuple
// CHECK:STDOUT:   %tuple.elem0.loc7_24.1: ref %empty_struct_type = tuple_access %tuple.ref, element0
// CHECK:STDOUT:   %tuple.elem0.loc7_24.2: ref %empty_struct_type = tuple_access %.var, element0
// CHECK:STDOUT:   %.loc7_24.1: init %empty_struct_type = struct_init () to %tuple.elem0.loc7_24.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc7_24.2: init %empty_struct_type = converted %tuple.elem0.loc7_24.1, %.loc7_24.1 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %tuple.elem1.loc7_24.1: ref %empty_struct_type = tuple_access %tuple.ref, element1
// CHECK:STDOUT:   %tuple.elem1.loc7_24.2: ref %empty_struct_type = tuple_access %.var, element1
// CHECK:STDOUT:   %.loc7_24.3: init %empty_struct_type = struct_init () to %tuple.elem1.loc7_24.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc7_24.4: init %empty_struct_type = converted %tuple.elem1.loc7_24.1, %.loc7_24.3 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc7_24.5: init %tuple.type.b6b = tuple_init (%.loc7_24.2, %.loc7_24.4) to %.var [concrete = constants.%tuple]
// CHECK:STDOUT:   %.loc7_3: init %tuple.type.b6b = converted %tuple.ref, %.loc7_24.5 [concrete = constants.%tuple]
// CHECK:STDOUT:   assign %.var, %.loc7_3
// CHECK:STDOUT:   %tuple.elem0.loc7_3: ref %empty_struct_type = tuple_access %.var, element0
// CHECK:STDOUT:   %tuple.elem1.loc7_3: ref %empty_struct_type = tuple_access %.var, element1
// CHECK:STDOUT:   %.loc7_12.1: type = splice_block %.loc7_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc7_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc7_12.3: type = converted %.loc7_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %x: ref %empty_struct_type = bind_name x, %tuple.elem0.loc7_3
// CHECK:STDOUT:   %.loc7_19.1: type = splice_block %.loc7_19.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc7_19.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc7_19.3: type = converted %.loc7_19.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %y: ref %empty_struct_type = bind_name y, %tuple.elem1.loc7_3
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc7: <bound method> = bound_method %.var, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method.loc7: <bound method> = bound_method %.var, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc7: %ptr.8fc = addr_of %.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc7: init %empty_tuple.type = call %bound_method.loc7(%addr.loc7)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc6: <bound method> = bound_method %tuple.var, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method.loc6: <bound method> = bound_method %tuple.var, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc6: %ptr.8fc = addr_of %tuple.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc6: init %empty_tuple.type = call %bound_method.loc6(%addr.loc6)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @G() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %tuple.patt: %pattern_type.de4 = binding_pattern tuple [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.loc13_27: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc13_31: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc13_32.1: %tuple.type.b6b = tuple_literal (%.loc13_27, %.loc13_31)
// CHECK:STDOUT:   %.loc13_21.1: type = splice_block %.loc13_21.5 [concrete = constants.%tuple.type.b6b] {
// CHECK:STDOUT:     %.loc13_16: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc13_20: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc13_21.2: %tuple.type.b6b = tuple_literal (%.loc13_16, %.loc13_20)
// CHECK:STDOUT:     %.loc13_21.3: type = converted %.loc13_16, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %.loc13_21.4: type = converted %.loc13_20, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %.loc13_21.5: type = converted %.loc13_21.2, constants.%tuple.type.b6b [concrete = constants.%tuple.type.b6b]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc13_27: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc13_32.2: %empty_struct_type = converted %.loc13_27, %empty_struct.loc13_27 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %empty_struct.loc13_31: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc13_32.3: %empty_struct_type = converted %.loc13_31, %empty_struct.loc13_31 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %tuple.loc13_32: %tuple.type.b6b = tuple_value (%.loc13_32.2, %.loc13_32.3) [concrete = constants.%tuple]
// CHECK:STDOUT:   %.loc13_32.4: %tuple.type.b6b = converted %.loc13_32.1, %tuple.loc13_32 [concrete = constants.%tuple]
// CHECK:STDOUT:   %tuple.loc13_7: %tuple.type.b6b = bind_name tuple, %.loc13_32.4
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.a96 = binding_pattern x [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.a96 = binding_pattern y [concrete]
// CHECK:STDOUT:     %.loc14_20: %pattern_type.de4 = tuple_pattern (%x.patt, %y.patt) [concrete]
// CHECK:STDOUT:     %.var_patt: %pattern_type.de4 = var_pattern %.loc14_20 [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.var: ref %tuple.type.b6b = var %.var_patt
// CHECK:STDOUT:   %tuple.ref: %tuple.type.b6b = name_ref tuple, %tuple.loc13_7
// CHECK:STDOUT:   %tuple.elem0.loc14_24.1: %empty_struct_type = tuple_access %tuple.ref, element0
// CHECK:STDOUT:   %tuple.elem0.loc14_24.2: ref %empty_struct_type = tuple_access %.var, element0
// CHECK:STDOUT:   %.loc14_24.1: init %empty_struct_type = struct_init () to %tuple.elem0.loc14_24.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc14_24.2: init %empty_struct_type = converted %tuple.elem0.loc14_24.1, %.loc14_24.1 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %tuple.elem1.loc14_24.1: %empty_struct_type = tuple_access %tuple.ref, element1
// CHECK:STDOUT:   %tuple.elem1.loc14_24.2: ref %empty_struct_type = tuple_access %.var, element1
// CHECK:STDOUT:   %.loc14_24.3: init %empty_struct_type = struct_init () to %tuple.elem1.loc14_24.2 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc14_24.4: init %empty_struct_type = converted %tuple.elem1.loc14_24.1, %.loc14_24.3 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc14_24.5: init %tuple.type.b6b = tuple_init (%.loc14_24.2, %.loc14_24.4) to %.var [concrete = constants.%tuple]
// CHECK:STDOUT:   %.loc14_3: init %tuple.type.b6b = converted %tuple.ref, %.loc14_24.5 [concrete = constants.%tuple]
// CHECK:STDOUT:   assign %.var, %.loc14_3
// CHECK:STDOUT:   %tuple.elem0.loc14_3: ref %empty_struct_type = tuple_access %.var, element0
// CHECK:STDOUT:   %tuple.elem1.loc14_3: ref %empty_struct_type = tuple_access %.var, element1
// CHECK:STDOUT:   %.loc14_12.1: type = splice_block %.loc14_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc14_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc14_12.3: type = converted %.loc14_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %x: ref %empty_struct_type = bind_name x, %tuple.elem0.loc14_3
// CHECK:STDOUT:   %.loc14_19.1: type = splice_block %.loc14_19.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc14_19.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc14_19.3: type = converted %.loc14_19.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %y: ref %empty_struct_type = bind_name y, %tuple.elem1.loc14_3
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound: <bound method> = bound_method %.var, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %.var, %T.as.Destroy.impl.Op.specific_fn
// CHECK:STDOUT:   %addr: %ptr.8fc = addr_of %.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call: init %empty_tuple.type = call %bound_method(%addr)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @H() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.a96 = binding_pattern x [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.a96 = binding_pattern y [concrete]
// CHECK:STDOUT:     %.loc22_20: %pattern_type.de4 = tuple_pattern (%x.patt, %y.patt) [concrete]
// CHECK:STDOUT:     %.var_patt: %pattern_type.de4 = var_pattern %.loc22_20 [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.var: ref %tuple.type.b6b = var %.var_patt
// CHECK:STDOUT:   %MakeTuple.ref: %MakeTuple.type = name_ref MakeTuple, file.%MakeTuple.decl [concrete = constants.%MakeTuple]
// CHECK:STDOUT:   %.loc22_3: ref %tuple.type.b6b = splice_block %.var {}
// CHECK:STDOUT:   %MakeTuple.call: init %tuple.type.b6b = call %MakeTuple.ref() to %.loc22_3
// CHECK:STDOUT:   assign %.var, %MakeTuple.call
// CHECK:STDOUT:   %tuple.elem0: ref %empty_struct_type = tuple_access %.var, element0
// CHECK:STDOUT:   %tuple.elem1: ref %empty_struct_type = tuple_access %.var, element1
// CHECK:STDOUT:   %.loc22_12.1: type = splice_block %.loc22_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc22_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc22_12.3: type = converted %.loc22_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %x: ref %empty_struct_type = bind_name x, %tuple.elem0
// CHECK:STDOUT:   %.loc22_19.1: type = splice_block %.loc22_19.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc22_19.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc22_19.3: type = converted %.loc22_19.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %y: ref %empty_struct_type = bind_name y, %tuple.elem1
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc22_3.1: <bound method> = bound_method %.loc22_3, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method.loc22_3.1: <bound method> = bound_method %.loc22_3, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc22_3.1: %ptr.8fc = addr_of %.loc22_3
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc22_3.1: init %empty_tuple.type = call %bound_method.loc22_3.1(%addr.loc22_3.1)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc22_3.2: <bound method> = bound_method %.var, constants.%T.as.Destroy.impl.Op.973
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method.loc22_3.2: <bound method> = bound_method %.var, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc22_3.2: %ptr.8fc = addr_of %.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc22_3.2: init %empty_tuple.type = call %bound_method.loc22_3.2(%addr.loc22_3.2)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- nested.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %pattern_type.a96: type = pattern_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %tuple.type.b6b: type = tuple_type (%empty_struct_type, %empty_struct_type) [concrete]
// CHECK:STDOUT:   %pattern_type.de4: type = pattern_type %tuple.type.b6b [concrete]
// CHECK:STDOUT:   %tuple.type.6ca: type = tuple_type (%empty_struct_type, %tuple.type.b6b) [concrete]
// CHECK:STDOUT:   %pattern_type.361: type = pattern_type %tuple.type.6ca [concrete]
// CHECK:STDOUT:   %empty_struct: %empty_struct_type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.a96 = binding_pattern x [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.a96 = binding_pattern y [concrete]
// CHECK:STDOUT:     %z.patt: %pattern_type.a96 = binding_pattern z [concrete]
// CHECK:STDOUT:     %.loc6_28: %pattern_type.de4 = tuple_pattern (%y.patt, %z.patt) [concrete]
// CHECK:STDOUT:     %.loc6_29: %pattern_type.361 = tuple_pattern (%x.patt, %.loc6_28) [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.loc6_35.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_40.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_44.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc6_45: %tuple.type.b6b = tuple_literal (%.loc6_40.1, %.loc6_44.1)
// CHECK:STDOUT:   %.loc6_46: %tuple.type.6ca = tuple_literal (%.loc6_35.1, %.loc6_45)
// CHECK:STDOUT:   %.loc6_12.1: type = splice_block %.loc6_12.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc6_12.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_12.3: type = converted %.loc6_12.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc6_35: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_35.2: %empty_struct_type = converted %.loc6_35.1, %empty_struct.loc6_35 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %x: %empty_struct_type = bind_name x, %.loc6_35.2
// CHECK:STDOUT:   %.loc6_20.1: type = splice_block %.loc6_20.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc6_20.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_20.3: type = converted %.loc6_20.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc6_40: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_40.2: %empty_struct_type = converted %.loc6_40.1, %empty_struct.loc6_40 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %y: %empty_struct_type = bind_name y, %.loc6_40.2
// CHECK:STDOUT:   %.loc6_27.1: type = splice_block %.loc6_27.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc6_27.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_27.3: type = converted %.loc6_27.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc6_44: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc6_44.2: %empty_struct_type = converted %.loc6_44.1, %empty_struct.loc6_44 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %z: %empty_struct_type = bind_name z, %.loc6_44.2
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- package_scope.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %pattern_type.a96: type = pattern_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %tuple.type: type = tuple_type (%empty_struct_type, %empty_struct_type) [concrete]
// CHECK:STDOUT:   %pattern_type.de4: type = pattern_type %tuple.type [concrete]
// CHECK:STDOUT:   %empty_struct: %empty_struct_type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.a96 = binding_pattern x [concrete]
// CHECK:STDOUT:     %y.patt: %pattern_type.a96 = binding_pattern y [concrete]
// CHECK:STDOUT:     %.loc5_18: %pattern_type.de4 = tuple_pattern (%x.patt, %y.patt) [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.loc5_10.1: type = splice_block %.loc5_10.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc5_10.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc5_10.3: type = converted %.loc5_10.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc5_24: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc5_24: %empty_struct_type = converted @__global_init.%.loc5_24, %empty_struct.loc5_24 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %x: %empty_struct_type = bind_name x, %.loc5_24
// CHECK:STDOUT:   %.loc5_17.1: type = splice_block %.loc5_17.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc5_17.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc5_17.3: type = converted %.loc5_17.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct.loc5_28: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %.loc5_28: %empty_struct_type = converted @__global_init.%.loc5_28, %empty_struct.loc5_28 [concrete = constants.%empty_struct]
// CHECK:STDOUT:   %y: %empty_struct_type = bind_name y, %.loc5_28
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %.loc5_24: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc5_28: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:   %.loc5_29: %tuple.type = tuple_literal (%.loc5_24, %.loc5_28)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
