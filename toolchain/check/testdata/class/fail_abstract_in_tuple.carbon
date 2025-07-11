// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/destroy.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/fail_abstract_in_tuple.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/fail_abstract_in_tuple.carbon

// --- fail_abstract_field.carbon
library "[[@TEST_NAME]]";

abstract class Abstract1 {}

class Contains {
  // CHECK:STDERR: fail_abstract_field.carbon:[[@LINE+7]]:10: error: field has abstract type `(Abstract1,)` [AbstractTypeInFieldDecl]
  // CHECK:STDERR:   var a: (Abstract1,);
  // CHECK:STDERR:          ^~~~~~~~~~~~
  // CHECK:STDERR: fail_abstract_field.carbon:[[@LINE-6]]:1: note: uses class that was declared abstract here [ClassAbstractHere]
  // CHECK:STDERR: abstract class Abstract1 {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var a: (Abstract1,);
}

// --- fail_abstract_var.carbon
library "[[@TEST_NAME]]";

abstract class Abstract2 {}

fn Var() {
  // CHECK:STDERR: fail_abstract_var.carbon:[[@LINE+7]]:10: error: binding pattern has abstract type `(Abstract2,)` in `var` pattern [AbstractTypeInVarPattern]
  // CHECK:STDERR:   var v: (Abstract2,);
  // CHECK:STDERR:          ^~~~~~~~~~~~
  // CHECK:STDERR: fail_abstract_var.carbon:[[@LINE-6]]:1: note: uses class that was declared abstract here [ClassAbstractHere]
  // CHECK:STDERR: abstract class Abstract2 {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var v: (Abstract2,);
}

// --- abstract_let.carbon
library "[[@TEST_NAME]]";

abstract class Abstract3 {
}

fn F(a: Abstract3) {
  let l: (Abstract3,) = (a,);
}

// --- fail_abstract_twice.carbon
library "[[@TEST_NAME]]";

abstract class Abstract4 {}
abstract class Abstract5 {}

fn Var2() {
  // CHECK:STDERR: fail_abstract_twice.carbon:[[@LINE+7]]:11: error: binding pattern has abstract type `(Abstract4, Abstract5)` in `var` pattern [AbstractTypeInVarPattern]
  // CHECK:STDERR:   var v2: (Abstract4, Abstract5);
  // CHECK:STDERR:           ^~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_abstract_twice.carbon:[[@LINE-7]]:1: note: uses class that was declared abstract here [ClassAbstractHere]
  // CHECK:STDERR: abstract class Abstract4 {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var v2: (Abstract4, Abstract5);
}

// --- fail_abstract_first.carbon
library "[[@TEST_NAME]]";

abstract class Abstract6 {}

fn Var3() {
  // CHECK:STDERR: fail_abstract_first.carbon:[[@LINE+7]]:11: error: binding pattern has abstract type `(Abstract6, {})` in `var` pattern [AbstractTypeInVarPattern]
  // CHECK:STDERR:   var v3: (Abstract6, {});
  // CHECK:STDERR:           ^~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_abstract_first.carbon:[[@LINE-6]]:1: note: uses class that was declared abstract here [ClassAbstractHere]
  // CHECK:STDERR: abstract class Abstract6 {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var v3: (Abstract6, {});
}

// --- fail_abstract_second.carbon
library "[[@TEST_NAME]]";

abstract class Abstract7 {}

fn Var4() {
  // CHECK:STDERR: fail_abstract_second.carbon:[[@LINE+7]]:11: error: binding pattern has abstract type `({}, Abstract7)` in `var` pattern [AbstractTypeInVarPattern]
  // CHECK:STDERR:   var v4: ({}, Abstract7);
  // CHECK:STDERR:           ^~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_abstract_second.carbon:[[@LINE-6]]:1: note: uses class that was declared abstract here [ClassAbstractHere]
  // CHECK:STDERR: abstract class Abstract7 {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var v4: ({}, Abstract7);
}

// --- lib.carbon
library "[[@TEST_NAME]]";

abstract class Abstract {}

// --- fail_import.carbon
library "[[@TEST_NAME]]";

import library "lib";

fn Var5() {
  // CHECK:STDERR: fail_import.carbon:[[@LINE+8]]:11: error: binding pattern has abstract type `(Abstract,)` in `var` pattern [AbstractTypeInVarPattern]
  // CHECK:STDERR:   var v5: (Abstract,);
  // CHECK:STDERR:           ^~~~~~~~~~~
  // CHECK:STDERR: fail_import.carbon:[[@LINE-6]]:1: in import [InImport]
  // CHECK:STDERR: lib.carbon:3:1: note: uses class that was declared abstract here [ClassAbstractHere]
  // CHECK:STDERR: abstract class Abstract {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  var v5: (Abstract,);
}

// CHECK:STDOUT: --- fail_abstract_field.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract1: type = class_type @Abstract1 [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %Contains: type = class_type @Contains [concrete]
// CHECK:STDOUT:   %tuple.type.85c: type = tuple_type (type) [concrete]
// CHECK:STDOUT:   %tuple.type.f19: type = tuple_type (%Abstract1) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract1 = %Abstract1.decl
// CHECK:STDOUT:     .Contains = %Contains.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract1.decl: type = class_decl @Abstract1 [concrete = constants.%Abstract1] {} {}
// CHECK:STDOUT:   %Contains.decl: type = class_decl @Contains [concrete = constants.%Contains] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract1 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract1
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Contains {
// CHECK:STDOUT:   %Abstract1.ref: type = name_ref Abstract1, file.%Abstract1.decl [concrete = constants.%Abstract1]
// CHECK:STDOUT:   %.loc13_21.1: %tuple.type.85c = tuple_literal (%Abstract1.ref)
// CHECK:STDOUT:   %.loc13_21.2: type = converted %.loc13_21.1, constants.%tuple.type.f19 [concrete = constants.%tuple.type.f19]
// CHECK:STDOUT:   %.loc13_8: <error> = field_decl a, element0 [concrete]
// CHECK:STDOUT:   %struct_type.a: type = struct_type {.a: <error>} [concrete = <error>]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %struct_type.a [concrete = <error>]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Contains
// CHECK:STDOUT:   .Abstract1 = <poisoned>
// CHECK:STDOUT:   .a = %.loc13_8
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_abstract_var.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract2: type = class_type @Abstract2 [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %Var.type: type = fn_type @Var [concrete]
// CHECK:STDOUT:   %Var: %Var.type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple.type.85c: type = tuple_type (type) [concrete]
// CHECK:STDOUT:   %tuple.type.ac6: type = tuple_type (%Abstract2) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract2 = %Abstract2.decl
// CHECK:STDOUT:     .Var = %Var.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract2.decl: type = class_decl @Abstract2 [concrete = constants.%Abstract2] {} {}
// CHECK:STDOUT:   %Var.decl: %Var.type = fn_decl @Var [concrete = constants.%Var] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract2 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Var() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v.patt: <error> = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.var_patt: <error> = var_pattern %v.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v.var: ref <error> = var %v.var_patt [concrete = <error>]
// CHECK:STDOUT:   %.loc13_21.1: type = splice_block %.loc13_21.3 [concrete = constants.%tuple.type.ac6] {
// CHECK:STDOUT:     %Abstract2.ref: type = name_ref Abstract2, file.%Abstract2.decl [concrete = constants.%Abstract2]
// CHECK:STDOUT:     %.loc13_21.2: %tuple.type.85c = tuple_literal (%Abstract2.ref)
// CHECK:STDOUT:     %.loc13_21.3: type = converted %.loc13_21.2, constants.%tuple.type.ac6 [concrete = constants.%tuple.type.ac6]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v: <error> = bind_name v, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- abstract_let.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract3: type = class_type @Abstract3 [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %pattern_type.32b: type = pattern_type %Abstract3 [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple.type.85c: type = tuple_type (type) [concrete]
// CHECK:STDOUT:   %tuple.type.fa1: type = tuple_type (%Abstract3) [concrete]
// CHECK:STDOUT:   %pattern_type.3cf: type = pattern_type %tuple.type.fa1 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract3 = %Abstract3.decl
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract3.decl: type = class_decl @Abstract3 [concrete = constants.%Abstract3] {} {}
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {
// CHECK:STDOUT:     %a.patt: %pattern_type.32b = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.param_patt: %pattern_type.32b = value_param_pattern %a.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %a.param: %Abstract3 = value_param call_param0
// CHECK:STDOUT:     %Abstract3.ref.loc6: type = name_ref Abstract3, file.%Abstract3.decl [concrete = constants.%Abstract3]
// CHECK:STDOUT:     %a: %Abstract3 = bind_name a, %a.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract3 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract3
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F(%a.param: %Abstract3) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %l.patt: %pattern_type.3cf = binding_pattern l [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a.ref: %Abstract3 = name_ref a, %a
// CHECK:STDOUT:   %.loc7_28: %tuple.type.fa1 = tuple_literal (%a.ref)
// CHECK:STDOUT:   %.loc7_21.1: type = splice_block %.loc7_21.3 [concrete = constants.%tuple.type.fa1] {
// CHECK:STDOUT:     %Abstract3.ref.loc7: type = name_ref Abstract3, file.%Abstract3.decl [concrete = constants.%Abstract3]
// CHECK:STDOUT:     %.loc7_21.2: %tuple.type.85c = tuple_literal (%Abstract3.ref.loc7)
// CHECK:STDOUT:     %.loc7_21.3: type = converted %.loc7_21.2, constants.%tuple.type.fa1 [concrete = constants.%tuple.type.fa1]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %l: %tuple.type.fa1 = bind_name l, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_abstract_twice.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract4: type = class_type @Abstract4 [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %Abstract5: type = class_type @Abstract5 [concrete]
// CHECK:STDOUT:   %Var2.type: type = fn_type @Var2 [concrete]
// CHECK:STDOUT:   %Var2: %Var2.type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple.type.24b: type = tuple_type (type, type) [concrete]
// CHECK:STDOUT:   %tuple.type.e3e: type = tuple_type (%Abstract4, %Abstract5) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract4 = %Abstract4.decl
// CHECK:STDOUT:     .Abstract5 = %Abstract5.decl
// CHECK:STDOUT:     .Var2 = %Var2.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract4.decl: type = class_decl @Abstract4 [concrete = constants.%Abstract4] {} {}
// CHECK:STDOUT:   %Abstract5.decl: type = class_decl @Abstract5 [concrete = constants.%Abstract5] {} {}
// CHECK:STDOUT:   %Var2.decl: %Var2.type = fn_decl @Var2 [concrete = constants.%Var2] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract4 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract4
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract5 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract5
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Var2() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v2.patt: <error> = binding_pattern v2 [concrete]
// CHECK:STDOUT:     %v2.var_patt: <error> = var_pattern %v2.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v2.var: ref <error> = var %v2.var_patt [concrete = <error>]
// CHECK:STDOUT:   %.loc14_32.1: type = splice_block %.loc14_32.3 [concrete = constants.%tuple.type.e3e] {
// CHECK:STDOUT:     %Abstract4.ref: type = name_ref Abstract4, file.%Abstract4.decl [concrete = constants.%Abstract4]
// CHECK:STDOUT:     %Abstract5.ref: type = name_ref Abstract5, file.%Abstract5.decl [concrete = constants.%Abstract5]
// CHECK:STDOUT:     %.loc14_32.2: %tuple.type.24b = tuple_literal (%Abstract4.ref, %Abstract5.ref)
// CHECK:STDOUT:     %.loc14_32.3: type = converted %.loc14_32.2, constants.%tuple.type.e3e [concrete = constants.%tuple.type.e3e]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v2: <error> = bind_name v2, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_abstract_first.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract6: type = class_type @Abstract6 [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %Var3.type: type = fn_type @Var3 [concrete]
// CHECK:STDOUT:   %Var3: %Var3.type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple.type.159: type = tuple_type (type, %empty_struct_type) [concrete]
// CHECK:STDOUT:   %tuple.type.d93: type = tuple_type (%Abstract6, %empty_struct_type) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract6 = %Abstract6.decl
// CHECK:STDOUT:     .Var3 = %Var3.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract6.decl: type = class_decl @Abstract6 [concrete = constants.%Abstract6] {} {}
// CHECK:STDOUT:   %Var3.decl: %Var3.type = fn_decl @Var3 [concrete = constants.%Var3] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract6 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract6
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Var3() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v3.patt: <error> = binding_pattern v3 [concrete]
// CHECK:STDOUT:     %v3.var_patt: <error> = var_pattern %v3.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v3.var: ref <error> = var %v3.var_patt [concrete = <error>]
// CHECK:STDOUT:   %.loc13_25.1: type = splice_block %.loc13_25.4 [concrete = constants.%tuple.type.d93] {
// CHECK:STDOUT:     %Abstract6.ref: type = name_ref Abstract6, file.%Abstract6.decl [concrete = constants.%Abstract6]
// CHECK:STDOUT:     %.loc13_24: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc13_25.2: %tuple.type.159 = tuple_literal (%Abstract6.ref, %.loc13_24)
// CHECK:STDOUT:     %.loc13_25.3: type = converted %.loc13_24, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %.loc13_25.4: type = converted %.loc13_25.2, constants.%tuple.type.d93 [concrete = constants.%tuple.type.d93]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v3: <error> = bind_name v3, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_abstract_second.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract7: type = class_type @Abstract7 [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %Var4.type: type = fn_type @Var4 [concrete]
// CHECK:STDOUT:   %Var4: %Var4.type = struct_value () [concrete]
// CHECK:STDOUT:   %tuple.type.c8c: type = tuple_type (%empty_struct_type, type) [concrete]
// CHECK:STDOUT:   %tuple.type.919: type = tuple_type (%empty_struct_type, %Abstract7) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract7 = %Abstract7.decl
// CHECK:STDOUT:     .Var4 = %Var4.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract7.decl: type = class_decl @Abstract7 [concrete = constants.%Abstract7] {} {}
// CHECK:STDOUT:   %Var4.decl: %Var4.type = fn_decl @Var4 [concrete = constants.%Var4] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract7 {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract7
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Var4() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v4.patt: <error> = binding_pattern v4 [concrete]
// CHECK:STDOUT:     %v4.var_patt: <error> = var_pattern %v4.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v4.var: ref <error> = var %v4.var_patt [concrete = <error>]
// CHECK:STDOUT:   %.loc13_25.1: type = splice_block %.loc13_25.4 [concrete = constants.%tuple.type.919] {
// CHECK:STDOUT:     %.loc13_13: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %Abstract7.ref: type = name_ref Abstract7, file.%Abstract7.decl [concrete = constants.%Abstract7]
// CHECK:STDOUT:     %.loc13_25.2: %tuple.type.c8c = tuple_literal (%.loc13_13, %Abstract7.ref)
// CHECK:STDOUT:     %.loc13_25.3: type = converted %.loc13_13, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %.loc13_25.4: type = converted %.loc13_25.2, constants.%tuple.type.919 [concrete = constants.%tuple.type.919]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v4: <error> = bind_name v4, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- lib.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Abstract: type = class_type @Abstract [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Abstract = %Abstract.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Abstract.decl: type = class_decl @Abstract [concrete = constants.%Abstract] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Abstract
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_import.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Var5.type: type = fn_type @Var5 [concrete]
// CHECK:STDOUT:   %Var5: %Var5.type = struct_value () [concrete]
// CHECK:STDOUT:   %Abstract: type = class_type @Abstract [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %tuple.type.85c: type = tuple_type (type) [concrete]
// CHECK:STDOUT:   %tuple.type.555: type = tuple_type (%Abstract) [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.Abstract: type = import_ref Main//lib, Abstract, loaded [concrete = constants.%Abstract]
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Main.import_ref.8f2: <witness> = import_ref Main//lib, loc3_26, loaded [concrete = constants.%complete_type]
// CHECK:STDOUT:   %Main.import_ref.ee1 = import_ref Main//lib, inst18 [no loc], unloaded
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Abstract = imports.%Main.Abstract
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Var5 = %Var5.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   %Var5.decl: %Var5.type = fn_decl @Var5 [concrete = constants.%Var5] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Abstract [from "lib.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.8f2
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.ee1
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Var5() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %v5.patt: <error> = binding_pattern v5 [concrete]
// CHECK:STDOUT:     %v5.var_patt: <error> = var_pattern %v5.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v5.var: ref <error> = var %v5.var_patt [concrete = <error>]
// CHECK:STDOUT:   %.loc14_21.1: type = splice_block %.loc14_21.3 [concrete = constants.%tuple.type.555] {
// CHECK:STDOUT:     %Abstract.ref: type = name_ref Abstract, imports.%Main.Abstract [concrete = constants.%Abstract]
// CHECK:STDOUT:     %.loc14_21.2: %tuple.type.85c = tuple_literal (%Abstract.ref)
// CHECK:STDOUT:     %.loc14_21.3: type = converted %.loc14_21.2, constants.%tuple.type.555 [concrete = constants.%tuple.type.555]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %v5: <error> = bind_name v5, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
