// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/none.carbon
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/alias/import.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/alias/import.carbon

// --- class1.carbon

library "[[@TEST_NAME]]";

class C {}

alias c_alias = C;

var a: C*;

// --- class2.carbon

library "[[@TEST_NAME]]";

import library "class1";

alias c_alias_alias = c_alias;

var b: c_alias*;

// --- class3.carbon

library "[[@TEST_NAME]]";

import library "class2";

var c: c_alias_alias*;

// --- var1.carbon

library "[[@TEST_NAME]]";

var a: () = ();

alias a_alias = a;

// --- var2.carbon

library "[[@TEST_NAME]]";

import library "var1";

alias a_alias_alias = a_alias;

var b: () = a_alias;

// --- var3.carbon

library "[[@TEST_NAME]]";

import library "var2";

var c: () = a_alias_alias;

// CHECK:STDOUT: --- class1.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %ptr: type = ptr_type %C [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %ptr [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .c_alias = %c_alias
// CHECK:STDOUT:     .a = %a
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [concrete = constants.%C] {} {}
// CHECK:STDOUT:   %C.ref.loc6: type = name_ref C, %C.decl [concrete = constants.%C]
// CHECK:STDOUT:   %c_alias: type = bind_alias c_alias, %C.decl [concrete = constants.%C]
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %a.patt: %pattern_type = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.var_patt: %pattern_type = var_pattern %a.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a.var: ref %ptr = var %a.var_patt [concrete]
// CHECK:STDOUT:   %.loc8: type = splice_block %ptr [concrete = constants.%ptr] {
// CHECK:STDOUT:     %C.ref.loc8: type = name_ref C, %C.decl [concrete = constants.%C]
// CHECK:STDOUT:     %ptr: type = ptr_type %C.ref.loc8 [concrete = constants.%ptr]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a: ref %ptr = bind_name a, %a.var [concrete = %a.var]
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
// CHECK:STDOUT: --- class2.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %ptr: type = ptr_type %C [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %ptr [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.C = import_ref Main//class1, C, unloaded
// CHECK:STDOUT:   %Main.c_alias: type = import_ref Main//class1, c_alias, loaded [concrete = constants.%C]
// CHECK:STDOUT:   %Main.a = import_ref Main//class1, a, unloaded
// CHECK:STDOUT:   %Main.import_ref.8f2: <witness> = import_ref Main//class1, loc4_10, loaded [concrete = constants.%complete_type]
// CHECK:STDOUT:   %Main.import_ref.2c4 = import_ref Main//class1, inst16 [no loc], unloaded
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .C = imports.%Main.C
// CHECK:STDOUT:     .c_alias = imports.%Main.c_alias
// CHECK:STDOUT:     .a = imports.%Main.a
// CHECK:STDOUT:     .c_alias_alias = %c_alias_alias
// CHECK:STDOUT:     .b = %b
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   %c_alias.ref.loc6: type = name_ref c_alias, imports.%Main.c_alias [concrete = constants.%C]
// CHECK:STDOUT:   %c_alias_alias: type = bind_alias c_alias_alias, imports.%Main.c_alias [concrete = constants.%C]
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %b.patt: %pattern_type = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.var_patt: %pattern_type = var_pattern %b.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b.var: ref %ptr = var %b.var_patt [concrete]
// CHECK:STDOUT:   %.loc8: type = splice_block %ptr [concrete = constants.%ptr] {
// CHECK:STDOUT:     %c_alias.ref.loc8: type = name_ref c_alias, imports.%Main.c_alias [concrete = constants.%C]
// CHECK:STDOUT:     %ptr: type = ptr_type %c_alias.ref.loc8 [concrete = constants.%ptr]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b: ref %ptr = bind_name b, %b.var [concrete = %b.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C [from "class1.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.8f2
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.2c4
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- class3.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %ptr: type = ptr_type %C [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %ptr [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.c_alias_alias: type = import_ref Main//class2, c_alias_alias, loaded [concrete = constants.%C]
// CHECK:STDOUT:   %Main.b = import_ref Main//class2, b, unloaded
// CHECK:STDOUT:   %Main.import_ref.8db: <witness> = import_ref Main//class2, inst23 [indirect], loaded [concrete = constants.%complete_type]
// CHECK:STDOUT:   %Main.import_ref.6a9 = import_ref Main//class2, inst24 [indirect], unloaded
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .c_alias_alias = imports.%Main.c_alias_alias
// CHECK:STDOUT:     .b = imports.%Main.b
// CHECK:STDOUT:     .c = %c
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %c.patt: %pattern_type = binding_pattern c [concrete]
// CHECK:STDOUT:     %c.var_patt: %pattern_type = var_pattern %c.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c.var: ref %ptr = var %c.var_patt [concrete]
// CHECK:STDOUT:   %.loc6: type = splice_block %ptr [concrete = constants.%ptr] {
// CHECK:STDOUT:     %c_alias_alias.ref: type = name_ref c_alias_alias, imports.%Main.c_alias_alias [concrete = constants.%C]
// CHECK:STDOUT:     %ptr: type = ptr_type %c_alias_alias.ref [concrete = constants.%ptr]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c: ref %ptr = bind_name c, %c.var [concrete = %c.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C [from "class2.carbon"] {
// CHECK:STDOUT:   complete_type_witness = imports.%Main.import_ref.8db
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = imports.%Main.import_ref.6a9
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- var1.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .a = %a
// CHECK:STDOUT:     .a_alias = %a_alias
// CHECK:STDOUT:   }
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %a.patt: %pattern_type = binding_pattern a [concrete]
// CHECK:STDOUT:     %a.var_patt: %pattern_type = var_pattern %a.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a.var: ref %empty_tuple.type = var %a.var_patt [concrete]
// CHECK:STDOUT:   %.loc4_9.1: type = splice_block %.loc4_9.3 [concrete = constants.%empty_tuple.type] {
// CHECK:STDOUT:     %.loc4_9.2: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc4_9.3: type = converted %.loc4_9.2, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %a: ref %empty_tuple.type = bind_name a, %a.var [concrete = %a.var]
// CHECK:STDOUT:   %a.ref: ref %empty_tuple.type = name_ref a, %a [concrete = %a.var]
// CHECK:STDOUT:   %a_alias: ref %empty_tuple.type = bind_alias a_alias, %a [concrete = %a.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %.loc4_14.1: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:   %.loc4_14.2: init %empty_tuple.type = tuple_init () to file.%a.var [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %.loc4_1: init %empty_tuple.type = converted %.loc4_14.1, %.loc4_14.2 [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   assign file.%a.var, %.loc4_1
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- var2.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.a = import_ref Main//var1, a, unloaded
// CHECK:STDOUT:   %Main.a_alias: ref %empty_tuple.type = import_ref Main//var1, a_alias, loaded [concrete = %a.var]
// CHECK:STDOUT:   %a.patt: %pattern_type = binding_pattern a [concrete]
// CHECK:STDOUT:   %a.var_patt: %pattern_type = var_pattern %a.patt [concrete]
// CHECK:STDOUT:   %a.var: ref %empty_tuple.type = var %a.var_patt [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .a = imports.%Main.a
// CHECK:STDOUT:     .a_alias = imports.%Main.a_alias
// CHECK:STDOUT:     .a_alias_alias = %a_alias_alias
// CHECK:STDOUT:     .b = %b
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   %a_alias.ref: ref %empty_tuple.type = name_ref a_alias, imports.%Main.a_alias [concrete = imports.%a.var]
// CHECK:STDOUT:   %a_alias_alias: ref %empty_tuple.type = bind_alias a_alias_alias, imports.%Main.a_alias [concrete = imports.%a.var]
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %b.patt: %pattern_type = binding_pattern b [concrete]
// CHECK:STDOUT:     %b.var_patt: %pattern_type = var_pattern %b.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b.var: ref %empty_tuple.type = var %b.var_patt [concrete]
// CHECK:STDOUT:   %.loc8_9.1: type = splice_block %.loc8_9.3 [concrete = constants.%empty_tuple.type] {
// CHECK:STDOUT:     %.loc8_9.2: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc8_9.3: type = converted %.loc8_9.2, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %b: ref %empty_tuple.type = bind_name b, %b.var [concrete = %b.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a_alias.ref: ref %empty_tuple.type = name_ref a_alias, imports.%Main.a_alias [concrete = imports.%a.var]
// CHECK:STDOUT:   %.loc8_13: init %empty_tuple.type = tuple_init () to file.%b.var [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %.loc8_1: init %empty_tuple.type = converted %a_alias.ref, %.loc8_13 [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   assign file.%b.var, %.loc8_1
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- var3.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.a_alias_alias: ref %empty_tuple.type = import_ref Main//var2, a_alias_alias, loaded [concrete = %a.var]
// CHECK:STDOUT:   %Main.b = import_ref Main//var2, b, unloaded
// CHECK:STDOUT:   %a.patt: %pattern_type = binding_pattern a [concrete]
// CHECK:STDOUT:   %a.var_patt: %pattern_type = var_pattern %a.patt [concrete]
// CHECK:STDOUT:   %a.var: ref %empty_tuple.type = var %a.var_patt [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .a_alias_alias = imports.%Main.a_alias_alias
// CHECK:STDOUT:     .b = imports.%Main.b
// CHECK:STDOUT:     .c = %c
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %c.patt: %pattern_type = binding_pattern c [concrete]
// CHECK:STDOUT:     %c.var_patt: %pattern_type = var_pattern %c.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c.var: ref %empty_tuple.type = var %c.var_patt [concrete]
// CHECK:STDOUT:   %.loc6_9.1: type = splice_block %.loc6_9.3 [concrete = constants.%empty_tuple.type] {
// CHECK:STDOUT:     %.loc6_9.2: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc6_9.3: type = converted %.loc6_9.2, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %c: ref %empty_tuple.type = bind_name c, %c.var [concrete = %c.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %a_alias_alias.ref: ref %empty_tuple.type = name_ref a_alias_alias, imports.%Main.a_alias_alias [concrete = imports.%a.var]
// CHECK:STDOUT:   %.loc6_13: init %empty_tuple.type = tuple_init () to file.%c.var [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   %.loc6_1: init %empty_tuple.type = converted %a_alias_alias.ref, %.loc6_13 [concrete = constants.%empty_tuple]
// CHECK:STDOUT:   assign file.%c.var, %.loc6_1
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
