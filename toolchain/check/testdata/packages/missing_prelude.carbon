// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/none.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/packages/missing_prelude.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/packages/missing_prelude.carbon

// --- fail_missing_prelude.carbon

library "[[@TEST_NAME]]";

// CHECK:STDERR: fail_missing_prelude.carbon:[[@LINE+4]]:8: error: `Core.Int` implicitly referenced here, but package `Core` not found [CoreNotFound]
// CHECK:STDERR: var n: i32;
// CHECK:STDERR:        ^~~
// CHECK:STDERR:
var n: i32;

// --- prelude_empty.carbon

package Core library "[[@TEST_NAME]]";

// --- fail_missing_prelude_member.carbon

library "[[@TEST_NAME]]";

import Core library "prelude_empty";

// CHECK:STDERR: fail_missing_prelude_member.carbon:[[@LINE+4]]:8: error: name `Core.Int` implicitly referenced here, but not found [CoreNameNotFound]
// CHECK:STDERR: var n: i32;
// CHECK:STDERR:        ^~~
// CHECK:STDERR:
var n: i32;

// --- prelude_fake_int.carbon

package Core library "[[@TEST_NAME]]";

fn Int[T:! type](N:! T) -> {} { return {}; }

// --- use_fake_int.carbon

library "[[@TEST_NAME]]";

import Core library "prelude_fake_int";

var n: {} = i32;

// --- prelude_use_in_prelude.carbon

package Core library "[[@TEST_NAME]]";

// Core is not an imported package here.

// Note that we are intentionally using a different type here from other cases
// to test that this function is called, not the one from prelude_fake_int.
fn Int[T:! type](N:! T) -> () { return (); }

var n: () = i32;

// --- fail_prelude_as_namespace.carbon

library "[[@TEST_NAME]]";

// CHECK:STDERR: fail_prelude_as_namespace.carbon:[[@LINE+8]]:11: error: `namespace` introducer should be followed by a name [ExpectedDeclName]
// CHECK:STDERR: namespace Core;
// CHECK:STDERR:           ^~~~
// CHECK:STDERR:
// CHECK:STDERR: fail_prelude_as_namespace.carbon:[[@LINE+4]]:11: error: semantics TODO: `handle invalid parse trees in `check`` [SemanticsTodo]
// CHECK:STDERR: namespace Core;
// CHECK:STDERR:           ^~~~
// CHECK:STDERR:
namespace Core;

// CHECK:STDERR: fail_prelude_as_namespace.carbon:[[@LINE+4]]:4: error: `fn` introducer should be followed by a name [ExpectedDeclName]
// CHECK:STDERR: fn Core.Int[T:! type](N:! T) -> {} { return {}; }
// CHECK:STDERR:    ^~~~
// CHECK:STDERR:
fn Core.Int[T:! type](N:! T) -> {} { return {}; }

var n: {} = i32;

// --- fail_prelude_as_class.carbon

library "[[@TEST_NAME]]";

// CHECK:STDERR: fail_prelude_as_class.carbon:[[@LINE+8]]:7: error: `class` introducer should be followed by a name [ExpectedDeclName]
// CHECK:STDERR: class Core {
// CHECK:STDERR:       ^~~~
// CHECK:STDERR:
// CHECK:STDERR: fail_prelude_as_class.carbon:[[@LINE+4]]:7: error: semantics TODO: `handle invalid parse trees in `check`` [SemanticsTodo]
// CHECK:STDERR: class Core {
// CHECK:STDERR:       ^~~~
// CHECK:STDERR:
class Core {
  fn Int[T:! type](N:! T) -> {} { return {}; }
}

var n: {} = i32;

// CHECK:STDOUT: --- fail_missing_prelude.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .n = %n
// CHECK:STDOUT:   }
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %n.patt: <error> = binding_pattern n [concrete]
// CHECK:STDOUT:     %n.var_patt: <error> = var_pattern %n.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %n.var: ref <error> = var %n.var_patt [concrete = <error>]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %n: <error> = bind_name n, <error> [concrete = <error>]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- prelude_empty.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_missing_prelude_member.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = <poisoned>
// CHECK:STDOUT:     import Core//prelude_empty
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .n = %n
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %n.patt: <error> = binding_pattern n [concrete]
// CHECK:STDOUT:     %n.var_patt: <error> = var_pattern %n.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %n.var: ref <error> = var %n.var_patt [concrete = <error>]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %n: <error> = bind_name n, <error> [concrete = <error>]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- prelude_fake_int.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %N: %T = bind_symbolic_name N, 1 [symbolic]
// CHECK:STDOUT:   %pattern_type.7dc: type = pattern_type %T [symbolic]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %pattern_type.a96: type = pattern_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %empty_struct: %empty_struct_type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Int = %Int.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Int.decl: %Int.type = fn_decl @Int [concrete = constants.%Int] {
// CHECK:STDOUT:     %T.patt: %pattern_type.98f = symbolic_binding_pattern T, 0 [concrete]
// CHECK:STDOUT:     %N.patt: @Int.%pattern_type (%pattern_type.7dc) = symbolic_binding_pattern N, 1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.a96 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.a96 = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %.loc4_29.1: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc4_29.2: type = converted %.loc4_29.1, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %T.loc4_8.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc4_8.1 (constants.%T)]
// CHECK:STDOUT:     %T.ref: type = name_ref T, %T.loc4_8.2 [symbolic = %T.loc4_8.1 (constants.%T)]
// CHECK:STDOUT:     %N.loc4_18.2: @Int.%T.loc4_8.1 (%T) = bind_symbolic_name N, 1 [symbolic = %N.loc4_18.1 (constants.%N)]
// CHECK:STDOUT:     %return.param: ref %empty_struct_type = out_param call_param0
// CHECK:STDOUT:     %return: ref %empty_struct_type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Int(%T.loc4_8.2: type, %N.loc4_18.2: @Int.%T.loc4_8.1 (%T)) {
// CHECK:STDOUT:   %T.loc4_8.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc4_8.1 (constants.%T)]
// CHECK:STDOUT:   %N.loc4_18.1: @Int.%T.loc4_8.1 (%T) = bind_symbolic_name N, 1 [symbolic = %N.loc4_18.1 (constants.%N)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %T.loc4_8.1 [symbolic = %pattern_type (constants.%pattern_type.7dc)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:
// CHECK:STDOUT:   fn() -> %empty_struct_type {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     %.loc4_41: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %empty_struct: %empty_struct_type = struct_value () [concrete = constants.%empty_struct]
// CHECK:STDOUT:     %.loc4_42: %empty_struct_type = converted %.loc4_41, %empty_struct [concrete = constants.%empty_struct]
// CHECK:STDOUT:     return %.loc4_42
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Int(constants.%T, constants.%N) {
// CHECK:STDOUT:   %T.loc4_8.1 => constants.%T
// CHECK:STDOUT:   %N.loc4_18.1 => constants.%N
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.7dc
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- use_fake_int.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %pattern_type.a96: type = pattern_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.7dc: type = pattern_type %T [symbolic]
// CHECK:STDOUT:   %N: %T = bind_symbolic_name N, 1 [symbolic]
// CHECK:STDOUT:   %pattern_type.dc0: type = pattern_type Core.IntLiteral [concrete]
// CHECK:STDOUT:   %Int.specific_fn: <specific function> = specific_function %Int, @Int(Core.IntLiteral, %int_32) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = %Core.Int
// CHECK:STDOUT:     import Core//prelude_fake_int
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Int: %Int.type = import_ref Core//prelude_fake_int, Int, loaded [concrete = constants.%Int]
// CHECK:STDOUT:   %Core.import_ref.5ab: type = import_ref Core//prelude_fake_int, loc4_8, loaded [symbolic = @Int.%T (constants.%T)]
// CHECK:STDOUT:   %Core.import_ref.34c: @Int.%T (%T) = import_ref Core//prelude_fake_int, loc4_18, loaded [symbolic = @Int.%N (constants.%N)]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .n = %n
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %n.patt: %pattern_type.a96 = binding_pattern n [concrete]
// CHECK:STDOUT:     %n.var_patt: %pattern_type.a96 = var_pattern %n.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %n.var: ref %empty_struct_type = var %n.var_patt [concrete]
// CHECK:STDOUT:   %.loc6_9.1: type = splice_block %.loc6_9.3 [concrete = constants.%empty_struct_type] {
// CHECK:STDOUT:     %.loc6_9.2: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc6_9.3: type = converted %.loc6_9.2, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %n: ref %empty_struct_type = bind_name n, %n.var [concrete = %n.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Int(imports.%Core.import_ref.5ab: type, imports.%Core.import_ref.34c: @Int.%T (%T)) [from "prelude_fake_int.carbon"] {
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic = %T (constants.%T)]
// CHECK:STDOUT:   %N: @Int.%T (%T) = bind_symbolic_name N, 1 [symbolic = %N (constants.%N)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %T [symbolic = %pattern_type (constants.%pattern_type.7dc)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:
// CHECK:STDOUT:   fn;
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %Int.specific_fn: <specific function> = specific_function constants.%Int, @Int(Core.IntLiteral, constants.%int_32) [concrete = constants.%Int.specific_fn]
// CHECK:STDOUT:   %Int.call: init %empty_struct_type = call %Int.specific_fn()
// CHECK:STDOUT:   assign file.%n.var, %Int.call
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Int(constants.%T, constants.%N) {
// CHECK:STDOUT:   %T => constants.%T
// CHECK:STDOUT:   %N => constants.%N
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.7dc
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Int(Core.IntLiteral, constants.%int_32) {
// CHECK:STDOUT:   %T => Core.IntLiteral
// CHECK:STDOUT:   %N => constants.%int_32
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.dc0
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- prelude_use_in_prelude.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %pattern_type.98f: type = pattern_type type [concrete]
// CHECK:STDOUT:   %N: %T = bind_symbolic_name N, 1 [symbolic]
// CHECK:STDOUT:   %pattern_type.7dc: type = pattern_type %T [symbolic]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %pattern_type.cb1: type = pattern_type %empty_tuple.type [concrete]
// CHECK:STDOUT:   %Int.type: type = fn_type @Int [concrete]
// CHECK:STDOUT:   %Int: %Int.type = struct_value () [concrete]
// CHECK:STDOUT:   %empty_tuple: %empty_tuple.type = tuple_value () [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %pattern_type.dc0: type = pattern_type Core.IntLiteral [concrete]
// CHECK:STDOUT:   %Int.specific_fn: <specific function> = specific_function %Int, @Int(Core.IntLiteral, %int_32) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Int = %Int.decl
// CHECK:STDOUT:     .n = %n
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Int.decl: %Int.type = fn_decl @Int [concrete = constants.%Int] {
// CHECK:STDOUT:     %T.patt: %pattern_type.98f = symbolic_binding_pattern T, 0 [concrete]
// CHECK:STDOUT:     %N.patt: @Int.%pattern_type (%pattern_type.7dc) = symbolic_binding_pattern N, 1 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.cb1 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.cb1 = out_param_pattern %return.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %.loc8_29.1: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc8_29.2: type = converted %.loc8_29.1, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:     %T.loc8_8.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc8_8.1 (constants.%T)]
// CHECK:STDOUT:     %T.ref: type = name_ref T, %T.loc8_8.2 [symbolic = %T.loc8_8.1 (constants.%T)]
// CHECK:STDOUT:     %N.loc8_18.2: @Int.%T.loc8_8.1 (%T) = bind_symbolic_name N, 1 [symbolic = %N.loc8_18.1 (constants.%N)]
// CHECK:STDOUT:     %return.param: ref %empty_tuple.type = out_param call_param0
// CHECK:STDOUT:     %return: ref %empty_tuple.type = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %n.patt: %pattern_type.cb1 = binding_pattern n [concrete]
// CHECK:STDOUT:     %n.var_patt: %pattern_type.cb1 = var_pattern %n.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %n.var: ref %empty_tuple.type = var %n.var_patt [concrete]
// CHECK:STDOUT:   %.loc10_9.1: type = splice_block %.loc10_9.3 [concrete = constants.%empty_tuple.type] {
// CHECK:STDOUT:     %.loc10_9.2: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %.loc10_9.3: type = converted %.loc10_9.2, constants.%empty_tuple.type [concrete = constants.%empty_tuple.type]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %n: ref %empty_tuple.type = bind_name n, %n.var [concrete = %n.var]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Int(%T.loc8_8.2: type, %N.loc8_18.2: @Int.%T.loc8_8.1 (%T)) {
// CHECK:STDOUT:   %T.loc8_8.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc8_8.1 (constants.%T)]
// CHECK:STDOUT:   %N.loc8_18.1: @Int.%T.loc8_8.1 (%T) = bind_symbolic_name N, 1 [symbolic = %N.loc8_18.1 (constants.%N)]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %T.loc8_8.1 [symbolic = %pattern_type (constants.%pattern_type.7dc)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:
// CHECK:STDOUT:   fn() -> %empty_tuple.type {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     %.loc8_41: %empty_tuple.type = tuple_literal ()
// CHECK:STDOUT:     %empty_tuple: %empty_tuple.type = tuple_value () [concrete = constants.%empty_tuple]
// CHECK:STDOUT:     %.loc8_42: %empty_tuple.type = converted %.loc8_41, %empty_tuple [concrete = constants.%empty_tuple]
// CHECK:STDOUT:     return %.loc8_42
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @__global_init() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %Int.specific_fn: <specific function> = specific_function constants.%Int, @Int(Core.IntLiteral, constants.%int_32) [concrete = constants.%Int.specific_fn]
// CHECK:STDOUT:   %Int.call: init %empty_tuple.type = call %Int.specific_fn()
// CHECK:STDOUT:   assign file.%n.var, %Int.call
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Int(constants.%T, constants.%N) {
// CHECK:STDOUT:   %T.loc8_8.1 => constants.%T
// CHECK:STDOUT:   %N.loc8_18.1 => constants.%N
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.7dc
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Int(Core.IntLiteral, constants.%int_32) {
// CHECK:STDOUT:   %T.loc8_8.1 => Core.IntLiteral
// CHECK:STDOUT:   %N.loc8_18.1 => constants.%int_32
// CHECK:STDOUT:   %pattern_type => constants.%pattern_type.dc0
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_prelude_as_namespace.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_prelude_as_class.carbon
// CHECK:STDOUT:
