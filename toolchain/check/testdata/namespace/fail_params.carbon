// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/convert.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/namespace/fail_params.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/namespace/fail_params.carbon

// CHECK:STDERR: fail_params.carbon:[[@LINE+4]]:12: error: `namespace` declaration cannot have parameters [UnexpectedDeclNameParams]
// CHECK:STDERR: namespace A();
// CHECK:STDERR:            ^~
// CHECK:STDERR:
namespace A();

// Parameters are ignored for error recovery.
fn A.F() {}

// CHECK:STDERR: fail_params.carbon:[[@LINE+8]]:16: error: name `Core.Int` implicitly referenced here, but not found [CoreNameNotFound]
// CHECK:STDERR: namespace B(n: i32);
// CHECK:STDERR:                ^~~
// CHECK:STDERR:
// CHECK:STDERR: fail_params.carbon:[[@LINE+4]]:12: error: `namespace` declaration cannot have parameters [UnexpectedDeclNameParams]
// CHECK:STDERR: namespace B(n: i32);
// CHECK:STDERR:            ^~~~~~~~
// CHECK:STDERR:
namespace B(n: i32);

// CHECK:STDERR: fail_params.carbon:[[@LINE+4]]:12: error: `namespace` declaration cannot have parameters [UnexpectedDeclNameParams]
// CHECK:STDERR: namespace C[T:! type](x: T);
// CHECK:STDERR:            ^~~~~~~~~~
// CHECK:STDERR:
namespace C[T:! type](x: T);

namespace D;
// CHECK:STDERR: fail_params.carbon:[[@LINE+7]]:4: error: redeclaration differs because of parameter list [RedeclParamListDiffers]
// CHECK:STDERR: fn D(T:! type).F() {}
// CHECK:STDERR:    ^
// CHECK:STDERR: fail_params.carbon:[[@LINE-4]]:1: note: previously declared without parameter list [RedeclParamListPrevious]
// CHECK:STDERR: namespace D;
// CHECK:STDERR: ^~~~~~~~~~~~
// CHECK:STDERR:
fn D(T:! type).F() {}

// CHECK:STDOUT: --- fail_params.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F.type.bd7: type = fn_type @F.loc22 [concrete]
// CHECK:STDOUT:   %F.199: %F.type.bd7 = struct_value () [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic]
// CHECK:STDOUT:   %F.type.b25: type = fn_type @F.loc48 [concrete]
// CHECK:STDOUT:   %F.c41: %F.type.b25 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Int = <poisoned>
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .A = %A
// CHECK:STDOUT:     .B = %B
// CHECK:STDOUT:     .C = %C
// CHECK:STDOUT:     .D = %D
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %A: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F = %F.decl.loc22
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.decl.loc22: %F.type.bd7 = fn_decl @F.loc22 [concrete = constants.%F.199] {} {}
// CHECK:STDOUT:   %n.param: <error> = value_param call_param0
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %n: <error> = bind_name n, %n.param
// CHECK:STDOUT:   %B: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %T: type = bind_symbolic_name T, 0 [symbolic = constants.%T]
// CHECK:STDOUT:   %x.param: %T = value_param call_param0
// CHECK:STDOUT:   %T.ref: type = name_ref T, %T [symbolic = constants.%T]
// CHECK:STDOUT:   %x: %T = bind_name x, %x.param
// CHECK:STDOUT:   %C: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %D: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %F.decl.loc48: %F.type.b25 = fn_decl @F.loc48 [concrete = constants.%F.c41] {} {
// CHECK:STDOUT:     %T.loc48_6.2: type = bind_symbolic_name T, 0 [symbolic = %T.loc48_6.1 (constants.%T)]
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F.loc22() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @F.loc48(%T.loc48_6.2: type) {
// CHECK:STDOUT:   %T.loc48_6.1: type = bind_symbolic_name T, 0 [symbolic = %T.loc48_6.1 (constants.%T)]
// CHECK:STDOUT:
// CHECK:STDOUT: !definition:
// CHECK:STDOUT:
// CHECK:STDOUT:   fn() {
// CHECK:STDOUT:   !entry:
// CHECK:STDOUT:     return
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @F.loc48(constants.%T) {
// CHECK:STDOUT:   %T.loc48_6.1 => constants.%T
// CHECK:STDOUT: }
// CHECK:STDOUT:
