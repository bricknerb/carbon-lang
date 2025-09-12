// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/primitives.carbon
// EXTRA-ARGS: --target=x86_64-pc-windows-msvc --clang-arg=-fno-PIE
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interop/cpp/builtins.llp64.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interop/cpp/builtins.llp64.carbon

// --- supported_types.carbon

library "[[@TEST_NAME]]";

import Cpp inline "";

fn F() {
  //@dump-sem-ir-begin
  let cpp_signed_char : Cpp.signed_char = 1 as i8;
  let carbon_signed_char: i8 = cpp_signed_char;

  let cpp_short : Cpp.short = 1 as i16;
  let carbon_short: i16 = cpp_short;

  let cpp_int : Cpp.int = 1 as i32;
  let carbon_int: i32 = cpp_int;

  let cpp_long_long : Cpp.long_long = 1 as i64;
  let carbon_long_long: i64 = cpp_long_long;

  let cpp_unsigned_char : Cpp.unsigned_char = 1 as u8;
  let carbon_unsigned_char: u8 = cpp_unsigned_char;

  let cpp_unsigned_short : Cpp.unsigned_short = 1 as u16;
  let carbon_unsigned_short: u16 = cpp_unsigned_short;

  let cpp_unsigned_int : Cpp.unsigned_int = 1 as u32;
  let carbon_unsigned_int: u32 = cpp_unsigned_int;

  let cpp_unsigned_long_long : Cpp.unsigned_long_long = 1 as u64;
  let carbon_unsigned_long_long: u64 = cpp_unsigned_long_long;

  let cpp_float : Cpp.float = 1.0 as f32;
  let carbon_float: f32 = cpp_float;

  let cpp_double : Cpp.double = 1.0 as f64;
  let carbon_double: f64 = cpp_double;
  //@dump-sem-ir-end
}

// --- fail_todo_unsupported_types.carbon

library "[[@TEST_NAME]]";

import Cpp inline "";

fn F() {
  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+7]]:18: error: semantics TODO: `Unsupported: builtin type: long` [SemanticsTodo]
  // CHECK:STDERR:   let cpp_long : Cpp.long = 1 as i64;
  // CHECK:STDERR:                  ^~~~~~~~
  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+4]]:18: note: in `Cpp` name lookup for `long` [InCppNameLookup]
  // CHECK:STDERR:   let cpp_long : Cpp.long = 1 as i64;
  // CHECK:STDERR:                  ^~~~~~~~
  // CHECK:STDERR:
  let cpp_long : Cpp.long = 1 as i64;
  let carbon_long: i64 = cpp_long;

  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+7]]:27: error: semantics TODO: `Unsupported: builtin type: unsigned long` [SemanticsTodo]
  // CHECK:STDERR:   let cpp_unsigned_long : Cpp.unsigned_long = 1 as u64;
  // CHECK:STDERR:                           ^~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+4]]:27: note: in `Cpp` name lookup for `unsigned_long` [InCppNameLookup]
  // CHECK:STDERR:   let cpp_unsigned_long : Cpp.unsigned_long = 1 as u64;
  // CHECK:STDERR:                           ^~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  let cpp_unsigned_long : Cpp.unsigned_long = 1 as u64;
  let carbon_unsigned_long: u64 = cpp_unsigned_long;

  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+7]]:25: error: semantics TODO: `Unsupported: builtin type: long double` [SemanticsTodo]
  // CHECK:STDERR:   let cpp_long_double : Cpp.long_double = 1.0 as f64;
  // CHECK:STDERR:                         ^~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+4]]:25: note: in `Cpp` name lookup for `long_double` [InCppNameLookup]
  // CHECK:STDERR:   let cpp_long_double : Cpp.long_double = 1.0 as f64;
  // CHECK:STDERR:                         ^~~~~~~~~~~~~~~
  // CHECK:STDERR:
  let cpp_long_double : Cpp.long_double = 1.0 as f64;
  let carbon_long_double: f64 = cpp_long_double;
  // CHECK:STDERR: fail_todo_unsupported_types.carbon:[[@LINE+4]]:3: error: missing `//@dump-sem-ir-begin` to match `//@dump-sem-ir-end` [DumpSemIRRangeMissingBegin]
  // CHECK:STDERR:   //@dump-sem-ir-end
  // CHECK:STDERR:   ^
  // CHECK:STDERR:
  //@dump-sem-ir-end
}

// --- fail_not_builtin.carbon

library "[[@TEST_NAME]]";

import Cpp inline "";

fn F() {
  // CHECK:STDERR: fail_not_builtin.carbon:[[@LINE+4]]:21: error: member name `not_builtin` not found in `Cpp` [MemberNameNotFoundInInstScope]
  // CHECK:STDERR:   let not_builtin : Cpp.not_builtin = 1;
  // CHECK:STDERR:                     ^~~~~~~~~~~~~~~
  // CHECK:STDERR:
  let not_builtin : Cpp.not_builtin = 1;
}

// --- fail_lookup_in_other_scopes.carbon

library "[[@TEST_NAME]]";

import Cpp inline '''
namespace MyNamespace {}
''';

fn F() {
  // CHECK:STDERR: fail_lookup_in_other_scopes.carbon:[[@LINE+4]]:14: error: member name `long` not found in `Cpp.MyNamespace` [MemberNameNotFoundInInstScope]
  // CHECK:STDERR:   let long : Cpp.MyNamespace.long = 1;
  // CHECK:STDERR:              ^~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  let long : Cpp.MyNamespace.long = 1;
}

// --- override_builtin.carbon

library "[[@TEST_NAME]]";

import Cpp inline '''
struct unsigned_int {
  auto foo() -> unsigned int;
};
''';

fn F() {
  //@dump-sem-ir-begin
  var unsigned_int : Cpp.unsigned_int;
  let x: u32 = unsigned_int.foo();
  //@dump-sem-ir-end
}

// CHECK:STDOUT: --- supported_types.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_8: Core.IntLiteral = int_value 8 [concrete]
// CHECK:STDOUT:   %i8: type = class_type @Int, @Int(%int_8) [concrete]
// CHECK:STDOUT:   %pattern_type.e3f: type = pattern_type %i8 [concrete]
// CHECK:STDOUT:   %int_1.5b8: Core.IntLiteral = int_value 1 [concrete]
// CHECK:STDOUT:   %As.type.b4c: type = facet_type <@As, @As(%i8)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.cc1: type = fn_type @As.Convert, @As(%i8) [concrete]
// CHECK:STDOUT:   %To: Core.IntLiteral = bind_symbolic_name To, 0 [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.676: type = fn_type @Core.IntLiteral.as.As.impl.Convert.1, @Core.IntLiteral.as.As.impl.40a(%To) [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.086: %Core.IntLiteral.as.As.impl.Convert.type.676 = struct_value () [symbolic]
// CHECK:STDOUT:   %As.impl_witness.f4c: <witness> = impl_witness imports.%As.impl_witness_table.3fe, @Core.IntLiteral.as.As.impl.40a(%int_8) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.8f6: type = fn_type @Core.IntLiteral.as.As.impl.Convert.1, @Core.IntLiteral.as.As.impl.40a(%int_8) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.3bd: %Core.IntLiteral.as.As.impl.Convert.type.8f6 = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.cdd: %As.type.b4c = facet_value Core.IntLiteral, (%As.impl_witness.f4c) [concrete]
// CHECK:STDOUT:   %.2b8: type = fn_type_with_self_type %As.Convert.type.cc1, %As.facet.cdd [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.c17: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.3bd [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.03d: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.3bd, @Core.IntLiteral.as.As.impl.Convert.1(%int_8) [concrete]
// CHECK:STDOUT:   %bound_method.41c: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.03d [concrete]
// CHECK:STDOUT:   %int_1.30e: %i8 = int_value 1 [concrete]
// CHECK:STDOUT:   %int_16: Core.IntLiteral = int_value 16 [concrete]
// CHECK:STDOUT:   %i16: type = class_type @Int, @Int(%int_16) [concrete]
// CHECK:STDOUT:   %pattern_type.2f8: type = pattern_type %i16 [concrete]
// CHECK:STDOUT:   %As.type.359: type = facet_type <@As, @As(%i16)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.be5: type = fn_type @As.Convert, @As(%i16) [concrete]
// CHECK:STDOUT:   %As.impl_witness.30d: <witness> = impl_witness imports.%As.impl_witness_table.3fe, @Core.IntLiteral.as.As.impl.40a(%int_16) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.bf1: type = fn_type @Core.IntLiteral.as.As.impl.Convert.1, @Core.IntLiteral.as.As.impl.40a(%int_16) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.3d5: %Core.IntLiteral.as.As.impl.Convert.type.bf1 = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.2f3: %As.type.359 = facet_value Core.IntLiteral, (%As.impl_witness.30d) [concrete]
// CHECK:STDOUT:   %.a59: type = fn_type_with_self_type %As.Convert.type.be5, %As.facet.2f3 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.f1d: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.3d5 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.2d7: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.3d5, @Core.IntLiteral.as.As.impl.Convert.1(%int_16) [concrete]
// CHECK:STDOUT:   %bound_method.0a7: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.2d7 [concrete]
// CHECK:STDOUT:   %int_1.f90: %i16 = int_value 1 [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.7ce: type = pattern_type %i32 [concrete]
// CHECK:STDOUT:   %As.type.047: type = facet_type <@As, @As(%i32)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.99b: type = fn_type @As.Convert, @As(%i32) [concrete]
// CHECK:STDOUT:   %As.impl_witness.a7b: <witness> = impl_witness imports.%As.impl_witness_table.3fe, @Core.IntLiteral.as.As.impl.40a(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.7bd: type = fn_type @Core.IntLiteral.as.As.impl.Convert.1, @Core.IntLiteral.as.As.impl.40a(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.11b: %Core.IntLiteral.as.As.impl.Convert.type.7bd = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.71b: %As.type.047 = facet_value Core.IntLiteral, (%As.impl_witness.a7b) [concrete]
// CHECK:STDOUT:   %.323: type = fn_type_with_self_type %As.Convert.type.99b, %As.facet.71b [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.b59: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.11b [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.8c7: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.11b, @Core.IntLiteral.as.As.impl.Convert.1(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.fee: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.8c7 [concrete]
// CHECK:STDOUT:   %int_1.5d2: %i32 = int_value 1 [concrete]
// CHECK:STDOUT:   %int_64: Core.IntLiteral = int_value 64 [concrete]
// CHECK:STDOUT:   %i64: type = class_type @Int, @Int(%int_64) [concrete]
// CHECK:STDOUT:   %pattern_type.95b: type = pattern_type %i64 [concrete]
// CHECK:STDOUT:   %As.type.229: type = facet_type <@As, @As(%i64)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.d57: type = fn_type @As.Convert, @As(%i64) [concrete]
// CHECK:STDOUT:   %As.impl_witness.9f4: <witness> = impl_witness imports.%As.impl_witness_table.3fe, @Core.IntLiteral.as.As.impl.40a(%int_64) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.e2c: type = fn_type @Core.IntLiteral.as.As.impl.Convert.1, @Core.IntLiteral.as.As.impl.40a(%int_64) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.d07: %Core.IntLiteral.as.As.impl.Convert.type.e2c = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.8f0: %As.type.229 = facet_value Core.IntLiteral, (%As.impl_witness.9f4) [concrete]
// CHECK:STDOUT:   %.cc0: type = fn_type_with_self_type %As.Convert.type.d57, %As.facet.8f0 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.d38: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.d07 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.97c: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.d07, @Core.IntLiteral.as.As.impl.Convert.1(%int_64) [concrete]
// CHECK:STDOUT:   %bound_method.8f8: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.97c [concrete]
// CHECK:STDOUT:   %int_1.41a: %i64 = int_value 1 [concrete]
// CHECK:STDOUT:   %u8: type = class_type @UInt, @UInt(%int_8) [concrete]
// CHECK:STDOUT:   %pattern_type.8f3: type = pattern_type %u8 [concrete]
// CHECK:STDOUT:   %As.type.714: type = facet_type <@As, @As(%u8)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.a56: type = fn_type @As.Convert, @As(%u8) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.6e4: type = fn_type @Core.IntLiteral.as.As.impl.Convert.2, @Core.IntLiteral.as.As.impl.387(%To) [symbolic]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.483: %Core.IntLiteral.as.As.impl.Convert.type.6e4 = struct_value () [symbolic]
// CHECK:STDOUT:   %As.impl_witness.8bb: <witness> = impl_witness imports.%As.impl_witness_table.526, @Core.IntLiteral.as.As.impl.387(%int_8) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.f76: type = fn_type @Core.IntLiteral.as.As.impl.Convert.2, @Core.IntLiteral.as.As.impl.387(%int_8) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.166: %Core.IntLiteral.as.As.impl.Convert.type.f76 = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.ac4: %As.type.714 = facet_value Core.IntLiteral, (%As.impl_witness.8bb) [concrete]
// CHECK:STDOUT:   %.f2b: type = fn_type_with_self_type %As.Convert.type.a56, %As.facet.ac4 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.32b: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.166 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.f30: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.166, @Core.IntLiteral.as.As.impl.Convert.2(%int_8) [concrete]
// CHECK:STDOUT:   %bound_method.df5: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.f30 [concrete]
// CHECK:STDOUT:   %int_1.e80: %u8 = int_value 1 [concrete]
// CHECK:STDOUT:   %u16: type = class_type @UInt, @UInt(%int_16) [concrete]
// CHECK:STDOUT:   %pattern_type.9db: type = pattern_type %u16 [concrete]
// CHECK:STDOUT:   %As.type.b84: type = facet_type <@As, @As(%u16)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.c7c: type = fn_type @As.Convert, @As(%u16) [concrete]
// CHECK:STDOUT:   %As.impl_witness.be9: <witness> = impl_witness imports.%As.impl_witness_table.526, @Core.IntLiteral.as.As.impl.387(%int_16) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.b2b: type = fn_type @Core.IntLiteral.as.As.impl.Convert.2, @Core.IntLiteral.as.As.impl.387(%int_16) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bfd: %Core.IntLiteral.as.As.impl.Convert.type.b2b = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.3fa: %As.type.b84 = facet_value Core.IntLiteral, (%As.impl_witness.be9) [concrete]
// CHECK:STDOUT:   %.e0b: type = fn_type_with_self_type %As.Convert.type.c7c, %As.facet.3fa [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.8d6: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.bfd [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.b40: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.bfd, @Core.IntLiteral.as.As.impl.Convert.2(%int_16) [concrete]
// CHECK:STDOUT:   %bound_method.779: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.b40 [concrete]
// CHECK:STDOUT:   %int_1.3e8: %u16 = int_value 1 [concrete]
// CHECK:STDOUT:   %u32: type = class_type @UInt, @UInt(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.4a9: type = pattern_type %u32 [concrete]
// CHECK:STDOUT:   %As.type.346: type = facet_type <@As, @As(%u32)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.b94: type = fn_type @As.Convert, @As(%u32) [concrete]
// CHECK:STDOUT:   %As.impl_witness.666: <witness> = impl_witness imports.%As.impl_witness_table.526, @Core.IntLiteral.as.As.impl.387(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.837: type = fn_type @Core.IntLiteral.as.As.impl.Convert.2, @Core.IntLiteral.as.As.impl.387(%int_32) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.358: %Core.IntLiteral.as.As.impl.Convert.type.837 = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.556: %As.type.346 = facet_value Core.IntLiteral, (%As.impl_witness.666) [concrete]
// CHECK:STDOUT:   %.cbe: type = fn_type_with_self_type %As.Convert.type.b94, %As.facet.556 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.67d: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.358 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.baf: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.358, @Core.IntLiteral.as.As.impl.Convert.2(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.d2b: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.baf [concrete]
// CHECK:STDOUT:   %int_1.c1d: %u32 = int_value 1 [concrete]
// CHECK:STDOUT:   %u64: type = class_type @UInt, @UInt(%int_64) [concrete]
// CHECK:STDOUT:   %pattern_type.157: type = pattern_type %u64 [concrete]
// CHECK:STDOUT:   %As.type.f20: type = facet_type <@As, @As(%u64)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.7eb: type = fn_type @As.Convert, @As(%u64) [concrete]
// CHECK:STDOUT:   %As.impl_witness.10d: <witness> = impl_witness imports.%As.impl_witness_table.526, @Core.IntLiteral.as.As.impl.387(%int_64) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.type.5aa: type = fn_type @Core.IntLiteral.as.As.impl.Convert.2, @Core.IntLiteral.as.As.impl.387(%int_64) [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.d63: %Core.IntLiteral.as.As.impl.Convert.type.5aa = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.3ba: %As.type.f20 = facet_value Core.IntLiteral, (%As.impl_witness.10d) [concrete]
// CHECK:STDOUT:   %.8d9: type = fn_type_with_self_type %As.Convert.type.7eb, %As.facet.3ba [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.bound.a7e: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.d63 [concrete]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.specific_fn.a60: <specific function> = specific_function %Core.IntLiteral.as.As.impl.Convert.d63, @Core.IntLiteral.as.As.impl.Convert.2(%int_64) [concrete]
// CHECK:STDOUT:   %bound_method.b3d: <bound method> = bound_method %int_1.5b8, %Core.IntLiteral.as.As.impl.Convert.specific_fn.a60 [concrete]
// CHECK:STDOUT:   %int_1.f23: %u64 = int_value 1 [concrete]
// CHECK:STDOUT:   %f32.97e: type = class_type @Float, @Float(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.201: type = pattern_type %f32.97e [concrete]
// CHECK:STDOUT:   %float.674bbc.1: Core.FloatLiteral = float_literal_value 10e-1 [concrete]
// CHECK:STDOUT:   %As.type.9fc: type = facet_type <@As, @As(%f32.97e)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.f5e: type = fn_type @As.Convert, @As(%f32.97e) [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.type.b91: type = fn_type @Core.FloatLiteral.as.As.impl.Convert, @Core.FloatLiteral.as.As.impl(%To) [symbolic]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.e7c: %Core.FloatLiteral.as.As.impl.Convert.type.b91 = struct_value () [symbolic]
// CHECK:STDOUT:   %As.impl_witness.992: <witness> = impl_witness imports.%As.impl_witness_table.f24, @Core.FloatLiteral.as.As.impl(%int_32) [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.type.421: type = fn_type @Core.FloatLiteral.as.As.impl.Convert, @Core.FloatLiteral.as.As.impl(%int_32) [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.3c7: %Core.FloatLiteral.as.As.impl.Convert.type.421 = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.218: %As.type.9fc = facet_value Core.FloatLiteral, (%As.impl_witness.992) [concrete]
// CHECK:STDOUT:   %.372: type = fn_type_with_self_type %As.Convert.type.f5e, %As.facet.218 [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.bound.cc9: <bound method> = bound_method %float.674bbc.1, %Core.FloatLiteral.as.As.impl.Convert.3c7 [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.specific_fn.4c6: <specific function> = specific_function %Core.FloatLiteral.as.As.impl.Convert.3c7, @Core.FloatLiteral.as.As.impl.Convert(%int_32) [concrete]
// CHECK:STDOUT:   %bound_method.100: <bound method> = bound_method %float.674bbc.1, %Core.FloatLiteral.as.As.impl.Convert.specific_fn.4c6 [concrete]
// CHECK:STDOUT:   %float.e3b: %f32.97e = float_value 1 [concrete]
// CHECK:STDOUT:   %f64.d77: type = class_type @Float, @Float(%int_64) [concrete]
// CHECK:STDOUT:   %pattern_type.0ae: type = pattern_type %f64.d77 [concrete]
// CHECK:STDOUT:   %float.674bbc.2: Core.FloatLiteral = float_literal_value 10e-1 [concrete]
// CHECK:STDOUT:   %As.type.a57: type = facet_type <@As, @As(%f64.d77)> [concrete]
// CHECK:STDOUT:   %As.Convert.type.8fc: type = fn_type @As.Convert, @As(%f64.d77) [concrete]
// CHECK:STDOUT:   %As.impl_witness.b46: <witness> = impl_witness imports.%As.impl_witness_table.f24, @Core.FloatLiteral.as.As.impl(%int_64) [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.type.281: type = fn_type @Core.FloatLiteral.as.As.impl.Convert, @Core.FloatLiteral.as.As.impl(%int_64) [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.251: %Core.FloatLiteral.as.As.impl.Convert.type.281 = struct_value () [concrete]
// CHECK:STDOUT:   %As.facet.b8a: %As.type.a57 = facet_value Core.FloatLiteral, (%As.impl_witness.b46) [concrete]
// CHECK:STDOUT:   %.ec3: type = fn_type_with_self_type %As.Convert.type.8fc, %As.facet.b8a [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.bound.64d: <bound method> = bound_method %float.674bbc.2, %Core.FloatLiteral.as.As.impl.Convert.251 [concrete]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.specific_fn.150: <specific function> = specific_function %Core.FloatLiteral.as.As.impl.Convert.251, @Core.FloatLiteral.as.As.impl.Convert(%int_64) [concrete]
// CHECK:STDOUT:   %bound_method.5cc: <bound method> = bound_method %float.674bbc.2, %Core.FloatLiteral.as.As.impl.Convert.specific_fn.150 [concrete]
// CHECK:STDOUT:   %float.d20: %f64.d77 = float_value 1 [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Cpp: <namespace> = namespace file.%Cpp.import_cpp, [concrete] {
// CHECK:STDOUT:     .signed_char = @F.%i8.1
// CHECK:STDOUT:     .short = @F.%i16.1
// CHECK:STDOUT:     .int = @F.%i32.1
// CHECK:STDOUT:     .long_long = @F.%i64.1
// CHECK:STDOUT:     .unsigned_char = @F.%u8.1
// CHECK:STDOUT:     .unsigned_short = @F.%u16.1
// CHECK:STDOUT:     .unsigned_int = @F.%u32.1
// CHECK:STDOUT:     .unsigned_long_long = @F.%u64.1
// CHECK:STDOUT:     .float = @F.%f32.1
// CHECK:STDOUT:     .double = @F.%f64.1
// CHECK:STDOUT:     import Cpp//...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import_ref.52c: @Core.IntLiteral.as.As.impl.40a.%Core.IntLiteral.as.As.impl.Convert.type (%Core.IntLiteral.as.As.impl.Convert.type.676) = import_ref Core//prelude/parts/int, loc32_39, loaded [symbolic = @Core.IntLiteral.as.As.impl.40a.%Core.IntLiteral.as.As.impl.Convert (constants.%Core.IntLiteral.as.As.impl.Convert.086)]
// CHECK:STDOUT:   %As.impl_witness_table.3fe = impl_witness_table (%Core.import_ref.52c), @Core.IntLiteral.as.As.impl.40a [concrete]
// CHECK:STDOUT:   %Core.import_ref.564: @Core.IntLiteral.as.As.impl.387.%Core.IntLiteral.as.As.impl.Convert.type (%Core.IntLiteral.as.As.impl.Convert.type.6e4) = import_ref Core//prelude/parts/uint, loc32_40, loaded [symbolic = @Core.IntLiteral.as.As.impl.387.%Core.IntLiteral.as.As.impl.Convert (constants.%Core.IntLiteral.as.As.impl.Convert.483)]
// CHECK:STDOUT:   %As.impl_witness_table.526 = impl_witness_table (%Core.import_ref.564), @Core.IntLiteral.as.As.impl.387 [concrete]
// CHECK:STDOUT:   %Core.import_ref.497: @Core.FloatLiteral.as.As.impl.%Core.FloatLiteral.as.As.impl.Convert.type (%Core.FloatLiteral.as.As.impl.Convert.type.b91) = import_ref Core//prelude/parts/float, loc25_41, loaded [symbolic = @Core.FloatLiteral.as.As.impl.%Core.FloatLiteral.as.As.impl.Convert (constants.%Core.FloatLiteral.as.As.impl.Convert.e7c)]
// CHECK:STDOUT:   %As.impl_witness_table.f24 = impl_witness_table (%Core.import_ref.497), @Core.FloatLiteral.as.As.impl [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_signed_char.patt: %pattern_type.e3f = binding_pattern cpp_signed_char [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc8: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_8.loc8: Core.IntLiteral = int_value 8 [concrete = constants.%int_8]
// CHECK:STDOUT:   %i8.loc8: type = class_type @Int, @Int(constants.%int_8) [concrete = constants.%i8]
// CHECK:STDOUT:   %impl.elem0.loc8: %.2b8 = impl_witness_access constants.%As.impl_witness.f4c, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.3bd]
// CHECK:STDOUT:   %bound_method.loc8_45.1: <bound method> = bound_method %int_1.loc8, %impl.elem0.loc8 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.c17]
// CHECK:STDOUT:   %specific_fn.loc8: <specific function> = specific_function %impl.elem0.loc8, @Core.IntLiteral.as.As.impl.Convert.1(constants.%int_8) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.03d]
// CHECK:STDOUT:   %bound_method.loc8_45.2: <bound method> = bound_method %int_1.loc8, %specific_fn.loc8 [concrete = constants.%bound_method.41c]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc8: init %i8 = call %bound_method.loc8_45.2(%int_1.loc8) [concrete = constants.%int_1.30e]
// CHECK:STDOUT:   %.loc8_45.1: %i8 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc8 [concrete = constants.%int_1.30e]
// CHECK:STDOUT:   %.loc8_45.2: %i8 = converted %int_1.loc8, %.loc8_45.1 [concrete = constants.%int_1.30e]
// CHECK:STDOUT:   %.loc8_28: type = splice_block %signed_char.ref [concrete = constants.%i8] {
// CHECK:STDOUT:     %Cpp.ref.loc8: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %signed_char.ref: type = name_ref signed_char, %i8.1 [concrete = constants.%i8]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_signed_char: %i8 = bind_name cpp_signed_char, %.loc8_45.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_signed_char.patt: %pattern_type.e3f = binding_pattern carbon_signed_char [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_signed_char.ref: %i8 = name_ref cpp_signed_char, %cpp_signed_char
// CHECK:STDOUT:   %.loc9: type = splice_block %i8.loc9 [concrete = constants.%i8] {
// CHECK:STDOUT:     %int_8.loc9: Core.IntLiteral = int_value 8 [concrete = constants.%int_8]
// CHECK:STDOUT:     %i8.loc9: type = class_type @Int, @Int(constants.%int_8) [concrete = constants.%i8]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_signed_char: %i8 = bind_name carbon_signed_char, %cpp_signed_char.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_short.patt: %pattern_type.2f8 = binding_pattern cpp_short [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc11: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_16.loc11: Core.IntLiteral = int_value 16 [concrete = constants.%int_16]
// CHECK:STDOUT:   %i16.loc11: type = class_type @Int, @Int(constants.%int_16) [concrete = constants.%i16]
// CHECK:STDOUT:   %impl.elem0.loc11: %.a59 = impl_witness_access constants.%As.impl_witness.30d, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.3d5]
// CHECK:STDOUT:   %bound_method.loc11_33.1: <bound method> = bound_method %int_1.loc11, %impl.elem0.loc11 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.f1d]
// CHECK:STDOUT:   %specific_fn.loc11: <specific function> = specific_function %impl.elem0.loc11, @Core.IntLiteral.as.As.impl.Convert.1(constants.%int_16) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.2d7]
// CHECK:STDOUT:   %bound_method.loc11_33.2: <bound method> = bound_method %int_1.loc11, %specific_fn.loc11 [concrete = constants.%bound_method.0a7]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc11: init %i16 = call %bound_method.loc11_33.2(%int_1.loc11) [concrete = constants.%int_1.f90]
// CHECK:STDOUT:   %.loc11_33.1: %i16 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc11 [concrete = constants.%int_1.f90]
// CHECK:STDOUT:   %.loc11_33.2: %i16 = converted %int_1.loc11, %.loc11_33.1 [concrete = constants.%int_1.f90]
// CHECK:STDOUT:   %.loc11_22: type = splice_block %short.ref [concrete = constants.%i16] {
// CHECK:STDOUT:     %Cpp.ref.loc11: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %short.ref: type = name_ref short, %i16.1 [concrete = constants.%i16]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_short: %i16 = bind_name cpp_short, %.loc11_33.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_short.patt: %pattern_type.2f8 = binding_pattern carbon_short [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_short.ref: %i16 = name_ref cpp_short, %cpp_short
// CHECK:STDOUT:   %.loc12: type = splice_block %i16.loc12 [concrete = constants.%i16] {
// CHECK:STDOUT:     %int_16.loc12: Core.IntLiteral = int_value 16 [concrete = constants.%int_16]
// CHECK:STDOUT:     %i16.loc12: type = class_type @Int, @Int(constants.%int_16) [concrete = constants.%i16]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_short: %i16 = bind_name carbon_short, %cpp_short.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_int.patt: %pattern_type.7ce = binding_pattern cpp_int [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc14: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_32.loc14: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %i32.loc14: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   %impl.elem0.loc14: %.323 = impl_witness_access constants.%As.impl_witness.a7b, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.11b]
// CHECK:STDOUT:   %bound_method.loc14_29.1: <bound method> = bound_method %int_1.loc14, %impl.elem0.loc14 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.b59]
// CHECK:STDOUT:   %specific_fn.loc14: <specific function> = specific_function %impl.elem0.loc14, @Core.IntLiteral.as.As.impl.Convert.1(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.8c7]
// CHECK:STDOUT:   %bound_method.loc14_29.2: <bound method> = bound_method %int_1.loc14, %specific_fn.loc14 [concrete = constants.%bound_method.fee]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc14: init %i32 = call %bound_method.loc14_29.2(%int_1.loc14) [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc14_29.1: %i32 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc14 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc14_29.2: %i32 = converted %int_1.loc14, %.loc14_29.1 [concrete = constants.%int_1.5d2]
// CHECK:STDOUT:   %.loc14_20: type = splice_block %int.ref [concrete = constants.%i32] {
// CHECK:STDOUT:     %Cpp.ref.loc14: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %int.ref: type = name_ref int, %i32.1 [concrete = constants.%i32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_int: %i32 = bind_name cpp_int, %.loc14_29.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_int.patt: %pattern_type.7ce = binding_pattern carbon_int [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_int.ref: %i32 = name_ref cpp_int, %cpp_int
// CHECK:STDOUT:   %.loc15: type = splice_block %i32.loc15 [concrete = constants.%i32] {
// CHECK:STDOUT:     %int_32.loc15: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %i32.loc15: type = class_type @Int, @Int(constants.%int_32) [concrete = constants.%i32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_int: %i32 = bind_name carbon_int, %cpp_int.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_long_long.patt: %pattern_type.95b = binding_pattern cpp_long_long [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc17: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_64.loc17: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:   %i64.loc17: type = class_type @Int, @Int(constants.%int_64) [concrete = constants.%i64]
// CHECK:STDOUT:   %impl.elem0.loc17: %.cc0 = impl_witness_access constants.%As.impl_witness.9f4, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.d07]
// CHECK:STDOUT:   %bound_method.loc17_41.1: <bound method> = bound_method %int_1.loc17, %impl.elem0.loc17 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.d38]
// CHECK:STDOUT:   %specific_fn.loc17: <specific function> = specific_function %impl.elem0.loc17, @Core.IntLiteral.as.As.impl.Convert.1(constants.%int_64) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.97c]
// CHECK:STDOUT:   %bound_method.loc17_41.2: <bound method> = bound_method %int_1.loc17, %specific_fn.loc17 [concrete = constants.%bound_method.8f8]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc17: init %i64 = call %bound_method.loc17_41.2(%int_1.loc17) [concrete = constants.%int_1.41a]
// CHECK:STDOUT:   %.loc17_41.1: %i64 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc17 [concrete = constants.%int_1.41a]
// CHECK:STDOUT:   %.loc17_41.2: %i64 = converted %int_1.loc17, %.loc17_41.1 [concrete = constants.%int_1.41a]
// CHECK:STDOUT:   %.loc17_26: type = splice_block %long_long.ref [concrete = constants.%i64] {
// CHECK:STDOUT:     %Cpp.ref.loc17: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %long_long.ref: type = name_ref long_long, %i64.1 [concrete = constants.%i64]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_long_long: %i64 = bind_name cpp_long_long, %.loc17_41.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_long_long.patt: %pattern_type.95b = binding_pattern carbon_long_long [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_long_long.ref: %i64 = name_ref cpp_long_long, %cpp_long_long
// CHECK:STDOUT:   %.loc18: type = splice_block %i64.loc18 [concrete = constants.%i64] {
// CHECK:STDOUT:     %int_64.loc18: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:     %i64.loc18: type = class_type @Int, @Int(constants.%int_64) [concrete = constants.%i64]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_long_long: %i64 = bind_name carbon_long_long, %cpp_long_long.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_unsigned_char.patt: %pattern_type.8f3 = binding_pattern cpp_unsigned_char [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc20: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_8.loc20: Core.IntLiteral = int_value 8 [concrete = constants.%int_8]
// CHECK:STDOUT:   %u8.loc20: type = class_type @UInt, @UInt(constants.%int_8) [concrete = constants.%u8]
// CHECK:STDOUT:   %impl.elem0.loc20: %.f2b = impl_witness_access constants.%As.impl_witness.8bb, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.166]
// CHECK:STDOUT:   %bound_method.loc20_49.1: <bound method> = bound_method %int_1.loc20, %impl.elem0.loc20 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.32b]
// CHECK:STDOUT:   %specific_fn.loc20: <specific function> = specific_function %impl.elem0.loc20, @Core.IntLiteral.as.As.impl.Convert.2(constants.%int_8) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.f30]
// CHECK:STDOUT:   %bound_method.loc20_49.2: <bound method> = bound_method %int_1.loc20, %specific_fn.loc20 [concrete = constants.%bound_method.df5]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc20: init %u8 = call %bound_method.loc20_49.2(%int_1.loc20) [concrete = constants.%int_1.e80]
// CHECK:STDOUT:   %.loc20_49.1: %u8 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc20 [concrete = constants.%int_1.e80]
// CHECK:STDOUT:   %.loc20_49.2: %u8 = converted %int_1.loc20, %.loc20_49.1 [concrete = constants.%int_1.e80]
// CHECK:STDOUT:   %.loc20_30: type = splice_block %unsigned_char.ref [concrete = constants.%u8] {
// CHECK:STDOUT:     %Cpp.ref.loc20: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %unsigned_char.ref: type = name_ref unsigned_char, %u8.1 [concrete = constants.%u8]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_char: %u8 = bind_name cpp_unsigned_char, %.loc20_49.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_unsigned_char.patt: %pattern_type.8f3 = binding_pattern carbon_unsigned_char [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_char.ref: %u8 = name_ref cpp_unsigned_char, %cpp_unsigned_char
// CHECK:STDOUT:   %.loc21: type = splice_block %u8.loc21 [concrete = constants.%u8] {
// CHECK:STDOUT:     %int_8.loc21: Core.IntLiteral = int_value 8 [concrete = constants.%int_8]
// CHECK:STDOUT:     %u8.loc21: type = class_type @UInt, @UInt(constants.%int_8) [concrete = constants.%u8]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_unsigned_char: %u8 = bind_name carbon_unsigned_char, %cpp_unsigned_char.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_unsigned_short.patt: %pattern_type.9db = binding_pattern cpp_unsigned_short [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc23: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_16.loc23: Core.IntLiteral = int_value 16 [concrete = constants.%int_16]
// CHECK:STDOUT:   %u16.loc23: type = class_type @UInt, @UInt(constants.%int_16) [concrete = constants.%u16]
// CHECK:STDOUT:   %impl.elem0.loc23: %.e0b = impl_witness_access constants.%As.impl_witness.be9, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bfd]
// CHECK:STDOUT:   %bound_method.loc23_51.1: <bound method> = bound_method %int_1.loc23, %impl.elem0.loc23 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.8d6]
// CHECK:STDOUT:   %specific_fn.loc23: <specific function> = specific_function %impl.elem0.loc23, @Core.IntLiteral.as.As.impl.Convert.2(constants.%int_16) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.b40]
// CHECK:STDOUT:   %bound_method.loc23_51.2: <bound method> = bound_method %int_1.loc23, %specific_fn.loc23 [concrete = constants.%bound_method.779]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc23: init %u16 = call %bound_method.loc23_51.2(%int_1.loc23) [concrete = constants.%int_1.3e8]
// CHECK:STDOUT:   %.loc23_51.1: %u16 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc23 [concrete = constants.%int_1.3e8]
// CHECK:STDOUT:   %.loc23_51.2: %u16 = converted %int_1.loc23, %.loc23_51.1 [concrete = constants.%int_1.3e8]
// CHECK:STDOUT:   %.loc23_31: type = splice_block %unsigned_short.ref [concrete = constants.%u16] {
// CHECK:STDOUT:     %Cpp.ref.loc23: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %unsigned_short.ref: type = name_ref unsigned_short, %u16.1 [concrete = constants.%u16]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_short: %u16 = bind_name cpp_unsigned_short, %.loc23_51.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_unsigned_short.patt: %pattern_type.9db = binding_pattern carbon_unsigned_short [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_short.ref: %u16 = name_ref cpp_unsigned_short, %cpp_unsigned_short
// CHECK:STDOUT:   %.loc24: type = splice_block %u16.loc24 [concrete = constants.%u16] {
// CHECK:STDOUT:     %int_16.loc24: Core.IntLiteral = int_value 16 [concrete = constants.%int_16]
// CHECK:STDOUT:     %u16.loc24: type = class_type @UInt, @UInt(constants.%int_16) [concrete = constants.%u16]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_unsigned_short: %u16 = bind_name carbon_unsigned_short, %cpp_unsigned_short.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_unsigned_int.patt: %pattern_type.4a9 = binding_pattern cpp_unsigned_int [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc26: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_32.loc26: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %u32.loc26: type = class_type @UInt, @UInt(constants.%int_32) [concrete = constants.%u32]
// CHECK:STDOUT:   %impl.elem0.loc26: %.cbe = impl_witness_access constants.%As.impl_witness.666, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.358]
// CHECK:STDOUT:   %bound_method.loc26_47.1: <bound method> = bound_method %int_1.loc26, %impl.elem0.loc26 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.67d]
// CHECK:STDOUT:   %specific_fn.loc26: <specific function> = specific_function %impl.elem0.loc26, @Core.IntLiteral.as.As.impl.Convert.2(constants.%int_32) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.baf]
// CHECK:STDOUT:   %bound_method.loc26_47.2: <bound method> = bound_method %int_1.loc26, %specific_fn.loc26 [concrete = constants.%bound_method.d2b]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc26: init %u32 = call %bound_method.loc26_47.2(%int_1.loc26) [concrete = constants.%int_1.c1d]
// CHECK:STDOUT:   %.loc26_47.1: %u32 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc26 [concrete = constants.%int_1.c1d]
// CHECK:STDOUT:   %.loc26_47.2: %u32 = converted %int_1.loc26, %.loc26_47.1 [concrete = constants.%int_1.c1d]
// CHECK:STDOUT:   %.loc26_29: type = splice_block %unsigned_int.ref [concrete = constants.%u32] {
// CHECK:STDOUT:     %Cpp.ref.loc26: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %unsigned_int.ref: type = name_ref unsigned_int, %u32.1 [concrete = constants.%u32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_int: %u32 = bind_name cpp_unsigned_int, %.loc26_47.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_unsigned_int.patt: %pattern_type.4a9 = binding_pattern carbon_unsigned_int [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_int.ref: %u32 = name_ref cpp_unsigned_int, %cpp_unsigned_int
// CHECK:STDOUT:   %.loc27: type = splice_block %u32.loc27 [concrete = constants.%u32] {
// CHECK:STDOUT:     %int_32.loc27: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %u32.loc27: type = class_type @UInt, @UInt(constants.%int_32) [concrete = constants.%u32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_unsigned_int: %u32 = bind_name carbon_unsigned_int, %cpp_unsigned_int.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_unsigned_long_long.patt: %pattern_type.157 = binding_pattern cpp_unsigned_long_long [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %int_1.loc29: Core.IntLiteral = int_value 1 [concrete = constants.%int_1.5b8]
// CHECK:STDOUT:   %int_64.loc29: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:   %u64.loc29: type = class_type @UInt, @UInt(constants.%int_64) [concrete = constants.%u64]
// CHECK:STDOUT:   %impl.elem0.loc29: %.8d9 = impl_witness_access constants.%As.impl_witness.10d, element0 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.d63]
// CHECK:STDOUT:   %bound_method.loc29_59.1: <bound method> = bound_method %int_1.loc29, %impl.elem0.loc29 [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.bound.a7e]
// CHECK:STDOUT:   %specific_fn.loc29: <specific function> = specific_function %impl.elem0.loc29, @Core.IntLiteral.as.As.impl.Convert.2(constants.%int_64) [concrete = constants.%Core.IntLiteral.as.As.impl.Convert.specific_fn.a60]
// CHECK:STDOUT:   %bound_method.loc29_59.2: <bound method> = bound_method %int_1.loc29, %specific_fn.loc29 [concrete = constants.%bound_method.b3d]
// CHECK:STDOUT:   %Core.IntLiteral.as.As.impl.Convert.call.loc29: init %u64 = call %bound_method.loc29_59.2(%int_1.loc29) [concrete = constants.%int_1.f23]
// CHECK:STDOUT:   %.loc29_59.1: %u64 = value_of_initializer %Core.IntLiteral.as.As.impl.Convert.call.loc29 [concrete = constants.%int_1.f23]
// CHECK:STDOUT:   %.loc29_59.2: %u64 = converted %int_1.loc29, %.loc29_59.1 [concrete = constants.%int_1.f23]
// CHECK:STDOUT:   %.loc29_35: type = splice_block %unsigned_long_long.ref [concrete = constants.%u64] {
// CHECK:STDOUT:     %Cpp.ref.loc29: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %unsigned_long_long.ref: type = name_ref unsigned_long_long, %u64.1 [concrete = constants.%u64]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_long_long: %u64 = bind_name cpp_unsigned_long_long, %.loc29_59.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_unsigned_long_long.patt: %pattern_type.157 = binding_pattern carbon_unsigned_long_long [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_unsigned_long_long.ref: %u64 = name_ref cpp_unsigned_long_long, %cpp_unsigned_long_long
// CHECK:STDOUT:   %.loc30: type = splice_block %u64.loc30 [concrete = constants.%u64] {
// CHECK:STDOUT:     %int_64.loc30: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:     %u64.loc30: type = class_type @UInt, @UInt(constants.%int_64) [concrete = constants.%u64]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_unsigned_long_long: %u64 = bind_name carbon_unsigned_long_long, %cpp_unsigned_long_long.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_float.patt: %pattern_type.201 = binding_pattern cpp_float [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %float.loc32: Core.FloatLiteral = float_literal_value 10e-1 [concrete = constants.%float.674bbc.1]
// CHECK:STDOUT:   %int_32.loc32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:   %f32.loc32: type = class_type @Float, @Float(constants.%int_32) [concrete = constants.%f32.97e]
// CHECK:STDOUT:   %impl.elem0.loc32: %.372 = impl_witness_access constants.%As.impl_witness.992, element0 [concrete = constants.%Core.FloatLiteral.as.As.impl.Convert.3c7]
// CHECK:STDOUT:   %bound_method.loc32_35.1: <bound method> = bound_method %float.loc32, %impl.elem0.loc32 [concrete = constants.%Core.FloatLiteral.as.As.impl.Convert.bound.cc9]
// CHECK:STDOUT:   %specific_fn.loc32: <specific function> = specific_function %impl.elem0.loc32, @Core.FloatLiteral.as.As.impl.Convert(constants.%int_32) [concrete = constants.%Core.FloatLiteral.as.As.impl.Convert.specific_fn.4c6]
// CHECK:STDOUT:   %bound_method.loc32_35.2: <bound method> = bound_method %float.loc32, %specific_fn.loc32 [concrete = constants.%bound_method.100]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.call.loc32: init %f32.97e = call %bound_method.loc32_35.2(%float.loc32) [concrete = constants.%float.e3b]
// CHECK:STDOUT:   %.loc32_35.1: %f32.97e = value_of_initializer %Core.FloatLiteral.as.As.impl.Convert.call.loc32 [concrete = constants.%float.e3b]
// CHECK:STDOUT:   %.loc32_35.2: %f32.97e = converted %float.loc32, %.loc32_35.1 [concrete = constants.%float.e3b]
// CHECK:STDOUT:   %.loc32_22: type = splice_block %float.ref [concrete = constants.%f32.97e] {
// CHECK:STDOUT:     %Cpp.ref.loc32: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %float.ref: type = name_ref float, %f32.1 [concrete = constants.%f32.97e]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_float: %f32.97e = bind_name cpp_float, %.loc32_35.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_float.patt: %pattern_type.201 = binding_pattern carbon_float [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_float.ref: %f32.97e = name_ref cpp_float, %cpp_float
// CHECK:STDOUT:   %.loc33: type = splice_block %f32.loc33 [concrete = constants.%f32.97e] {
// CHECK:STDOUT:     %int_32.loc33: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %f32.loc33: type = class_type @Float, @Float(constants.%int_32) [concrete = constants.%f32.97e]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_float: %f32.97e = bind_name carbon_float, %cpp_float.ref
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %cpp_double.patt: %pattern_type.0ae = binding_pattern cpp_double [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %float.loc35: Core.FloatLiteral = float_literal_value 10e-1 [concrete = constants.%float.674bbc.2]
// CHECK:STDOUT:   %int_64.loc35: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:   %f64.loc35: type = class_type @Float, @Float(constants.%int_64) [concrete = constants.%f64.d77]
// CHECK:STDOUT:   %impl.elem0.loc35: %.ec3 = impl_witness_access constants.%As.impl_witness.b46, element0 [concrete = constants.%Core.FloatLiteral.as.As.impl.Convert.251]
// CHECK:STDOUT:   %bound_method.loc35_37.1: <bound method> = bound_method %float.loc35, %impl.elem0.loc35 [concrete = constants.%Core.FloatLiteral.as.As.impl.Convert.bound.64d]
// CHECK:STDOUT:   %specific_fn.loc35: <specific function> = specific_function %impl.elem0.loc35, @Core.FloatLiteral.as.As.impl.Convert(constants.%int_64) [concrete = constants.%Core.FloatLiteral.as.As.impl.Convert.specific_fn.150]
// CHECK:STDOUT:   %bound_method.loc35_37.2: <bound method> = bound_method %float.loc35, %specific_fn.loc35 [concrete = constants.%bound_method.5cc]
// CHECK:STDOUT:   %Core.FloatLiteral.as.As.impl.Convert.call.loc35: init %f64.d77 = call %bound_method.loc35_37.2(%float.loc35) [concrete = constants.%float.d20]
// CHECK:STDOUT:   %.loc35_37.1: %f64.d77 = value_of_initializer %Core.FloatLiteral.as.As.impl.Convert.call.loc35 [concrete = constants.%float.d20]
// CHECK:STDOUT:   %.loc35_37.2: %f64.d77 = converted %float.loc35, %.loc35_37.1 [concrete = constants.%float.d20]
// CHECK:STDOUT:   %.loc35_23: type = splice_block %double.ref [concrete = constants.%f64.d77] {
// CHECK:STDOUT:     %Cpp.ref.loc35: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:     %double.ref: type = name_ref double, %f64.1 [concrete = constants.%f64.d77]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_double: %f64.d77 = bind_name cpp_double, %.loc35_37.2
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %carbon_double.patt: %pattern_type.0ae = binding_pattern carbon_double [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %cpp_double.ref: %f64.d77 = name_ref cpp_double, %cpp_double
// CHECK:STDOUT:   %.loc36: type = splice_block %f64.loc36 [concrete = constants.%f64.d77] {
// CHECK:STDOUT:     %int_64.loc36: Core.IntLiteral = int_value 64 [concrete = constants.%int_64]
// CHECK:STDOUT:     %f64.loc36: type = class_type @Float, @Float(constants.%int_64) [concrete = constants.%f64.d77]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %carbon_double: %f64.d77 = bind_name carbon_double, %cpp_double.ref
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- override_builtin.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %unsigned_int: type = class_type @unsigned_int [concrete]
// CHECK:STDOUT:   %pattern_type.5ec: type = pattern_type %unsigned_int [concrete]
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %u32: type = class_type @UInt, @UInt(%int_32) [concrete]
// CHECK:STDOUT:   %pattern_type.4a9: type = pattern_type %u32 [concrete]
// CHECK:STDOUT:   %ptr.d47: type = ptr_type %unsigned_int [concrete]
// CHECK:STDOUT:   %unsigned_int.foo.type: type = fn_type @unsigned_int.foo [concrete]
// CHECK:STDOUT:   %unsigned_int.foo: %unsigned_int.foo.type = struct_value () [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.f3c: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%unsigned_int) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.293: %T.as.Destroy.impl.Op.type.f3c = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Cpp: <namespace> = namespace file.%Cpp.import_cpp, [concrete] {
// CHECK:STDOUT:     .unsigned_int = %unsigned_int.decl
// CHECK:STDOUT:     import Cpp//...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %unsigned_int.decl: type = class_decl @unsigned_int [concrete = constants.%unsigned_int] {} {}
// CHECK:STDOUT:   %unsigned_int.foo.decl: %unsigned_int.foo.type = fn_decl @unsigned_int.foo [concrete = constants.%unsigned_int.foo] {
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     <elided>
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %unsigned_int.patt: %pattern_type.5ec = binding_pattern unsigned_int [concrete]
// CHECK:STDOUT:     %unsigned_int.var_patt: %pattern_type.5ec = var_pattern %unsigned_int.patt [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %unsigned_int.var: ref %unsigned_int = var %unsigned_int.var_patt
// CHECK:STDOUT:   %.loc12: type = splice_block %unsigned_int.ref.loc12 [concrete = constants.%unsigned_int] {
// CHECK:STDOUT:     %Cpp.ref: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:     %unsigned_int.ref.loc12: type = name_ref unsigned_int, imports.%unsigned_int.decl [concrete = constants.%unsigned_int]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %unsigned_int: ref %unsigned_int = bind_name unsigned_int, %unsigned_int.var
// CHECK:STDOUT:   name_binding_decl {
// CHECK:STDOUT:     %x.patt: %pattern_type.4a9 = binding_pattern x [concrete]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %unsigned_int.ref.loc13: ref %unsigned_int = name_ref unsigned_int, %unsigned_int
// CHECK:STDOUT:   %foo.ref: %unsigned_int.foo.type = name_ref foo, imports.%unsigned_int.foo.decl [concrete = constants.%unsigned_int.foo]
// CHECK:STDOUT:   %unsigned_int.foo.bound: <bound method> = bound_method %unsigned_int.ref.loc13, %foo.ref
// CHECK:STDOUT:   %addr.loc13: %ptr.d47 = addr_of %unsigned_int.ref.loc13
// CHECK:STDOUT:   %unsigned_int.foo.call: init %u32 = call %unsigned_int.foo.bound(%addr.loc13)
// CHECK:STDOUT:   %.loc13_10: type = splice_block %u32 [concrete = constants.%u32] {
// CHECK:STDOUT:     %int_32: Core.IntLiteral = int_value 32 [concrete = constants.%int_32]
// CHECK:STDOUT:     %u32: type = class_type @UInt, @UInt(constants.%int_32) [concrete = constants.%u32]
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.loc13_33.1: %u32 = value_of_initializer %unsigned_int.foo.call
// CHECK:STDOUT:   %.loc13_33.2: %u32 = converted %unsigned_int.foo.call, %.loc13_33.1
// CHECK:STDOUT:   %x: %u32 = bind_name x, %.loc13_33.2
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound: <bound method> = bound_method %unsigned_int.var, constants.%T.as.Destroy.impl.Op.293
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT:   %bound_method: <bound method> = bound_method %unsigned_int.var, %T.as.Destroy.impl.Op.specific_fn
// CHECK:STDOUT:   %addr.loc12: %ptr.d47 = addr_of %unsigned_int.var
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call: init %empty_tuple.type = call %bound_method(%addr.loc12)
// CHECK:STDOUT:   <elided>
// CHECK:STDOUT: }
// CHECK:STDOUT:
