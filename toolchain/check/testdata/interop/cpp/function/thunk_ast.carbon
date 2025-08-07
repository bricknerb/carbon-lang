// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/int.carbon
// EXTRA-ARGS: --dump-cpp-ast
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interop/cpp/function/thunk_ast.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interop/cpp/function/thunk_ast.carbon
// CHECK:STDOUT: TranslationUnitDecl {{0x[a-f0-9]+}} <<invalid sloc>> <invalid sloc>
// CHECK:STDOUT: |-TypedefDecl {{0x[a-f0-9]+}} <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
// CHECK:STDOUT: | `-BuiltinType {{0x[a-f0-9]+}} '__int128'
// CHECK:STDOUT: |-TypedefDecl {{0x[a-f0-9]+}} <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
// CHECK:STDOUT: | `-BuiltinType {{0x[a-f0-9]+}} 'unsigned __int128'
// CHECK:STDOUT: |-TypedefDecl {{0x[a-f0-9]+}} <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
// CHECK:STDOUT: | `-RecordType {{0x[a-f0-9]+}} '__NSConstantString_tag'
// CHECK:STDOUT: |   `-CXXRecord {{0x[a-f0-9]+}} '__NSConstantString_tag'
// CHECK:STDOUT: |-TypedefDecl {{0x[a-f0-9]+}} <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
// CHECK:STDOUT: | `-PointerType {{0x[a-f0-9]+}} 'char *'
// CHECK:STDOUT: |   `-BuiltinType {{0x[a-f0-9]+}} 'char'
// CHECK:STDOUT: |-TypedefDecl {{0x[a-f0-9]+}} <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag[1]'
// CHECK:STDOUT: | `-ConstantArrayType {{0x[a-f0-9]+}} '__va_list_tag[1]' 1 {{}}
// CHECK:STDOUT: |   `-RecordType {{0x[a-f0-9]+}} '__va_list_tag'
// CHECK:STDOUT: |     `-CXXRecord {{0x[a-f0-9]+}} '__va_list_tag'

// --- thunk_required.h

auto foo(short a) -> void;
// CHECK:STDOUT: |-FunctionDecl {{0x[a-f0-9]+}} <./thunk_required.h:[[@LINE-1]]:1, col:22> col:6 used foo 'auto (short) -> void'
// CHECK:STDOUT: | `-ParmVarDecl {{0x[a-f0-9]+}} <col:10, col:16> col:16 a 'short'
// CHECK:STDOUT: `-FunctionDecl {{0x[a-f0-9]+}} <col:6> col:6 foo__carbon_thunk 'auto (short * _Nonnull) -> void' extern
// CHECK:STDOUT:   |-ParmVarDecl {{0x[a-f0-9]+}} <col:6> col:6 used a 'short * _Nonnull':'short *'
// CHECK:STDOUT:   |-ReturnStmt {{0x[a-f0-9]+}} <col:6>
// CHECK:STDOUT:   | `-CallExpr {{0x[a-f0-9]+}} <col:6> 'void'
// CHECK:STDOUT:   |   |-ImplicitCastExpr {{0x[a-f0-9]+}} <col:6> 'auto (*)(short) -> void' <FunctionToPointerDecay>
// CHECK:STDOUT:   |   | `-DeclRefExpr {{0x[a-f0-9]+}} <col:6> 'auto (short) -> void' Function {{0x[a-f0-9]+}} 'foo' 'auto (short) -> void'
// CHECK:STDOUT:   |   `-ImplicitCastExpr {{0x[a-f0-9]+}} <col:6> 'short' <LValueToRValue>
// CHECK:STDOUT:   |     `-UnaryOperator {{0x[a-f0-9]+}} <col:6> 'short' lvalue prefix '*' cannot overflow
// CHECK:STDOUT:   |       `-ImplicitCastExpr {{0x[a-f0-9]+}} <col:6> 'short * _Nonnull':'short *' <LValueToRValue>
// CHECK:STDOUT:   |         `-DeclRefExpr {{0x[a-f0-9]+}} <col:6> 'short * _Nonnull':'short *' lvalue ParmVar {{0x[a-f0-9]+}} 'a' 'short * _Nonnull':'short *'
// CHECK:STDOUT:   |-AlwaysInlineAttr {{0x[a-f0-9]+}} <<invalid sloc>> Implicit always_inline
// CHECK:STDOUT:   `-AsmLabelAttr {{0x[a-f0-9]+}} <col:6> Implicit "_Z3foos.carbon_thunk"

// --- import_thunk_required.carbon

library "[[@TEST_NAME]]";

import Cpp library "thunk_required.h";

fn F() {
  Cpp.foo(1 as i16);
}
