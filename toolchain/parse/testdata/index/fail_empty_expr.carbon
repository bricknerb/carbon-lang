// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/index/fail_empty_expr.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/index/fail_empty_expr.carbon

// CHECK:STDERR: fail_empty_expr.carbon:[[@LINE+4]]:16: error: expected expression [ExpectedExpr]
// CHECK:STDERR: var v: i32 = t[];
// CHECK:STDERR:                ^
// CHECK:STDERR:
var v: i32 = t[];

// CHECK:STDOUT: - filename: fail_empty_expr.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:       {kind: 'VariableIntroducer', text: 'var'},
// CHECK:STDOUT:           {kind: 'IdentifierNameNotBeforeParams', text: 'v'},
// CHECK:STDOUT:           {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:         {kind: 'VarBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:       {kind: 'VariablePattern', text: 'var', subtree_size: 4},
// CHECK:STDOUT:       {kind: 'VariableInitializer', text: '='},
// CHECK:STDOUT:           {kind: 'IdentifierNameExpr', text: 't'},
// CHECK:STDOUT:         {kind: 'IndexExprStart', text: '[', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'InvalidParse', text: ']', has_error: yes},
// CHECK:STDOUT:       {kind: 'IndexExpr', text: ']', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:     {kind: 'VariableDecl', text: ';', subtree_size: 11},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
