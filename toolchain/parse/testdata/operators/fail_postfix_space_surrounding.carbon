// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/operators/fail_postfix_space_surrounding.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/operators/fail_postfix_space_surrounding.carbon

// CHECK:STDERR: fail_postfix_space_surrounding.carbon:[[@LINE+4]]:18: error: whitespace is not allowed before this unary operator [UnaryOperatorHasWhitespace]
// CHECK:STDERR: var v: type = i8 * ;
// CHECK:STDERR:                  ^
// CHECK:STDERR:
var v: type = i8 * ;

// CHECK:STDOUT: - filename: fail_postfix_space_surrounding.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:       {kind: 'VariableIntroducer', text: 'var'},
// CHECK:STDOUT:           {kind: 'IdentifierNameNotBeforeParams', text: 'v'},
// CHECK:STDOUT:           {kind: 'TypeTypeLiteral', text: 'type'},
// CHECK:STDOUT:         {kind: 'VarBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:       {kind: 'VariablePattern', text: 'var', subtree_size: 4},
// CHECK:STDOUT:       {kind: 'VariableInitializer', text: '='},
// CHECK:STDOUT:         {kind: 'IntTypeLiteral', text: 'i8'},
// CHECK:STDOUT:       {kind: 'PostfixOperatorStar', text: '*', subtree_size: 2},
// CHECK:STDOUT:     {kind: 'VariableDecl', text: ';', subtree_size: 9},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
