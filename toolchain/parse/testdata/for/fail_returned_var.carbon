// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/for/fail_returned_var.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/for/fail_returned_var.carbon

fn foo() -> i32 {
  // CHECK:STDERR: fail_returned_var.carbon:[[@LINE+4]]:8: error: expected name in binding pattern [ExpectedBindingPattern]
  // CHECK:STDERR:   for (returned var x: i32 in y) {
  // CHECK:STDERR:        ^~~~~~~~
  // CHECK:STDERR:
  for (returned var x: i32 in y) {
    return var;
  }
}

// CHECK:STDOUT: - filename: fail_returned_var.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'foo'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:           {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:         {kind: 'ReturnType', text: '->', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 7},
// CHECK:STDOUT:           {kind: 'ForHeaderStart', text: '('},
// CHECK:STDOUT:               {kind: 'IdentifierNameNotBeforeParams', text: 'returned', has_error: yes},
// CHECK:STDOUT:               {kind: 'InvalidParse', text: 'returned', has_error: yes},
// CHECK:STDOUT:             {kind: 'LetBindingPattern', text: 'returned', has_error: yes, subtree_size: 3},
// CHECK:STDOUT:           {kind: 'ForIn', text: '(', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:         {kind: 'ForHeader', text: ')', has_error: yes, subtree_size: 6},
// CHECK:STDOUT:           {kind: 'CodeBlockStart', text: '{'},
// CHECK:STDOUT:             {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'ReturnVarModifier', text: 'var'},
// CHECK:STDOUT:           {kind: 'ReturnStatement', text: ';', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'CodeBlock', text: '}', subtree_size: 5},
// CHECK:STDOUT:       {kind: 'ForStatement', text: 'for', subtree_size: 12},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 20},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
