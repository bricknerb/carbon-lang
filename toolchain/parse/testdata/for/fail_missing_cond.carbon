// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/for/fail_missing_cond.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/for/fail_missing_cond.carbon

fn F() {
  // CHECK:STDERR: fail_missing_cond.carbon:[[@LINE+8]]:7: error: expected `(` after `for` [ExpectedParenAfter]
  // CHECK:STDERR:   for {
  // CHECK:STDERR:       ^
  // CHECK:STDERR:
  // CHECK:STDERR: fail_missing_cond.carbon:[[@LINE+4]]:7: error: expected name in binding pattern [ExpectedBindingPattern]
  // CHECK:STDERR:   for {
  // CHECK:STDERR:       ^
  // CHECK:STDERR:
  for {
  }
// CHECK:STDERR: fail_missing_cond.carbon:[[@LINE+8]]:1: error: expected braced code block [ExpectedCodeBlock]
// CHECK:STDERR: }
// CHECK:STDERR: ^
// CHECK:STDERR:
// CHECK:STDERR: fail_missing_cond.carbon:[[@LINE+4]]:1: error: expected expression [ExpectedExpr]
// CHECK:STDERR: }
// CHECK:STDERR: ^
// CHECK:STDERR:
}

// CHECK:STDOUT: - filename: fail_missing_cond.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'F'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:           {kind: 'ForHeaderStart', text: 'for', has_error: yes},
// CHECK:STDOUT:               {kind: 'IdentifierNameNotBeforeParams', text: '{', has_error: yes},
// CHECK:STDOUT:               {kind: 'InvalidParse', text: '{', has_error: yes},
// CHECK:STDOUT:             {kind: 'LetBindingPattern', text: '{', has_error: yes, subtree_size: 3},
// CHECK:STDOUT:           {kind: 'ForIn', text: 'for', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:             {kind: 'StructLiteralStart', text: '{'},
// CHECK:STDOUT:           {kind: 'StructLiteral', text: '}', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'ForHeader', text: 'for', has_error: yes, subtree_size: 8},
// CHECK:STDOUT:           {kind: 'CodeBlockStart', text: '}', has_error: yes},
// CHECK:STDOUT:             {kind: 'InvalidParse', text: '}', has_error: yes},
// CHECK:STDOUT:           {kind: 'ExprStatement', text: '}', has_error: yes, subtree_size: 2},
// CHECK:STDOUT:         {kind: 'CodeBlock', text: '}', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:       {kind: 'ForStatement', text: 'for', subtree_size: 13},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 19},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
