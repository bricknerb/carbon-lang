// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/if/fail_else_unbraced.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/if/fail_else_unbraced.carbon

fn F() {
  if (a)
    // CHECK:STDERR: fail_else_unbraced.carbon:[[@LINE+4]]:5: error: expected braced code block [ExpectedCodeBlock]
    // CHECK:STDERR:     if (b)
    // CHECK:STDERR:     ^~
    // CHECK:STDERR:
    if (b)
      // CHECK:STDERR: fail_else_unbraced.carbon:[[@LINE+4]]:7: error: expected braced code block [ExpectedCodeBlock]
      // CHECK:STDERR:       c;
      // CHECK:STDERR:       ^
      // CHECK:STDERR:
      c;
    else
      // CHECK:STDERR: fail_else_unbraced.carbon:[[@LINE+4]]:7: error: expected braced code block [ExpectedCodeBlock]
      // CHECK:STDERR:       d;
      // CHECK:STDERR:       ^
      // CHECK:STDERR:
      d;
  else
    // CHECK:STDERR: fail_else_unbraced.carbon:[[@LINE+4]]:5: error: expected braced code block [ExpectedCodeBlock]
    // CHECK:STDERR:     e;
    // CHECK:STDERR:     ^
    // CHECK:STDERR:
    e;
  if (x) { f; }
  else if (x) { g; }
  else { h; }
}

// CHECK:STDOUT: - filename: fail_else_unbraced.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'F'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:           {kind: 'IfConditionStart', text: '('},
// CHECK:STDOUT:           {kind: 'IdentifierNameExpr', text: 'a'},
// CHECK:STDOUT:         {kind: 'IfCondition', text: ')', subtree_size: 3},
// CHECK:STDOUT:           {kind: 'CodeBlockStart', text: 'if', has_error: yes},
// CHECK:STDOUT:               {kind: 'IfConditionStart', text: '('},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'b'},
// CHECK:STDOUT:             {kind: 'IfCondition', text: ')', subtree_size: 3},
// CHECK:STDOUT:               {kind: 'CodeBlockStart', text: 'c', has_error: yes},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'c'},
// CHECK:STDOUT:               {kind: 'ExprStatement', text: ';', subtree_size: 2},
// CHECK:STDOUT:             {kind: 'CodeBlock', text: 'c', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:             {kind: 'IfStatementElse', text: 'else'},
// CHECK:STDOUT:               {kind: 'CodeBlockStart', text: 'd', has_error: yes},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'd'},
// CHECK:STDOUT:               {kind: 'ExprStatement', text: ';', subtree_size: 2},
// CHECK:STDOUT:             {kind: 'CodeBlock', text: 'd', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:           {kind: 'IfStatement', text: 'if', subtree_size: 13},
// CHECK:STDOUT:         {kind: 'CodeBlock', text: 'if', has_error: yes, subtree_size: 15},
// CHECK:STDOUT:         {kind: 'IfStatementElse', text: 'else'},
// CHECK:STDOUT:           {kind: 'CodeBlockStart', text: 'e', has_error: yes},
// CHECK:STDOUT:             {kind: 'IdentifierNameExpr', text: 'e'},
// CHECK:STDOUT:           {kind: 'ExprStatement', text: ';', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'CodeBlock', text: 'e', has_error: yes, subtree_size: 4},
// CHECK:STDOUT:       {kind: 'IfStatement', text: 'if', subtree_size: 24},
// CHECK:STDOUT:           {kind: 'IfConditionStart', text: '('},
// CHECK:STDOUT:           {kind: 'IdentifierNameExpr', text: 'x'},
// CHECK:STDOUT:         {kind: 'IfCondition', text: ')', subtree_size: 3},
// CHECK:STDOUT:           {kind: 'CodeBlockStart', text: '{'},
// CHECK:STDOUT:             {kind: 'IdentifierNameExpr', text: 'f'},
// CHECK:STDOUT:           {kind: 'ExprStatement', text: ';', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'CodeBlock', text: '}', subtree_size: 4},
// CHECK:STDOUT:         {kind: 'IfStatementElse', text: 'else'},
// CHECK:STDOUT:             {kind: 'IfConditionStart', text: '('},
// CHECK:STDOUT:             {kind: 'IdentifierNameExpr', text: 'x'},
// CHECK:STDOUT:           {kind: 'IfCondition', text: ')', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'CodeBlockStart', text: '{'},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'g'},
// CHECK:STDOUT:             {kind: 'ExprStatement', text: ';', subtree_size: 2},
// CHECK:STDOUT:           {kind: 'CodeBlock', text: '}', subtree_size: 4},
// CHECK:STDOUT:           {kind: 'IfStatementElse', text: 'else'},
// CHECK:STDOUT:             {kind: 'CodeBlockStart', text: '{'},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'h'},
// CHECK:STDOUT:             {kind: 'ExprStatement', text: ';', subtree_size: 2},
// CHECK:STDOUT:           {kind: 'CodeBlock', text: '}', subtree_size: 4},
// CHECK:STDOUT:         {kind: 'IfStatement', text: 'if', subtree_size: 13},
// CHECK:STDOUT:       {kind: 'IfStatement', text: 'if', subtree_size: 22},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 52},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
