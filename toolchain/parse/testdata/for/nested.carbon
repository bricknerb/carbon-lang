// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/for/nested.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/for/nested.carbon

fn foo() {
  for (var y: i32 in x) {
      for (var z: i32 in y) {
          Print(z);
      }
  }
}

// CHECK:STDOUT: - filename: nested.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'foo'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:           {kind: 'ForHeaderStart', text: '('},
// CHECK:STDOUT:                 {kind: 'IdentifierNameNotBeforeParams', text: 'y'},
// CHECK:STDOUT:                 {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:               {kind: 'VarBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'VariablePattern', text: 'var', subtree_size: 4},
// CHECK:STDOUT:           {kind: 'ForIn', text: 'in', subtree_size: 5},
// CHECK:STDOUT:           {kind: 'IdentifierNameExpr', text: 'x'},
// CHECK:STDOUT:         {kind: 'ForHeader', text: ')', subtree_size: 8},
// CHECK:STDOUT:           {kind: 'CodeBlockStart', text: '{'},
// CHECK:STDOUT:               {kind: 'ForHeaderStart', text: '('},
// CHECK:STDOUT:                     {kind: 'IdentifierNameNotBeforeParams', text: 'z'},
// CHECK:STDOUT:                     {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:                   {kind: 'VarBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:                 {kind: 'VariablePattern', text: 'var', subtree_size: 4},
// CHECK:STDOUT:               {kind: 'ForIn', text: 'in', subtree_size: 5},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'y'},
// CHECK:STDOUT:             {kind: 'ForHeader', text: ')', subtree_size: 8},
// CHECK:STDOUT:               {kind: 'CodeBlockStart', text: '{'},
// CHECK:STDOUT:                     {kind: 'IdentifierNameExpr', text: 'Print'},
// CHECK:STDOUT:                   {kind: 'CallExprStart', text: '(', subtree_size: 2},
// CHECK:STDOUT:                   {kind: 'IdentifierNameExpr', text: 'z'},
// CHECK:STDOUT:                 {kind: 'CallExpr', text: ')', subtree_size: 4},
// CHECK:STDOUT:               {kind: 'ExprStatement', text: ';', subtree_size: 5},
// CHECK:STDOUT:             {kind: 'CodeBlock', text: '}', subtree_size: 7},
// CHECK:STDOUT:           {kind: 'ForStatement', text: 'for', subtree_size: 16},
// CHECK:STDOUT:         {kind: 'CodeBlock', text: '}', subtree_size: 18},
// CHECK:STDOUT:       {kind: 'ForStatement', text: 'for', subtree_size: 27},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 33},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
