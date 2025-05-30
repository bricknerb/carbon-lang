// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/class/fn_definitions.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/class/fn_definitions.carbon

class Foo {
  fn Make() -> Self { return {.x = 0}; }

  fn Baz[self: Self]() -> i32 { return self.x; }

  fn Modify[addr self: Self*]() { self->x = 1; }
}

// CHECK:STDOUT: - filename: fn_definitions.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'Foo'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:           {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:           {kind: 'IdentifierNameBeforeParams', text: 'Make'},
// CHECK:STDOUT:             {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:           {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:             {kind: 'SelfTypeNameExpr', text: 'Self'},
// CHECK:STDOUT:           {kind: 'ReturnType', text: '->', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 7},
// CHECK:STDOUT:           {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'StructLiteralStart', text: '{'},
// CHECK:STDOUT:                 {kind: 'IdentifierNameNotBeforeParams', text: 'x'},
// CHECK:STDOUT:               {kind: 'StructFieldDesignator', text: '.', subtree_size: 2},
// CHECK:STDOUT:               {kind: 'IntLiteral', text: '0'},
// CHECK:STDOUT:             {kind: 'StructLiteralField', text: '=', subtree_size: 4},
// CHECK:STDOUT:           {kind: 'StructLiteral', text: '}', subtree_size: 6},
// CHECK:STDOUT:         {kind: 'ReturnStatement', text: ';', subtree_size: 8},
// CHECK:STDOUT:       {kind: 'FunctionDefinition', text: '}', subtree_size: 16},
// CHECK:STDOUT:           {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:           {kind: 'IdentifierNameBeforeParams', text: 'Baz'},
// CHECK:STDOUT:             {kind: 'ImplicitParamListStart', text: '['},
// CHECK:STDOUT:               {kind: 'SelfValueName', text: 'self'},
// CHECK:STDOUT:               {kind: 'SelfTypeNameExpr', text: 'Self'},
// CHECK:STDOUT:             {kind: 'LetBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:           {kind: 'ImplicitParamList', text: ']', subtree_size: 5},
// CHECK:STDOUT:             {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:           {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:             {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:           {kind: 'ReturnType', text: '->', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 12},
// CHECK:STDOUT:           {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'SelfValueNameExpr', text: 'self'},
// CHECK:STDOUT:             {kind: 'IdentifierNameNotBeforeParams', text: 'x'},
// CHECK:STDOUT:           {kind: 'MemberAccessExpr', text: '.', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'ReturnStatement', text: ';', subtree_size: 5},
// CHECK:STDOUT:       {kind: 'FunctionDefinition', text: '}', subtree_size: 18},
// CHECK:STDOUT:           {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:           {kind: 'IdentifierNameBeforeParams', text: 'Modify'},
// CHECK:STDOUT:             {kind: 'ImplicitParamListStart', text: '['},
// CHECK:STDOUT:                 {kind: 'SelfValueName', text: 'self'},
// CHECK:STDOUT:                   {kind: 'SelfTypeNameExpr', text: 'Self'},
// CHECK:STDOUT:                 {kind: 'PostfixOperatorStar', text: '*', subtree_size: 2},
// CHECK:STDOUT:               {kind: 'LetBindingPattern', text: ':', subtree_size: 4},
// CHECK:STDOUT:             {kind: 'Addr', text: 'addr', subtree_size: 5},
// CHECK:STDOUT:           {kind: 'ImplicitParamList', text: ']', subtree_size: 7},
// CHECK:STDOUT:             {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:           {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 12},
// CHECK:STDOUT:               {kind: 'SelfValueNameExpr', text: 'self'},
// CHECK:STDOUT:               {kind: 'IdentifierNameNotBeforeParams', text: 'x'},
// CHECK:STDOUT:             {kind: 'PointerMemberAccessExpr', text: '->', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'IntLiteral', text: '1'},
// CHECK:STDOUT:           {kind: 'InfixOperatorEqual', text: '=', subtree_size: 5},
// CHECK:STDOUT:         {kind: 'ExprStatement', text: ';', subtree_size: 6},
// CHECK:STDOUT:       {kind: 'FunctionDefinition', text: '}', subtree_size: 19},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 57},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
