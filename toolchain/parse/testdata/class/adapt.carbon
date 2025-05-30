// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/class/adapt.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/class/adapt.carbon

// --- adapt.carbon

class A {
  adapt ();
}

class B {
  adapt A;
}

class C {
  adapt i32;
}

class D {
  extend adapt {.a: i32, .b: i32};
}

class E {
  adapt GetType(42);
}

// This is syntactically OK but semantically invalid.
adapt i32;

// --- fail_bad_syntax.carbon

class A {
  // CHECK:STDERR: fail_bad_syntax.carbon:[[@LINE+4]]:11: error: `adapt` declarations must end with a `;` [ExpectedDeclSemi]
  // CHECK:STDERR:   adapt 1 trailing_token;
  // CHECK:STDERR:           ^~~~~~~~~~~~~~
  // CHECK:STDERR:
  adapt 1 trailing_token;
}

class B {
  // CHECK:STDERR: fail_bad_syntax.carbon:[[@LINE+4]]:8: error: expected expression [ExpectedExpr]
  // CHECK:STDERR:   adapt;
  // CHECK:STDERR:        ^
  // CHECK:STDERR:
  adapt;
}

class C {
  adapt
// CHECK:STDERR: fail_bad_syntax.carbon:[[@LINE+4]]:1: error: expected expression [ExpectedExpr]
// CHECK:STDERR: }
// CHECK:STDERR: ^
// CHECK:STDERR:
}

class D {
  // CHECK:STDERR: fail_bad_syntax.carbon:[[@LINE+4]]:9: error: expected expression [ExpectedExpr]
  // CHECK:STDERR:   adapt class;
  // CHECK:STDERR:         ^~~~~
  // CHECK:STDERR:
  adapt class;
}

fn F() {
  adapt i32;
}

// CHECK:STDOUT: - filename: adapt.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'A'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:           {kind: 'TupleLiteralStart', text: '('},
// CHECK:STDOUT:         {kind: 'TupleLiteral', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', subtree_size: 4},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 8},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'B'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'IdentifierNameExpr', text: 'A'},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', subtree_size: 3},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'C'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', subtree_size: 3},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'D'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'ExtendModifier', text: 'extend'},
// CHECK:STDOUT:           {kind: 'StructTypeLiteralStart', text: '{'},
// CHECK:STDOUT:               {kind: 'IdentifierNameNotBeforeParams', text: 'a'},
// CHECK:STDOUT:             {kind: 'StructFieldDesignator', text: '.', subtree_size: 2},
// CHECK:STDOUT:             {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:           {kind: 'StructTypeLiteralField', text: ':', subtree_size: 4},
// CHECK:STDOUT:           {kind: 'StructTypeLiteralComma', text: ','},
// CHECK:STDOUT:               {kind: 'IdentifierNameNotBeforeParams', text: 'b'},
// CHECK:STDOUT:             {kind: 'StructFieldDesignator', text: '.', subtree_size: 2},
// CHECK:STDOUT:             {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:           {kind: 'StructTypeLiteralField', text: ':', subtree_size: 4},
// CHECK:STDOUT:         {kind: 'StructTypeLiteral', text: '}', subtree_size: 11},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', subtree_size: 14},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 18},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'E'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:             {kind: 'IdentifierNameExpr', text: 'GetType'},
// CHECK:STDOUT:           {kind: 'CallExprStart', text: '(', subtree_size: 2},
// CHECK:STDOUT:           {kind: 'IntLiteral', text: '42'},
// CHECK:STDOUT:         {kind: 'CallExpr', text: ')', subtree_size: 4},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', subtree_size: 6},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 10},
// CHECK:STDOUT:       {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:       {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:     {kind: 'AdaptDecl', text: ';', subtree_size: 3},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
// CHECK:STDOUT: - filename: fail_bad_syntax.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'A'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'IntLiteral', text: '1'},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', has_error: yes, subtree_size: 3},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'B'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'InvalidParse', text: ';', has_error: yes},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', has_error: yes, subtree_size: 3},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'C'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'InvalidParse', text: '}', has_error: yes},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: 'adapt', has_error: yes, subtree_size: 3},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'D'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'InvalidParse', text: 'class', has_error: yes},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', has_error: yes, subtree_size: 3},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'F'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:         {kind: 'AdaptIntroducer', text: 'adapt'},
// CHECK:STDOUT:         {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:       {kind: 'AdaptDecl', text: ';', subtree_size: 3},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 9},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
