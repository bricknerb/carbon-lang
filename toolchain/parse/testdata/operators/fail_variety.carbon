// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/operators/fail_variety.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/operators/fail_variety.carbon

fn F() {
  // CHECK:STDERR: fail_variety.carbon:[[@LINE+20]]:21: error: operator `=` can only be used as a complete statement [StatementOperatorAsSubExpr]
  // CHECK:STDERR:   n = a * b + c * d = d * d << e & f - not g;
  // CHECK:STDERR:                     ^
  // CHECK:STDERR:
  // CHECK:STDERR: fail_variety.carbon:[[@LINE+16]]:29: error: parentheses are required to disambiguate operator precedence [OperatorRequiresParentheses]
  // CHECK:STDERR:   n = a * b + c * d = d * d << e & f - not g;
  // CHECK:STDERR:                             ^~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_variety.carbon:[[@LINE+12]]:34: error: parentheses are required to disambiguate operator precedence [OperatorRequiresParentheses]
  // CHECK:STDERR:   n = a * b + c * d = d * d << e & f - not g;
  // CHECK:STDERR:                                  ^
  // CHECK:STDERR:
  // CHECK:STDERR: fail_variety.carbon:[[@LINE+8]]:38: error: parentheses are required to disambiguate operator precedence [OperatorRequiresParentheses]
  // CHECK:STDERR:   n = a * b + c * d = d * d << e & f - not g;
  // CHECK:STDERR:                                      ^
  // CHECK:STDERR:
  // CHECK:STDERR: fail_variety.carbon:[[@LINE+4]]:40: error: parentheses are required around this unary `not` operator [UnaryOperatorRequiresParentheses]
  // CHECK:STDERR:   n = a * b + c * d = d * d << e & f - not g;
  // CHECK:STDERR:                                        ^~~
  // CHECK:STDERR:
  n = a * b + c * d = d * d << e & f - not g;
}

// CHECK:STDOUT: - filename: fail_variety.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'F'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:             {kind: 'IdentifierNameExpr', text: 'n'},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'a'},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'b'},
// CHECK:STDOUT:               {kind: 'InfixOperatorStar', text: '*', subtree_size: 3},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'c'},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'd'},
// CHECK:STDOUT:               {kind: 'InfixOperatorStar', text: '*', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'InfixOperatorPlus', text: '+', subtree_size: 7},
// CHECK:STDOUT:           {kind: 'InfixOperatorEqual', text: '=', subtree_size: 9},
// CHECK:STDOUT:                   {kind: 'IdentifierNameExpr', text: 'd'},
// CHECK:STDOUT:                   {kind: 'IdentifierNameExpr', text: 'd'},
// CHECK:STDOUT:                 {kind: 'InfixOperatorStar', text: '*', subtree_size: 3},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'e'},
// CHECK:STDOUT:               {kind: 'InfixOperatorLessLess', text: '<<', has_error: yes, subtree_size: 5},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'f'},
// CHECK:STDOUT:             {kind: 'InfixOperatorAmp', text: '&', has_error: yes, subtree_size: 7},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'g'},
// CHECK:STDOUT:             {kind: 'PrefixOperatorNot', text: 'not', subtree_size: 2},
// CHECK:STDOUT:           {kind: 'InfixOperatorMinus', text: '-', has_error: yes, subtree_size: 10},
// CHECK:STDOUT:         {kind: 'InfixOperatorEqual', text: '=', has_error: yes, subtree_size: 20},
// CHECK:STDOUT:       {kind: 'ExprStatement', text: ';', subtree_size: 21},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 27},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
