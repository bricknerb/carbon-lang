// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/match/match.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/match/match.carbon

fn f() -> i32 {
  var x: i32 = 3;
  match (f(x)) {
    case (a: i32, b: i32) => { return 0; }
    case (a: i32) if (a < 0) => { return 2; }
    case a: i32 if (a != x) => { return 3; }
    default => { return 4; }
  }
  return 0;
}

// CHECK:STDOUT: - filename: match.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'f'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:           {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:         {kind: 'ReturnType', text: '->', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'VariableIntroducer', text: 'var'},
// CHECK:STDOUT:             {kind: 'IdentifierNameNotBeforeParams', text: 'x'},
// CHECK:STDOUT:             {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:           {kind: 'VarBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'VariablePattern', text: 'var', subtree_size: 4},
// CHECK:STDOUT:         {kind: 'VariableInitializer', text: '='},
// CHECK:STDOUT:         {kind: 'IntLiteral', text: '3'},
// CHECK:STDOUT:       {kind: 'VariableDecl', text: ';', subtree_size: 8},
// CHECK:STDOUT:           {kind: 'MatchIntroducer', text: 'match'},
// CHECK:STDOUT:             {kind: 'MatchConditionStart', text: '('},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'f'},
// CHECK:STDOUT:               {kind: 'CallExprStart', text: '(', subtree_size: 2},
// CHECK:STDOUT:               {kind: 'IdentifierNameExpr', text: 'x'},
// CHECK:STDOUT:             {kind: 'CallExpr', text: ')', subtree_size: 4},
// CHECK:STDOUT:           {kind: 'MatchCondition', text: ')', subtree_size: 6},
// CHECK:STDOUT:         {kind: 'MatchStatementStart', text: '{', subtree_size: 8},
// CHECK:STDOUT:             {kind: 'MatchCaseIntroducer', text: 'case'},
// CHECK:STDOUT:               {kind: 'TuplePatternStart', text: '('},
// CHECK:STDOUT:                 {kind: 'IdentifierNameNotBeforeParams', text: 'a'},
// CHECK:STDOUT:                 {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:               {kind: 'LetBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:               {kind: 'PatternListComma', text: ','},
// CHECK:STDOUT:                 {kind: 'IdentifierNameNotBeforeParams', text: 'b'},
// CHECK:STDOUT:                 {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:               {kind: 'LetBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'TuplePattern', text: ')', subtree_size: 9},
// CHECK:STDOUT:             {kind: 'MatchCaseEqualGreater', text: '=>'},
// CHECK:STDOUT:           {kind: 'MatchCaseStart', text: '{', subtree_size: 12},
// CHECK:STDOUT:             {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'IntLiteral', text: '0'},
// CHECK:STDOUT:           {kind: 'ReturnStatement', text: ';', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'MatchCase', text: '}', subtree_size: 16},
// CHECK:STDOUT:             {kind: 'MatchCaseIntroducer', text: 'case'},
// CHECK:STDOUT:               {kind: 'TuplePatternStart', text: '('},
// CHECK:STDOUT:                 {kind: 'IdentifierNameNotBeforeParams', text: 'a'},
// CHECK:STDOUT:                 {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:               {kind: 'LetBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'TuplePattern', text: ')', subtree_size: 5},
// CHECK:STDOUT:               {kind: 'MatchCaseGuardIntroducer', text: 'if'},
// CHECK:STDOUT:               {kind: 'MatchCaseGuardStart', text: '('},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'a'},
// CHECK:STDOUT:                 {kind: 'IntLiteral', text: '0'},
// CHECK:STDOUT:               {kind: 'InfixOperatorLess', text: '<', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'MatchCaseGuard', text: ')', subtree_size: 6},
// CHECK:STDOUT:             {kind: 'MatchCaseEqualGreater', text: '=>'},
// CHECK:STDOUT:           {kind: 'MatchCaseStart', text: '{', subtree_size: 14},
// CHECK:STDOUT:             {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'IntLiteral', text: '2'},
// CHECK:STDOUT:           {kind: 'ReturnStatement', text: ';', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'MatchCase', text: '}', subtree_size: 18},
// CHECK:STDOUT:             {kind: 'MatchCaseIntroducer', text: 'case'},
// CHECK:STDOUT:               {kind: 'IdentifierNameNotBeforeParams', text: 'a'},
// CHECK:STDOUT:               {kind: 'IntTypeLiteral', text: 'i32'},
// CHECK:STDOUT:             {kind: 'LetBindingPattern', text: ':', subtree_size: 3},
// CHECK:STDOUT:               {kind: 'MatchCaseGuardIntroducer', text: 'if'},
// CHECK:STDOUT:               {kind: 'MatchCaseGuardStart', text: '('},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'a'},
// CHECK:STDOUT:                 {kind: 'IdentifierNameExpr', text: 'x'},
// CHECK:STDOUT:               {kind: 'InfixOperatorExclaimEqual', text: '!=', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'MatchCaseGuard', text: ')', subtree_size: 6},
// CHECK:STDOUT:             {kind: 'MatchCaseEqualGreater', text: '=>'},
// CHECK:STDOUT:           {kind: 'MatchCaseStart', text: '{', subtree_size: 12},
// CHECK:STDOUT:             {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'IntLiteral', text: '3'},
// CHECK:STDOUT:           {kind: 'ReturnStatement', text: ';', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'MatchCase', text: '}', subtree_size: 16},
// CHECK:STDOUT:             {kind: 'MatchDefaultIntroducer', text: 'default'},
// CHECK:STDOUT:             {kind: 'MatchDefaultEqualGreater', text: '=>'},
// CHECK:STDOUT:           {kind: 'MatchDefaultStart', text: '{', subtree_size: 3},
// CHECK:STDOUT:             {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:             {kind: 'IntLiteral', text: '4'},
// CHECK:STDOUT:           {kind: 'ReturnStatement', text: ';', subtree_size: 3},
// CHECK:STDOUT:         {kind: 'MatchDefault', text: '}', subtree_size: 7},
// CHECK:STDOUT:       {kind: 'MatchStatement', text: '}', subtree_size: 66},
// CHECK:STDOUT:         {kind: 'ReturnStatementStart', text: 'return'},
// CHECK:STDOUT:         {kind: 'IntLiteral', text: '0'},
// CHECK:STDOUT:       {kind: 'ReturnStatement', text: ';', subtree_size: 3},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 85},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
