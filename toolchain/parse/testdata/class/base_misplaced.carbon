// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/class/base_misplaced.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/class/base_misplaced.carbon

base class B {}

// These are rejected by check.
base: B;

fn F() {
  base: B;
}


// CHECK:STDOUT: - filename: base_misplaced.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'ClassIntroducer', text: 'class'},
// CHECK:STDOUT:         {kind: 'BaseModifier', text: 'base'},
// CHECK:STDOUT:         {kind: 'IdentifierNameNotBeforeParams', text: 'B'},
// CHECK:STDOUT:       {kind: 'ClassDefinitionStart', text: '{', subtree_size: 4},
// CHECK:STDOUT:     {kind: 'ClassDefinition', text: '}', subtree_size: 5},
// CHECK:STDOUT:       {kind: 'BaseIntroducer', text: 'base'},
// CHECK:STDOUT:       {kind: 'BaseColon', text: ':'},
// CHECK:STDOUT:       {kind: 'IdentifierNameExpr', text: 'B'},
// CHECK:STDOUT:     {kind: 'BaseDecl', text: ';', subtree_size: 4},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'F'},
// CHECK:STDOUT:           {kind: 'ExplicitParamListStart', text: '('},
// CHECK:STDOUT:         {kind: 'ExplicitParamList', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'FunctionDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:         {kind: 'BaseIntroducer', text: 'base'},
// CHECK:STDOUT:         {kind: 'BaseColon', text: ':'},
// CHECK:STDOUT:         {kind: 'IdentifierNameExpr', text: 'B'},
// CHECK:STDOUT:       {kind: 'BaseDecl', text: ';', subtree_size: 4},
// CHECK:STDOUT:     {kind: 'FunctionDefinition', text: '}', subtree_size: 10},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
