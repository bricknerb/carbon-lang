// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/function/definition/builtin.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/function/definition/builtin.carbon

fn F() = "builtin";

impl T as I {
  fn Op() = "builtin";
}

// CHECK:STDOUT: - filename: builtin.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:         {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:         {kind: 'IdentifierNameBeforeParams', text: 'F'},
// CHECK:STDOUT:           {kind: 'TuplePatternStart', text: '('},
// CHECK:STDOUT:         {kind: 'TuplePattern', text: ')', subtree_size: 2},
// CHECK:STDOUT:       {kind: 'BuiltinFunctionDefinitionStart', text: '=', subtree_size: 5},
// CHECK:STDOUT:       {kind: 'BuiltinName', text: '"builtin"'},
// CHECK:STDOUT:     {kind: 'BuiltinFunctionDefinition', text: ';', subtree_size: 7},
// CHECK:STDOUT:         {kind: 'ImplIntroducer', text: 'impl'},
// CHECK:STDOUT:           {kind: 'IdentifierNameExpr', text: 'T'},
// CHECK:STDOUT:         {kind: 'TypeImplAs', text: 'as', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'IdentifierNameExpr', text: 'I'},
// CHECK:STDOUT:       {kind: 'ImplDefinitionStart', text: '{', subtree_size: 5},
// CHECK:STDOUT:           {kind: 'FunctionIntroducer', text: 'fn'},
// CHECK:STDOUT:           {kind: 'IdentifierNameBeforeParams', text: 'Op'},
// CHECK:STDOUT:             {kind: 'TuplePatternStart', text: '('},
// CHECK:STDOUT:           {kind: 'TuplePattern', text: ')', subtree_size: 2},
// CHECK:STDOUT:         {kind: 'BuiltinFunctionDefinitionStart', text: '=', subtree_size: 5},
// CHECK:STDOUT:         {kind: 'BuiltinName', text: '"builtin"'},
// CHECK:STDOUT:       {kind: 'BuiltinFunctionDefinition', text: ';', subtree_size: 7},
// CHECK:STDOUT:     {kind: 'ImplDefinition', text: '}', subtree_size: 13},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
