// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/parse/testdata/packages/import/semi_before.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/parse/testdata/packages/import/semi_before.carbon

// We allow an empty declaration in the initial sequence of imports.
;

import library "foo";

;

import Bar;

;

// CHECK:STDOUT: - filename: semi_before.carbon
// CHECK:STDOUT:   parse_tree: [
// CHECK:STDOUT:     {kind: 'FileStart', text: ''},
// CHECK:STDOUT:     {kind: 'EmptyDecl', text: ';'},
// CHECK:STDOUT:       {kind: 'ImportIntroducer', text: 'import'},
// CHECK:STDOUT:         {kind: 'LibraryName', text: '"foo"'},
// CHECK:STDOUT:       {kind: 'LibrarySpecifier', text: 'library', subtree_size: 2},
// CHECK:STDOUT:     {kind: 'ImportDecl', text: ';', subtree_size: 4},
// CHECK:STDOUT:     {kind: 'EmptyDecl', text: ';'},
// CHECK:STDOUT:       {kind: 'ImportIntroducer', text: 'import'},
// CHECK:STDOUT:       {kind: 'IdentifierPackageName', text: 'Bar'},
// CHECK:STDOUT:     {kind: 'ImportDecl', text: ';', subtree_size: 3},
// CHECK:STDOUT:     {kind: 'EmptyDecl', text: ';'},
// CHECK:STDOUT:     {kind: 'FileEnd', text: ''},
// CHECK:STDOUT:   ]
