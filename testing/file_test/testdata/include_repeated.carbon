// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: testing/file_test/testdata/include_files/no_split.carbon
// INCLUDE-FILE: testing/file_test/testdata/include_files/no_split.carbon
// INCLUDE-FILE: testing/file_test/testdata/include_files/no_split.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //testing/file_test:file_test_base_test --test_arg=--file_tests=testing/file_test/testdata/include_repeated.carbon
// TIP: To dump output, run:
// TIP:   bazel run //testing/file_test:file_test_base_test -- --dump_output --file_tests=testing/file_test/testdata/include_repeated.carbon

// CHECK:STDOUT: 3 args: `default_args`, `include_repeated.carbon`, `include_files/no_split.carbon`
// CHECK:STDOUT: include_files/no_split.carbon:5: no split
