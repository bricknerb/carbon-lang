// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //testing/file_test:file_test_base_test --test_arg=--file_tests=testing/file_test/testdata/multiple_file_refs_in_line.carbon
// TIP: To dump output, run:
// TIP:   bazel run //testing/file_test:file_test_base_test -- --dump_output --file_tests=testing/file_test/testdata/multiple_file_refs_in_line.carbon
// CHECK:STDOUT: 3 args: `default_args`, `a.carbon`, `b.carbon`

// --- a.carbon
a.carbon:1: b.carbon:3: a.carbon:3: hello
// CHECK:STDOUT: a.carbon:[[@LINE-1]]: a.carbon:[[@LINE-1]]: b.carbon:3: a.carbon:[[@LINE+1]]: hello

// --- b.carbon


a.carbon:1: b.carbon:3: a.carbon:3: hello
// CHECK:STDOUT: b.carbon:[[@LINE-1]]: a.carbon:1: b.carbon:[[@LINE-1]]: a.carbon:3: hello
a.carbon:1: b.carbon:5: a.carbon:3: hello
// CHECK:STDOUT: b.carbon:[[@LINE-1]]: a.carbon:1: b.carbon:[[@LINE-1]]: a.carbon:3: hello
a.carbon:1: b.carbon:7: a.carbon:3: hello
// CHECK:STDOUT: b.carbon:[[@LINE-1]]: a.carbon:1: b.carbon:[[@LINE-1]]: a.carbon:3: hello
