// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// EXTRA-ARGS: --dump-sem-ir-ranges=only
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interop/cpp/no_prelude/file_not_found.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interop/cpp/no_prelude/file_not_found.carbon
// CHECK:STDERR: fail_cpp_file_not_found.carbon.generated.cpp_imports.h:1: error: C++:
// CHECK:STDERR: fail_cpp_file_not_found.carbon.generated.cpp_imports.h:1:10: fatal error: 'not_found.h' file not found
// CHECK:STDERR:     1 | #include "not_found.h"
// CHECK:STDERR:       |          ^~~~~~~~~~~~~
// CHECK:STDERR:  [CppInteropParseError]

// --- fail_cpp_file_not_found.carbon

library "[[@TEST_NAME]]";

//@dump-sem-ir-begin
// CHECK:STDERR: fail_cpp_file_not_found.carbon:[[@LINE+4]]:1: note: in `Cpp` import [InCppImport]
// CHECK:STDERR: import Cpp library "not_found.h";
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR:
import Cpp library "not_found.h";
//@dump-sem-ir-end

// CHECK:STDOUT: --- fail_cpp_file_not_found.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   %Cpp.import_cpp = import_cpp {
// CHECK:STDOUT:     import Cpp "not_found.h"
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
