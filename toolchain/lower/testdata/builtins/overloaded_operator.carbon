// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/full.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/lower/testdata/builtins/overloaded_operator.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/lower/testdata/builtins/overloaded_operator.carbon

fn AddThreeIntegers(a: i32, b: i32, c: i32) -> i32 {
  return a + b + c;
}

// CHECK:STDOUT: ; ModuleID = 'overloaded_operator.carbon'
// CHECK:STDOUT: source_filename = "overloaded_operator.carbon"
// CHECK:STDOUT:
// CHECK:STDOUT: define i32 @_CAddThreeIntegers.Main(i32 %a, i32 %b, i32 %c) !dbg !4 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Int.as.AddWith.impl.Op.call.loc14_12 = add i32 %a, %b, !dbg !7
// CHECK:STDOUT:   %Int.as.AddWith.impl.Op.call.loc14_16 = add i32 %Int.as.AddWith.impl.Op.call.loc14_12, %c, !dbg !7
// CHECK:STDOUT:   ret i32 %Int.as.AddWith.impl.Op.call.loc14_16, !dbg !8
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: !llvm.module.flags = !{!0, !1}
// CHECK:STDOUT: !llvm.dbg.cu = !{!2}
// CHECK:STDOUT:
// CHECK:STDOUT: !0 = !{i32 7, !"Dwarf Version", i32 5}
// CHECK:STDOUT: !1 = !{i32 2, !"Debug Info Version", i32 3}
// CHECK:STDOUT: !2 = distinct !DICompileUnit(language: DW_LANG_C, file: !3, producer: "carbon", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
// CHECK:STDOUT: !3 = !DIFile(filename: "overloaded_operator.carbon", directory: "")
// CHECK:STDOUT: !4 = distinct !DISubprogram(name: "AddThreeIntegers", linkageName: "_CAddThreeIntegers.Main", scope: null, file: !3, line: 13, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !5 = !DISubroutineType(types: !6)
// CHECK:STDOUT: !6 = !{}
// CHECK:STDOUT: !7 = !DILocation(line: 14, column: 10, scope: !4)
// CHECK:STDOUT: !8 = !DILocation(line: 14, column: 3, scope: !4)
