// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/int.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/lower/testdata/function/call/params_one_comma.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/lower/testdata/function/call/params_one_comma.carbon

fn Foo(a: i32,) {}

fn Main() {
  Foo(1);
  Foo(1,);
}

// CHECK:STDOUT: ; ModuleID = 'params_one_comma.carbon'
// CHECK:STDOUT: source_filename = "params_one_comma.carbon"
// CHECK:STDOUT:
// CHECK:STDOUT: define void @_CFoo.Main(i32 %a) !dbg !4 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   ret void, !dbg !7
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define void @_CMain.Main() !dbg !8 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   call void @_CFoo.Main(i32 1), !dbg !9
// CHECK:STDOUT:   call void @_CFoo.Main(i32 1), !dbg !10
// CHECK:STDOUT:   ret void, !dbg !11
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: !llvm.module.flags = !{!0, !1}
// CHECK:STDOUT: !llvm.dbg.cu = !{!2}
// CHECK:STDOUT:
// CHECK:STDOUT: !0 = !{i32 7, !"Dwarf Version", i32 5}
// CHECK:STDOUT: !1 = !{i32 2, !"Debug Info Version", i32 3}
// CHECK:STDOUT: !2 = distinct !DICompileUnit(language: DW_LANG_C, file: !3, producer: "carbon", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
// CHECK:STDOUT: !3 = !DIFile(filename: "params_one_comma.carbon", directory: "")
// CHECK:STDOUT: !4 = distinct !DISubprogram(name: "Foo", linkageName: "_CFoo.Main", scope: null, file: !3, line: 13, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !5 = !DISubroutineType(types: !6)
// CHECK:STDOUT: !6 = !{}
// CHECK:STDOUT: !7 = !DILocation(line: 13, column: 1, scope: !4)
// CHECK:STDOUT: !8 = distinct !DISubprogram(name: "Main", linkageName: "_CMain.Main", scope: null, file: !3, line: 15, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !9 = !DILocation(line: 16, column: 3, scope: !8)
// CHECK:STDOUT: !10 = !DILocation(line: 17, column: 3, scope: !8)
// CHECK:STDOUT: !11 = !DILocation(line: 15, column: 1, scope: !8)
