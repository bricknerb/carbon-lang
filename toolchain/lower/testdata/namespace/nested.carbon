// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/convert.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/lower/testdata/namespace/nested.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/lower/testdata/namespace/nested.carbon

namespace Foo;
namespace Foo.Bar;

fn Foo.Bar.Wiz() {
}

fn Foo.Bar.Baz() {
  Foo.Bar.Wiz();
}

// CHECK:STDOUT: ; ModuleID = 'nested.carbon'
// CHECK:STDOUT: source_filename = "nested.carbon"
// CHECK:STDOUT:
// CHECK:STDOUT: define void @_CWiz.Bar.Foo.Main() !dbg !4 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   ret void, !dbg !7
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define void @_CBaz.Bar.Foo.Main() !dbg !8 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   call void @_CWiz.Bar.Foo.Main(), !dbg !9
// CHECK:STDOUT:   ret void, !dbg !10
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: !llvm.module.flags = !{!0, !1}
// CHECK:STDOUT: !llvm.dbg.cu = !{!2}
// CHECK:STDOUT:
// CHECK:STDOUT: !0 = !{i32 7, !"Dwarf Version", i32 5}
// CHECK:STDOUT: !1 = !{i32 2, !"Debug Info Version", i32 3}
// CHECK:STDOUT: !2 = distinct !DICompileUnit(language: DW_LANG_C, file: !3, producer: "carbon", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
// CHECK:STDOUT: !3 = !DIFile(filename: "nested.carbon", directory: "")
// CHECK:STDOUT: !4 = distinct !DISubprogram(name: "Wiz", linkageName: "_CWiz.Bar.Foo.Main", scope: null, file: !3, line: 16, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !5 = !DISubroutineType(types: !6)
// CHECK:STDOUT: !6 = !{}
// CHECK:STDOUT: !7 = !DILocation(line: 16, column: 1, scope: !4)
// CHECK:STDOUT: !8 = distinct !DISubprogram(name: "Baz", linkageName: "_CBaz.Bar.Foo.Main", scope: null, file: !3, line: 19, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !9 = !DILocation(line: 20, column: 3, scope: !8)
// CHECK:STDOUT: !10 = !DILocation(line: 19, column: 1, scope: !8)
