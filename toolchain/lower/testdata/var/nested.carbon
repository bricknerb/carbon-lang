// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/int.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/lower/testdata/var/nested.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/lower/testdata/var/nested.carbon

fn Run() -> i32 {
  var a: i32 = 1;
  while (true) {
    var b: i32 = a;
    a = b;
  }
  return a;
}

// CHECK:STDOUT: ; ModuleID = 'nested.carbon'
// CHECK:STDOUT: source_filename = "nested.carbon"
// CHECK:STDOUT:
// CHECK:STDOUT: define i32 @main() !dbg !4 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %a.var = alloca i32, align 4, !dbg !7
// CHECK:STDOUT:   %b.var = alloca i32, align 4, !dbg !8
// CHECK:STDOUT:   call void @llvm.lifetime.start.p0(i64 4, ptr %a.var), !dbg !7
// CHECK:STDOUT:   store i32 1, ptr %a.var, align 4, !dbg !7
// CHECK:STDOUT:   br label %while.cond, !dbg !9
// CHECK:STDOUT:
// CHECK:STDOUT: while.cond:                                       ; preds = %while.body, %entry
// CHECK:STDOUT:   br i1 true, label %while.body, label %while.done, !dbg !9
// CHECK:STDOUT:
// CHECK:STDOUT: while.body:                                       ; preds = %while.cond
// CHECK:STDOUT:   call void @llvm.lifetime.start.p0(i64 4, ptr %b.var), !dbg !8
// CHECK:STDOUT:   %.loc16_18 = load i32, ptr %a.var, align 4, !dbg !10
// CHECK:STDOUT:   store i32 %.loc16_18, ptr %b.var, align 4, !dbg !8
// CHECK:STDOUT:   %.loc17 = load i32, ptr %b.var, align 4, !dbg !11
// CHECK:STDOUT:   store i32 %.loc17, ptr %a.var, align 4, !dbg !12
// CHECK:STDOUT:   br label %while.cond, !dbg !13
// CHECK:STDOUT:
// CHECK:STDOUT: while.done:                                       ; preds = %while.cond
// CHECK:STDOUT:   %.loc19 = load i32, ptr %a.var, align 4, !dbg !14
// CHECK:STDOUT:   ret i32 %.loc19, !dbg !15
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
// CHECK:STDOUT: declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #0
// CHECK:STDOUT:
// CHECK:STDOUT: ; uselistorder directives
// CHECK:STDOUT: uselistorder ptr @llvm.lifetime.start.p0, { 1, 0 }
// CHECK:STDOUT:
// CHECK:STDOUT: attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
// CHECK:STDOUT:
// CHECK:STDOUT: !llvm.module.flags = !{!0, !1}
// CHECK:STDOUT: !llvm.dbg.cu = !{!2}
// CHECK:STDOUT:
// CHECK:STDOUT: !0 = !{i32 7, !"Dwarf Version", i32 5}
// CHECK:STDOUT: !1 = !{i32 2, !"Debug Info Version", i32 3}
// CHECK:STDOUT: !2 = distinct !DICompileUnit(language: DW_LANG_C, file: !3, producer: "carbon", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
// CHECK:STDOUT: !3 = !DIFile(filename: "nested.carbon", directory: "")
// CHECK:STDOUT: !4 = distinct !DISubprogram(name: "Run", linkageName: "main", scope: null, file: !3, line: 13, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !5 = !DISubroutineType(types: !6)
// CHECK:STDOUT: !6 = !{}
// CHECK:STDOUT: !7 = !DILocation(line: 14, column: 3, scope: !4)
// CHECK:STDOUT: !8 = !DILocation(line: 16, column: 5, scope: !4)
// CHECK:STDOUT: !9 = !DILocation(line: 15, column: 9, scope: !4)
// CHECK:STDOUT: !10 = !DILocation(line: 16, column: 18, scope: !4)
// CHECK:STDOUT: !11 = !DILocation(line: 17, column: 9, scope: !4)
// CHECK:STDOUT: !12 = !DILocation(line: 17, column: 5, scope: !4)
// CHECK:STDOUT: !13 = !DILocation(line: 15, column: 3, scope: !4)
// CHECK:STDOUT: !14 = !DILocation(line: 19, column: 10, scope: !4)
// CHECK:STDOUT: !15 = !DILocation(line: 19, column: 3, scope: !4)
