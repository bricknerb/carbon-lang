// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/int.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/lower/testdata/let/copy_value_rep.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/lower/testdata/let/copy_value_rep.carbon

class X {
  var a: i32;
}

fn Run() -> i32 {
  var x: X = {.a = 1};
  let n: i32 = x.a;
  x.a = 2;
  // Should return 1, not 2.
  return n;
}

// CHECK:STDOUT: ; ModuleID = 'copy_value_rep.carbon'
// CHECK:STDOUT: source_filename = "copy_value_rep.carbon"
// CHECK:STDOUT:
// CHECK:STDOUT: @X.val.loc18_3 = internal constant { i32 } { i32 1 }
// CHECK:STDOUT:
// CHECK:STDOUT: define i32 @main() !dbg !4 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %x.var = alloca { i32 }, align 8, !dbg !7
// CHECK:STDOUT:   call void @llvm.lifetime.start.p0(i64 4, ptr %x.var), !dbg !7
// CHECK:STDOUT:   %.loc18_21.3.a = getelementptr inbounds nuw { i32 }, ptr %x.var, i32 0, i32 0, !dbg !8
// CHECK:STDOUT:   call void @llvm.memcpy.p0.p0.i64(ptr align 4 %x.var, ptr align 4 @X.val.loc18_3, i64 4, i1 false), !dbg !7
// CHECK:STDOUT:   %.loc19_17.1.a = getelementptr inbounds nuw { i32 }, ptr %x.var, i32 0, i32 0, !dbg !9
// CHECK:STDOUT:   %.loc19_17.2 = load i32, ptr %.loc19_17.1.a, align 4, !dbg !9
// CHECK:STDOUT:   %.loc20_4.a = getelementptr inbounds nuw { i32 }, ptr %x.var, i32 0, i32 0, !dbg !10
// CHECK:STDOUT:   store i32 2, ptr %.loc20_4.a, align 4, !dbg !10
// CHECK:STDOUT:   ret i32 %.loc19_17.2, !dbg !11
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
// CHECK:STDOUT: declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #0
// CHECK:STDOUT:
// CHECK:STDOUT: ; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
// CHECK:STDOUT: declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #1
// CHECK:STDOUT:
// CHECK:STDOUT: attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
// CHECK:STDOUT: attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
// CHECK:STDOUT:
// CHECK:STDOUT: !llvm.module.flags = !{!0, !1}
// CHECK:STDOUT: !llvm.dbg.cu = !{!2}
// CHECK:STDOUT:
// CHECK:STDOUT: !0 = !{i32 7, !"Dwarf Version", i32 5}
// CHECK:STDOUT: !1 = !{i32 2, !"Debug Info Version", i32 3}
// CHECK:STDOUT: !2 = distinct !DICompileUnit(language: DW_LANG_C, file: !3, producer: "carbon", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
// CHECK:STDOUT: !3 = !DIFile(filename: "copy_value_rep.carbon", directory: "")
// CHECK:STDOUT: !4 = distinct !DISubprogram(name: "Run", linkageName: "main", scope: null, file: !3, line: 17, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !5 = !DISubroutineType(types: !6)
// CHECK:STDOUT: !6 = !{}
// CHECK:STDOUT: !7 = !DILocation(line: 18, column: 3, scope: !4)
// CHECK:STDOUT: !8 = !DILocation(line: 18, column: 14, scope: !4)
// CHECK:STDOUT: !9 = !DILocation(line: 19, column: 16, scope: !4)
// CHECK:STDOUT: !10 = !DILocation(line: 20, column: 3, scope: !4)
// CHECK:STDOUT: !11 = !DILocation(line: 22, column: 3, scope: !4)
