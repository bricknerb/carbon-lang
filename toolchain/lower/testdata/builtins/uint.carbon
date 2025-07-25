// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/primitives.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/lower/testdata/builtins/uint.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/lower/testdata/builtins/uint.carbon

fn Negate(a: u64) -> u64 = "int.unegate";
fn TestNegate(a: u64) -> u64 { return Negate(a); }

fn Add(a: u64, b: u64) -> u64 = "int.uadd";
fn TestAdd(a: u64, b: u64) -> u64 { return Add(a, b); }

fn Sub(a: u64, b: u64) -> u64 = "int.usub";
fn TestSub(a: u64, b: u64) -> u64 { return Sub(a, b); }

fn Mul(a: u64, b: u64) -> u64 = "int.umul";
fn TestMul(a: u64, b: u64) -> u64 { return Mul(a, b); }

fn Div(a: u64, b: u64) -> u64 = "int.udiv";
fn TestDiv(a: u64, b: u64) -> u64 { return Div(a, b); }

fn Mod(a: u64, b: u64) -> u64 = "int.umod";
fn TestMod(a: u64, b: u64) -> u64 { return Mod(a, b); }

fn Complement(a: u64) -> u64 = "int.complement";
fn TestComplement(a: u64) -> u64 { return Complement(a); }

fn And(a: u64, b: u64) -> u64 = "int.and";
fn TestAnd(a: u64, b: u64) -> u64 { return And(a, b); }

fn Or(a: u64, b: u64) -> u64 = "int.or";
fn TestOr(a: u64, b: u64) -> u64 { return Or(a, b); }

fn Xor(a: u64, b: u64) -> u64 = "int.xor";
fn TestXor(a: u64, b: u64) -> u64 { return Xor(a, b); }

fn LeftShift(a: u64, b: u64) -> u64 = "int.left_shift";
fn TestLeftShift(a: u64, b: u64) -> u64 { return LeftShift(a, b); }

fn RightShift(a: u64, b: u64) -> u64 = "int.right_shift";
fn TestRightShift(a: u64, b: u64) -> u64 { return RightShift(a, b); }

fn Eq(a: u64, b: u64) -> bool = "int.eq";
fn TestEq(a: u64, b: u64) -> bool { return Eq(a, b); }

fn Neq(a: u64, b: u64) -> bool = "int.neq";
fn TestNeq(a: u64, b: u64) -> bool { return Neq(a, b); }

fn Less(a: u64, b: u64) -> bool = "int.less";
fn TestLess(a: u64, b: u64) -> bool { return Less(a, b); }

fn LessEq(a: u64, b: u64) -> bool = "int.less_eq";
fn TestLessEq(a: u64, b: u64) -> bool { return LessEq(a, b); }

fn Greater(a: u64, b: u64) -> bool = "int.greater";
fn TestGreater(a: u64, b: u64) -> bool { return Greater(a, b); }

fn GreaterEq(a: u64, b: u64) -> bool = "int.greater_eq";
fn TestGreaterEq(a: u64, b: u64) -> bool { return GreaterEq(a, b); }

// CHECK:STDOUT: ; ModuleID = 'uint.carbon'
// CHECK:STDOUT: source_filename = "uint.carbon"
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestNegate.Main(i64 %a) !dbg !4 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Negate.call = sub i64 0, %a, !dbg !7
// CHECK:STDOUT:   ret i64 %Negate.call, !dbg !8
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestAdd.Main(i64 %a, i64 %b) !dbg !9 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Add.call = add i64 %a, %b, !dbg !10
// CHECK:STDOUT:   ret i64 %Add.call, !dbg !11
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestSub.Main(i64 %a, i64 %b) !dbg !12 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Sub.call = sub i64 %a, %b, !dbg !13
// CHECK:STDOUT:   ret i64 %Sub.call, !dbg !14
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestMul.Main(i64 %a, i64 %b) !dbg !15 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Mul.call = mul i64 %a, %b, !dbg !16
// CHECK:STDOUT:   ret i64 %Mul.call, !dbg !17
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestDiv.Main(i64 %a, i64 %b) !dbg !18 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Div.call = udiv i64 %a, %b, !dbg !19
// CHECK:STDOUT:   ret i64 %Div.call, !dbg !20
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestMod.Main(i64 %a, i64 %b) !dbg !21 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Mod.call = urem i64 %a, %b, !dbg !22
// CHECK:STDOUT:   ret i64 %Mod.call, !dbg !23
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestComplement.Main(i64 %a) !dbg !24 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Complement.call = xor i64 -1, %a, !dbg !25
// CHECK:STDOUT:   ret i64 %Complement.call, !dbg !26
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestAnd.Main(i64 %a, i64 %b) !dbg !27 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %And.call = and i64 %a, %b, !dbg !28
// CHECK:STDOUT:   ret i64 %And.call, !dbg !29
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestOr.Main(i64 %a, i64 %b) !dbg !30 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Or.call = or i64 %a, %b, !dbg !31
// CHECK:STDOUT:   ret i64 %Or.call, !dbg !32
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestXor.Main(i64 %a, i64 %b) !dbg !33 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Xor.call = xor i64 %a, %b, !dbg !34
// CHECK:STDOUT:   ret i64 %Xor.call, !dbg !35
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestLeftShift.Main(i64 %a, i64 %b) !dbg !36 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %LeftShift.call = shl i64 %a, %b, !dbg !37
// CHECK:STDOUT:   ret i64 %LeftShift.call, !dbg !38
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i64 @_CTestRightShift.Main(i64 %a, i64 %b) !dbg !39 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %RightShift.call = lshr i64 %a, %b, !dbg !40
// CHECK:STDOUT:   ret i64 %RightShift.call, !dbg !41
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i1 @_CTestEq.Main(i64 %a, i64 %b) !dbg !42 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Eq.call = icmp eq i64 %a, %b, !dbg !43
// CHECK:STDOUT:   ret i1 %Eq.call, !dbg !44
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i1 @_CTestNeq.Main(i64 %a, i64 %b) !dbg !45 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Neq.call = icmp ne i64 %a, %b, !dbg !46
// CHECK:STDOUT:   ret i1 %Neq.call, !dbg !47
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i1 @_CTestLess.Main(i64 %a, i64 %b) !dbg !48 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Less.call = icmp ult i64 %a, %b, !dbg !49
// CHECK:STDOUT:   ret i1 %Less.call, !dbg !50
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i1 @_CTestLessEq.Main(i64 %a, i64 %b) !dbg !51 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %LessEq.call = icmp ule i64 %a, %b, !dbg !52
// CHECK:STDOUT:   ret i1 %LessEq.call, !dbg !53
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i1 @_CTestGreater.Main(i64 %a, i64 %b) !dbg !54 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %Greater.call = icmp ugt i64 %a, %b, !dbg !55
// CHECK:STDOUT:   ret i1 %Greater.call, !dbg !56
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: define i1 @_CTestGreaterEq.Main(i64 %a, i64 %b) !dbg !57 {
// CHECK:STDOUT: entry:
// CHECK:STDOUT:   %GreaterEq.call = icmp uge i64 %a, %b, !dbg !58
// CHECK:STDOUT:   ret i1 %GreaterEq.call, !dbg !59
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: !llvm.module.flags = !{!0, !1}
// CHECK:STDOUT: !llvm.dbg.cu = !{!2}
// CHECK:STDOUT:
// CHECK:STDOUT: !0 = !{i32 7, !"Dwarf Version", i32 5}
// CHECK:STDOUT: !1 = !{i32 2, !"Debug Info Version", i32 3}
// CHECK:STDOUT: !2 = distinct !DICompileUnit(language: DW_LANG_C, file: !3, producer: "carbon", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
// CHECK:STDOUT: !3 = !DIFile(filename: "uint.carbon", directory: "")
// CHECK:STDOUT: !4 = distinct !DISubprogram(name: "TestNegate", linkageName: "_CTestNegate.Main", scope: null, file: !3, line: 14, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !5 = !DISubroutineType(types: !6)
// CHECK:STDOUT: !6 = !{}
// CHECK:STDOUT: !7 = !DILocation(line: 14, column: 39, scope: !4)
// CHECK:STDOUT: !8 = !DILocation(line: 14, column: 32, scope: !4)
// CHECK:STDOUT: !9 = distinct !DISubprogram(name: "TestAdd", linkageName: "_CTestAdd.Main", scope: null, file: !3, line: 17, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !10 = !DILocation(line: 17, column: 44, scope: !9)
// CHECK:STDOUT: !11 = !DILocation(line: 17, column: 37, scope: !9)
// CHECK:STDOUT: !12 = distinct !DISubprogram(name: "TestSub", linkageName: "_CTestSub.Main", scope: null, file: !3, line: 20, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !13 = !DILocation(line: 20, column: 44, scope: !12)
// CHECK:STDOUT: !14 = !DILocation(line: 20, column: 37, scope: !12)
// CHECK:STDOUT: !15 = distinct !DISubprogram(name: "TestMul", linkageName: "_CTestMul.Main", scope: null, file: !3, line: 23, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !16 = !DILocation(line: 23, column: 44, scope: !15)
// CHECK:STDOUT: !17 = !DILocation(line: 23, column: 37, scope: !15)
// CHECK:STDOUT: !18 = distinct !DISubprogram(name: "TestDiv", linkageName: "_CTestDiv.Main", scope: null, file: !3, line: 26, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !19 = !DILocation(line: 26, column: 44, scope: !18)
// CHECK:STDOUT: !20 = !DILocation(line: 26, column: 37, scope: !18)
// CHECK:STDOUT: !21 = distinct !DISubprogram(name: "TestMod", linkageName: "_CTestMod.Main", scope: null, file: !3, line: 29, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !22 = !DILocation(line: 29, column: 44, scope: !21)
// CHECK:STDOUT: !23 = !DILocation(line: 29, column: 37, scope: !21)
// CHECK:STDOUT: !24 = distinct !DISubprogram(name: "TestComplement", linkageName: "_CTestComplement.Main", scope: null, file: !3, line: 32, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !25 = !DILocation(line: 32, column: 43, scope: !24)
// CHECK:STDOUT: !26 = !DILocation(line: 32, column: 36, scope: !24)
// CHECK:STDOUT: !27 = distinct !DISubprogram(name: "TestAnd", linkageName: "_CTestAnd.Main", scope: null, file: !3, line: 35, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !28 = !DILocation(line: 35, column: 44, scope: !27)
// CHECK:STDOUT: !29 = !DILocation(line: 35, column: 37, scope: !27)
// CHECK:STDOUT: !30 = distinct !DISubprogram(name: "TestOr", linkageName: "_CTestOr.Main", scope: null, file: !3, line: 38, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !31 = !DILocation(line: 38, column: 43, scope: !30)
// CHECK:STDOUT: !32 = !DILocation(line: 38, column: 36, scope: !30)
// CHECK:STDOUT: !33 = distinct !DISubprogram(name: "TestXor", linkageName: "_CTestXor.Main", scope: null, file: !3, line: 41, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !34 = !DILocation(line: 41, column: 44, scope: !33)
// CHECK:STDOUT: !35 = !DILocation(line: 41, column: 37, scope: !33)
// CHECK:STDOUT: !36 = distinct !DISubprogram(name: "TestLeftShift", linkageName: "_CTestLeftShift.Main", scope: null, file: !3, line: 44, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !37 = !DILocation(line: 44, column: 50, scope: !36)
// CHECK:STDOUT: !38 = !DILocation(line: 44, column: 43, scope: !36)
// CHECK:STDOUT: !39 = distinct !DISubprogram(name: "TestRightShift", linkageName: "_CTestRightShift.Main", scope: null, file: !3, line: 47, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !40 = !DILocation(line: 47, column: 51, scope: !39)
// CHECK:STDOUT: !41 = !DILocation(line: 47, column: 44, scope: !39)
// CHECK:STDOUT: !42 = distinct !DISubprogram(name: "TestEq", linkageName: "_CTestEq.Main", scope: null, file: !3, line: 50, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !43 = !DILocation(line: 50, column: 44, scope: !42)
// CHECK:STDOUT: !44 = !DILocation(line: 50, column: 37, scope: !42)
// CHECK:STDOUT: !45 = distinct !DISubprogram(name: "TestNeq", linkageName: "_CTestNeq.Main", scope: null, file: !3, line: 53, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !46 = !DILocation(line: 53, column: 45, scope: !45)
// CHECK:STDOUT: !47 = !DILocation(line: 53, column: 38, scope: !45)
// CHECK:STDOUT: !48 = distinct !DISubprogram(name: "TestLess", linkageName: "_CTestLess.Main", scope: null, file: !3, line: 56, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !49 = !DILocation(line: 56, column: 46, scope: !48)
// CHECK:STDOUT: !50 = !DILocation(line: 56, column: 39, scope: !48)
// CHECK:STDOUT: !51 = distinct !DISubprogram(name: "TestLessEq", linkageName: "_CTestLessEq.Main", scope: null, file: !3, line: 59, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !52 = !DILocation(line: 59, column: 48, scope: !51)
// CHECK:STDOUT: !53 = !DILocation(line: 59, column: 41, scope: !51)
// CHECK:STDOUT: !54 = distinct !DISubprogram(name: "TestGreater", linkageName: "_CTestGreater.Main", scope: null, file: !3, line: 62, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !55 = !DILocation(line: 62, column: 49, scope: !54)
// CHECK:STDOUT: !56 = !DILocation(line: 62, column: 42, scope: !54)
// CHECK:STDOUT: !57 = distinct !DISubprogram(name: "TestGreaterEq", linkageName: "_CTestGreaterEq.Main", scope: null, file: !3, line: 65, type: !5, spFlags: DISPFlagDefinition, unit: !2)
// CHECK:STDOUT: !58 = !DILocation(line: 65, column: 51, scope: !57)
// CHECK:STDOUT: !59 = !DILocation(line: 65, column: 44, scope: !57)
