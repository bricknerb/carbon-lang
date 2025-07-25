// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/uint.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/builtins/int/convert.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/builtins/int/convert.carbon

// --- int_ops.carbon

library "[[@TEST_NAME]]";

// Size preserving
fn Int32ToInt32(a: i32) -> i32 = "int.convert";
fn Int32ToUint32(a: i32) -> u32 = "int.convert";
fn Uint32ToInt32(a: u32) -> i32 = "int.convert";
fn Uint32ToUint32(a: u32) -> u32 = "int.convert";
fn IntLiteralToIntLiteral(a: Core.IntLiteral()) -> Core.IntLiteral() = "int.convert";

// Narrowing
fn Int32ToInt16(a: i32) -> i16 = "int.convert";
fn Int32ToUint16(a: i32) -> u16 = "int.convert";
fn Uint32ToInt16(a: u32) -> i16 = "int.convert";
fn Uint32ToUint16(a: u32) -> u16 = "int.convert";
fn IntLiteralToInt16(a: Core.IntLiteral()) -> i16 = "int.convert";
fn IntLiteralToUint16(a: Core.IntLiteral()) -> u16 = "int.convert";

// Widening
fn Int32ToInt64(a: i32) -> i64 = "int.convert";
fn Int32ToUint64(a: i32) -> u64 = "int.convert";
fn Uint32ToInt64(a: u32) -> i64 = "int.convert";
fn Uint32ToUint64(a: u32) -> u64 = "int.convert";
fn Int32ToIntLiteral(a: i32) -> Core.IntLiteral() = "int.convert";
fn Uint32ToIntLiteral(a: u32) -> Core.IntLiteral() = "int.convert";

class Expect[T:! type](N:! T) {}
fn Test[T:! type](N:! T) -> Expect(N) { return {}; }

// --- runtime_call.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn SizePreserving(a: i32) -> u32 {
  //@dump-sem-ir-begin
  return Int32ToUint32(a);
  //@dump-sem-ir-end
}

fn Narrowing(a: i32) -> i16 {
  //@dump-sem-ir-begin
  return Int32ToInt16(a);
  //@dump-sem-ir-end
}

fn Widening(a: i32) -> i64 {
  //@dump-sem-ir-begin
  return Int32ToInt64(a);
  //@dump-sem-ir-end
}

// --- fail_self_test.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn F() {
  // Ensure our testing machinery works.
  // CHECK:STDERR: fail_self_test.carbon:[[@LINE+7]]:3: error: cannot convert expression of type `Expect(0)` to `Expect(1)` with `as` [ConversionFailure]
  // CHECK:STDERR:   Test(Int32ToInt32(0)) as Expect(1 as i32);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_self_test.carbon:[[@LINE+4]]:3: note: type `Expect(0)` does not implement interface `Core.As(Expect(1))` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   Test(Int32ToInt32(0)) as Expect(1 as i32);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Test(Int32ToInt32(0)) as Expect(1 as i32);
}

// --- identity.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn F() {
  Test(Int32ToInt32(-0x8000_0000)) as Expect(-0x8000_0000 as i32);
  Test(Int32ToInt32(-1)) as Expect(-1 as i32);
  Test(Int32ToInt32(0)) as Expect(0 as i32);
  Test(Int32ToInt32(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as i32);

  Test(Uint32ToUint32(0)) as Expect(0 as u32);
  Test(Uint32ToUint32(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as u32);
  Test(Uint32ToUint32(0x8000_0000)) as Expect(0x8000_0000 as u32);
  Test(Uint32ToUint32(0xFFFF_FFFF)) as Expect(0xFFFF_FFFF as u32);

  Test(IntLiteralToIntLiteral(0x1_0000_0000_0000_0000)) as
      Expect(0x1_0000_0000_0000_0000);
  Test(IntLiteralToIntLiteral(-1)) as Expect(-1);
}

// --- same_size.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn F() {
  Test(Int32ToUint32(-0x8000_0000)) as Expect(0x8000_0000 as u32);
  Test(Int32ToUint32(-1)) as Expect(0xFFFF_FFFF as u32);
  Test(Int32ToUint32(0)) as Expect(0 as u32);
  Test(Int32ToUint32(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as u32);

  Test(Uint32ToInt32(0)) as Expect(0 as i32);
  Test(Uint32ToInt32(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as i32);
  Test(Uint32ToInt32(0x8000_0000)) as Expect(-0x8000_0000 as i32);
  Test(Uint32ToInt32(0xFFFF_FFFF)) as Expect(-1 as i32);
}

// --- truncate.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn F() {
  Test(Int32ToInt16(-0x8000_0000)) as Expect(0 as i16);
  Test(Int32ToInt16(-0x7FFF_EDCC)) as Expect(0x1234 as i16);
  Test(Int32ToInt16(-0x7FFF_1234)) as Expect(-0x1234 as i16);
  Test(Int32ToInt16(-0x8000)) as Expect(-0x8000 as i16);
  Test(Int32ToInt16(-1)) as Expect(-1 as i16);
  Test(Int32ToInt16(0)) as Expect(0 as i16);
  Test(Int32ToInt16(0x7FFF)) as Expect(0x7FFF as i16);
  Test(Int32ToInt16(0xFFFF)) as Expect(-1 as i16);
  Test(Int32ToInt16(0x7FFF_1234)) as Expect(0x1234 as i16);
  Test(Int32ToInt16(0x7FFF_EDCC)) as Expect(-0x1234 as i16);
  Test(Int32ToInt16(0x7FFF_FFFF)) as Expect(-1 as i16);

  Test(Int32ToUint16(-0x8000_0000)) as Expect(0 as u16);
  Test(Int32ToUint16(-0x7FFF_EDCC)) as Expect(0x1234 as u16);
  Test(Int32ToUint16(-0x7FFF_1234)) as Expect(0xEDCC as u16);
  Test(Int32ToUint16(-0x8000)) as Expect(0x8000 as u16);
  Test(Int32ToUint16(-1)) as Expect(0xFFFF as u16);
  Test(Int32ToUint16(0)) as Expect(0 as u16);
  Test(Int32ToUint16(0x7FFF)) as Expect(0x7FFF as u16);
  Test(Int32ToUint16(0xFFFF)) as Expect(0xFFFF as u16);
  Test(Int32ToUint16(0x7FFF_1234)) as Expect(0x1234 as u16);
  Test(Int32ToUint16(0x7FFF_EDCC)) as Expect(0xEDCC as u16);
  Test(Int32ToUint16(0x7FFF_FFFF)) as Expect(0xFFFF as u16);

  Test(Uint32ToInt16(0x8000_0000)) as Expect(0 as i16);
  Test(Uint32ToInt16(0xFFFF_1234)) as Expect(0x1234 as i16);
  Test(Uint32ToInt16(0xFFFF_EDCC)) as Expect(-0x1234 as i16);
  Test(Uint32ToInt16(0xFFFF_8000)) as Expect(-0x8000 as i16);
  Test(Uint32ToInt16(0xFFFF_FFFF)) as Expect(-1 as i16);
  Test(Uint32ToInt16(0)) as Expect(0 as i16);
  Test(Uint32ToInt16(0x7FFF)) as Expect(0x7FFF as i16);
  Test(Uint32ToInt16(0xFFFF)) as Expect(-1 as i16);
  Test(Uint32ToInt16(0x7FFF_1234)) as Expect(0x1234 as i16);
  Test(Uint32ToInt16(0x7FFF_EDCC)) as Expect(-0x1234 as i16);
  Test(Uint32ToInt16(0x7FFF_FFFF)) as Expect(-1 as i16);

  Test(Uint32ToUint16(0x8000_0000)) as Expect(0 as u16);
  Test(Uint32ToUint16(0xFFFF_1234)) as Expect(0x1234 as u16);
  Test(Uint32ToUint16(0xFFFF_EDCC)) as Expect(0xEDCC as u16);
  Test(Uint32ToUint16(0xFFFF_8000)) as Expect(0x8000 as u16);
  Test(Uint32ToUint16(0xFFFF_FFFF)) as Expect(0xFFFF as u16);
  Test(Uint32ToUint16(0)) as Expect(0 as u16);
  Test(Uint32ToUint16(0x7FFF)) as Expect(0x7FFF as u16);
  Test(Uint32ToUint16(0xFFFF)) as Expect(0xFFFF as u16);
  Test(Uint32ToUint16(0x7FFF_1234)) as Expect(0x1234 as u16);
  Test(Uint32ToUint16(0x7FFF_EDCC)) as Expect(0xEDCC as u16);
  Test(Uint32ToUint16(0x7FFF_FFFF)) as Expect(0xFFFF as u16);

  Test(IntLiteralToInt16(0)) as Expect(0 as i16);
  Test(IntLiteralToInt16(0x7FFF)) as Expect(0x7FFF as i16);
  Test(IntLiteralToInt16(0x8000)) as Expect(-0x8000 as i16);
  Test(IntLiteralToInt16(0xFFFF)) as Expect(-1 as i16);
  Test(IntLiteralToInt16(0x1_2345)) as Expect(0x2345 as i16);
  Test(IntLiteralToInt16(-1)) as Expect(-1 as i16);

  Test(IntLiteralToUint16(0)) as Expect(0 as u16);
  Test(IntLiteralToUint16(0x7FFF)) as Expect(0x7FFF as u16);
  Test(IntLiteralToUint16(0x8000)) as Expect(0x8000 as u16);
  Test(IntLiteralToUint16(0xFFFF)) as Expect(0xFFFF as u16);
  Test(IntLiteralToUint16(0x1_2345)) as Expect(0x2345 as u16);
  Test(IntLiteralToUint16(-1)) as Expect(0xFFFF as u16);
}

// --- zero_extend.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn F() {
  Test(Uint32ToInt64(0)) as Expect(0 as i64);
  Test(Uint32ToInt64(0x1234_5678)) as Expect(0x1234_5678 as i64);
  Test(Uint32ToInt64(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as i64);
  Test(Uint32ToInt64(0x8000_0000)) as Expect(0x8000_0000 as i64);
  Test(Uint32ToInt64(0xFFFF_FFFF)) as Expect(0xFFFF_FFFF as i64);

  Test(Uint32ToUint64(0)) as Expect(0 as u64);
  Test(Uint32ToUint64(0x1234_5678)) as Expect(0x1234_5678 as u64);
  Test(Uint32ToUint64(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as u64);
  Test(Uint32ToUint64(0x8000_0000)) as Expect(0x8000_0000 as u64);
  Test(Uint32ToUint64(0xFFFF_FFFF)) as Expect(0xFFFF_FFFF as u64);

  Test(Uint32ToIntLiteral(0x1234_5678)) as Expect(0x1234_5678);
  Test(Uint32ToIntLiteral(0x8765_4321)) as Expect(0x8765_4321);
  Test(Uint32ToIntLiteral(0xFFFF_FFFF)) as Expect(0xFFFF_FFFF);
}

// --- sign_extend.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

fn F() {
  Test(Int32ToInt64(0)) as Expect(0 as i64);
  Test(Int32ToInt64(0x1234_5678)) as Expect(0x1234_5678 as i64);
  Test(Int32ToInt64(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as i64);
  Test(Int32ToInt64(-1)) as Expect(-1 as i64);

  Test(Int32ToUint64(0)) as Expect(0 as u64);
  Test(Int32ToUint64(0x1234_5678)) as Expect(0x1234_5678 as u64);
  Test(Int32ToUint64(0x7FFF_FFFF)) as Expect(0x7FFF_FFFF as u64);
  Test(Int32ToUint64(-1)) as Expect(0xFFFF_FFFF_FFFF_FFFF as u64);
  Test(Int32ToUint64(-0x8000_0000)) as Expect(0xFFFF_FFFF_8000_0000 as u64);

  Test(Int32ToIntLiteral(0x1234_5678)) as Expect(0x1234_5678);
  Test(Int32ToIntLiteral(-0x1234_5678)) as Expect(-0x1234_5678);
  Test(Int32ToIntLiteral(-1)) as Expect(-1);
}

// --- fail_not_constant.carbon

library "[[@TEST_NAME]]";
import library "int_ops";

let not_constant: Core.IntLiteral() = 0;

// CHECK:STDERR: fail_not_constant.carbon:[[@LINE+8]]:33: error: non-constant call to compile-time-only function [NonConstantCallToCompTimeOnlyFunction]
// CHECK:STDERR: let convert_not_constant: i16 = IntLiteralToInt16(not_constant);
// CHECK:STDERR:                                 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR: fail_not_constant.carbon:[[@LINE-7]]:1: in import [InImport]
// CHECK:STDERR: int_ops.carbon:16:1: note: compile-time-only function declared here [CompTimeOnlyFunctionHere]
// CHECK:STDERR: fn IntLiteralToInt16(a: Core.IntLiteral()) -> i16 = "int.convert";
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR:
let convert_not_constant: i16 = IntLiteralToInt16(not_constant);

// CHECK:STDOUT: --- runtime_call.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %int_32: Core.IntLiteral = int_value 32 [concrete]
// CHECK:STDOUT:   %i32: type = class_type @Int, @Int(%int_32) [concrete]
// CHECK:STDOUT:   %u32: type = class_type @UInt, @UInt(%int_32) [concrete]
// CHECK:STDOUT:   %Int32ToUint32.type: type = fn_type @Int32ToUint32 [concrete]
// CHECK:STDOUT:   %Int32ToUint32: %Int32ToUint32.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_16: Core.IntLiteral = int_value 16 [concrete]
// CHECK:STDOUT:   %i16: type = class_type @Int, @Int(%int_16) [concrete]
// CHECK:STDOUT:   %Int32ToInt16.type: type = fn_type @Int32ToInt16 [concrete]
// CHECK:STDOUT:   %Int32ToInt16: %Int32ToInt16.type = struct_value () [concrete]
// CHECK:STDOUT:   %int_64: Core.IntLiteral = int_value 64 [concrete]
// CHECK:STDOUT:   %i64: type = class_type @Int, @Int(%int_64) [concrete]
// CHECK:STDOUT:   %Int32ToInt64.type: type = fn_type @Int32ToInt64 [concrete]
// CHECK:STDOUT:   %Int32ToInt64: %Int32ToInt64.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.Int32ToUint32: %Int32ToUint32.type = import_ref Main//int_ops, Int32ToUint32, loaded [concrete = constants.%Int32ToUint32]
// CHECK:STDOUT:   %Main.Int32ToInt16: %Int32ToInt16.type = import_ref Main//int_ops, Int32ToInt16, loaded [concrete = constants.%Int32ToInt16]
// CHECK:STDOUT:   %Main.Int32ToInt64: %Int32ToInt64.type = import_ref Main//int_ops, Int32ToInt64, loaded [concrete = constants.%Int32ToInt64]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @SizePreserving(%a.param: %i32) -> %u32 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Int32ToUint32.ref: %Int32ToUint32.type = name_ref Int32ToUint32, imports.%Main.Int32ToUint32 [concrete = constants.%Int32ToUint32]
// CHECK:STDOUT:   %a.ref: %i32 = name_ref a, %a
// CHECK:STDOUT:   %Int32ToUint32.call: init %u32 = call %Int32ToUint32.ref(%a.ref)
// CHECK:STDOUT:   %.loc7_26.1: %u32 = value_of_initializer %Int32ToUint32.call
// CHECK:STDOUT:   %.loc7_26.2: %u32 = converted %Int32ToUint32.call, %.loc7_26.1
// CHECK:STDOUT:   return %.loc7_26.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Narrowing(%a.param: %i32) -> %i16 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Int32ToInt16.ref: %Int32ToInt16.type = name_ref Int32ToInt16, imports.%Main.Int32ToInt16 [concrete = constants.%Int32ToInt16]
// CHECK:STDOUT:   %a.ref: %i32 = name_ref a, %a
// CHECK:STDOUT:   %Int32ToInt16.call: init %i16 = call %Int32ToInt16.ref(%a.ref)
// CHECK:STDOUT:   %.loc13_25.1: %i16 = value_of_initializer %Int32ToInt16.call
// CHECK:STDOUT:   %.loc13_25.2: %i16 = converted %Int32ToInt16.call, %.loc13_25.1
// CHECK:STDOUT:   return %.loc13_25.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Widening(%a.param: %i32) -> %i64 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %Int32ToInt64.ref: %Int32ToInt64.type = name_ref Int32ToInt64, imports.%Main.Int32ToInt64 [concrete = constants.%Int32ToInt64]
// CHECK:STDOUT:   %a.ref: %i32 = name_ref a, %a
// CHECK:STDOUT:   %Int32ToInt64.call: init %i64 = call %Int32ToInt64.ref(%a.ref)
// CHECK:STDOUT:   %.loc19_25.1: %i64 = value_of_initializer %Int32ToInt64.call
// CHECK:STDOUT:   %.loc19_25.2: %i64 = converted %Int32ToInt64.call, %.loc19_25.1
// CHECK:STDOUT:   return %.loc19_25.2
// CHECK:STDOUT: }
// CHECK:STDOUT:
