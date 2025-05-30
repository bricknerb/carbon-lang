// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

package Core library "prelude/types/int";

import library "prelude/types/int_literal";
import library "prelude/operators";

private fn MakeInt(size: IntLiteral()) -> type = "int.make_type_signed";

class Int(N:! IntLiteral()) {
  adapt MakeInt(N);
}

// Conversions.

impl forall [To:! IntLiteral()] IntLiteral() as ImplicitAs(Int(To)) {
  fn Convert[self: Self]() -> Int(To) = "int.convert_checked";
}

impl forall [From:! IntLiteral()] Int(From) as ImplicitAs(IntLiteral()) {
  fn Convert[self: Self]() -> IntLiteral() = "int.convert_checked";
}

// TODO: Remove these once ImplicitAs extends As.
impl forall [To:! IntLiteral()] IntLiteral() as As(Int(To)) {
  fn Convert[self: Self]() -> Int(To) = "int.convert_checked";
}

impl forall [From:! IntLiteral()] Int(From) as As(IntLiteral()) {
  fn Convert[self: Self]() -> IntLiteral() = "int.convert_checked";
}

// TODO: Allow as an implicit conversion if N > M.
impl forall [From:! IntLiteral(), To:! IntLiteral()] Int(From) as As(Int(To)) {
  fn Convert[self: Self]() -> Int(To) = "int.convert";
}

// Comparisons.

impl forall [N:! IntLiteral()] Int(N) as Eq {
  fn Equal[self: Self](other: Self) -> bool = "int.eq";
  fn NotEqual[self: Self](other: Self) -> bool = "int.neq";
}

impl forall [N:! IntLiteral()] Int(N) as Ordered {
  // TODO: fn Compare
  fn Less[self: Self](other: Self) -> bool = "int.less";
  fn LessOrEquivalent[self: Self](other: Self) -> bool = "int.less_eq";
  fn Greater[self: Self](other: Self) -> bool = "int.greater";
  fn GreaterOrEquivalent[self: Self](other: Self) -> bool = "int.greater_eq";
}

// Arithmetic.

impl forall [N:! IntLiteral()] Int(N) as Add {
  fn Op[self: Self](other: Self) -> Self = "int.sadd";
}

impl forall [N:! IntLiteral()] Int(N) as Div {
  fn Op[self: Self](other: Self) -> Self = "int.sdiv";
}

impl forall [N:! IntLiteral()] Int(N) as Mod {
  fn Op[self: Self](other: Self) -> Self = "int.smod";
}

impl forall [N:! IntLiteral()] Int(N) as Mul {
  fn Op[self: Self](other: Self) -> Self = "int.smul";
}

impl forall [N:! IntLiteral()] Int(N) as Negate {
  fn Op[self: Self]() -> Self = "int.snegate";
}

impl forall [N:! IntLiteral()] Int(N) as Sub {
  fn Op[self: Self](other: Self) -> Self = "int.ssub";
}

// Bitwise operators.

impl forall [N:! IntLiteral()] Int(N) as BitAnd {
  fn Op[self: Self](other: Self) -> Self = "int.and";
}

impl forall [N:! IntLiteral()] Int(N) as BitComplement {
  fn Op[self: Self]() -> Self = "int.complement";
}

impl forall [N:! IntLiteral()] Int(N) as BitOr {
  fn Op[self: Self](other: Self) -> Self = "int.or";
}

impl forall [N:! IntLiteral()] Int(N) as BitXor {
  fn Op[self: Self](other: Self) -> Self = "int.xor";
}

impl forall [N:! IntLiteral()] Int(N) as LeftShift {
  fn Op[self: Self](other: Self) -> Self = "int.left_shift";
}

impl forall [N:! IntLiteral()] Int(N) as RightShift {
  fn Op[self: Self](other: Self) -> Self = "int.right_shift";
}

// Compound assignments.

impl forall [N:! IntLiteral()] Int(N) as AddAssign {
  fn Op[addr self: Self*](other: Self) = "int.sadd_assign";
}

impl forall [N:! IntLiteral()] Int(N) as BitAndAssign {
  fn Op[addr self: Self*](other: Self) = "int.and_assign";
}

impl forall [N:! IntLiteral()] Int(N) as BitOrAssign {
  fn Op[addr self: Self*](other: Self) = "int.or_assign";
}

impl forall [N:! IntLiteral()] Int(N) as BitXorAssign {
  fn Op[addr self: Self*](other: Self) = "int.xor_assign";
}

impl forall [N:! IntLiteral()] Int(N) as DivAssign {
  fn Op[addr self: Self*](other: Self) = "int.sdiv_assign";
}

impl forall [N:! IntLiteral()] Int(N) as LeftShiftAssign {
  fn Op[addr self: Self*](other: Self) = "int.left_shift_assign";
}

impl forall [N:! IntLiteral()] Int(N) as ModAssign {
  fn Op[addr self: Self*](other: Self) = "int.smod_assign";
}

impl forall [N:! IntLiteral()] Int(N) as MulAssign {
  fn Op[addr self: Self*](other: Self) = "int.smul_assign";
}

impl forall [N:! IntLiteral()] Int(N) as RightShiftAssign {
  fn Op[addr self: Self*](other: Self) = "int.right_shift_assign";
}

impl forall [N:! IntLiteral()] Int(N) as SubAssign {
  fn Op[addr self: Self*](other: Self) = "int.ssub_assign";
}

// Increment and decrement.

// impl forall [N:! IntLiteral()] Int(N) as Dec {
impl i32 as Dec {
  fn Op[addr self: Self*]() {
    *self -= 1;
  }
}

// impl forall [N:! IntLiteral()] Int(N) as Inc {
impl i32 as Inc {
  fn Op[addr self: Self*]() {
    *self += 1;
  }
}
