// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// EXTRA-ARGS: --no-dump-sem-ir
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/impl/no_prelude/impl_cycle.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/impl/no_prelude/impl_cycle.carbon

// --- core.carbon
package Core;

interface As(Dest:! type) {
  fn Convert[self: Self]() -> Dest;
}

interface ImplicitAs(Dest:! type) {
  fn Convert[self: Self]() -> Dest;
}

// --- fail_impl_simple_cycle.carbon
library "[[@TEST_NAME]]";

interface Z {}

// This creates a dependency cycle with itself.
impl forall [T:! Z] T as Z {}

class Point {
  impl as Z {}
}

fn F() {
  // CHECK:STDERR: fail_impl_simple_cycle.carbon:[[@LINE+7]]:21: error: cycle found in lookup of interface <TODO: interface name> for type `Point` [ImplLookupCycle]
  // CHECK:STDERR:   ({} as Point) as (Point as Z);
  // CHECK:STDERR:                     ^~~~~~~~~~
  // CHECK:STDERR: fail_impl_simple_cycle.carbon:[[@LINE-10]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: impl forall [T:! Z] T as Z {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  ({} as Point) as (Point as Z);
}

// --- fail_impl_cycle_one_generic_param.carbon
library "[[@TEST_NAME]]";

import Core;

interface ComparableWith(T:! type) {}

// This creates a dependency cycle with itself.
impl forall [U:! type, T:! ComparableWith(U)]
    U as ComparableWith(T) {}

class C {}
class D {}
impl C as ComparableWith(D) {}

fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}

fn F() {
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+20]]:3: error: cycle found in lookup of interface <TODO: interface name> for type `C` [ImplLookupCycle]
  // CHECK:STDERR:   Compare({} as C, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-13]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-9]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+10]]:3: error: cannot implicitly convert from `type` to `ComparableWith(C)` [ImplicitAsConversionFailure]
  // CHECK:STDERR:   Compare({} as C, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+7]]:3: note: type `type` does not implement interface `Core.ImplicitAs(ComparableWith(C))` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   Compare({} as C, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-19]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Compare({} as C, {} as C);

  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+13]]:3: error: cycle found in lookup of interface <TODO: interface name> for type `D` [ImplLookupCycle]
  // CHECK:STDERR:   Compare({} as C, {} as D);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-35]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-38]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-34]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Compare({} as C, {} as D);
}

// --- impl_recurse_with_simpler_type_no_cycle.carbon
library "[[@TEST_NAME]]";

import Core;

class Wraps(T:! type) {}

interface Printable {}

// If T is printable, so is Wraps(T).
impl forall [T:! Printable] Wraps(T) as Printable {}

class C {}
impl C as Printable {}

fn F() {
  ({} as C) as (C as Printable);
  ({} as Wraps(C)) as (Wraps(C) as Printable);
}

// --- impl_recurse_with_simpler_type_in_generic_param_no_cycle.carbon
library "[[@TEST_NAME]]";

import Core;

class Wraps(T:! type) {}

interface ComparableTo(T:! type) {}

// If U is comparable to T, then U is comparable to Wraps(T).
impl forall [T:! type, U:! ComparableTo(T)] U as ComparableTo(Wraps(T)) {}
// If U is comparable to T then Wraps(U) is comparable to T.
impl forall [T:! type, U:! ComparableTo(T)] Wraps(U) as ComparableTo(T) {}

class C {}
class D {}
impl C as ComparableTo(D) {}

fn F() {
  ({} as C) as (C as ComparableTo(D));
  ({} as C) as (C as ComparableTo(Wraps(D)));
  ({} as Wraps(C)) as (Wraps(C) as ComparableTo(D));
  ({} as Wraps(C)) as (Wraps(C) as ComparableTo(Wraps(D)));
}

// --- fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon
library "[[@TEST_NAME]]";

import Core;

// Implement this for a type in one direction.
interface ComparableTo(T:! type) {}
// Use this as a bound with two types in any direction.
interface ComparableWith(T:! type) {}

class C {}
class D {}
// C is comparable to D. This should imply:
// - C is ComparableWith(C).
// - C is ComparableWith(D).
// - D is ComparableWith(C).
impl C as ComparableTo(D) {}

// T is always ComparableWith(T).
impl forall [T:! type] T as ComparableWith(T) {}
// If U is ComparableTo(T), U is ComparableWith(T).
//
// TODO: When we look for the impl of `D as ComparableTo(C)` here, we find that
// it's not implemented. We then go to the next impl which does match, but
// deduction generates an error here regardless.
impl forall [T:! type, U:! ComparableTo(T)] U as ComparableWith(T) {}
// If U is ComparableTo(T), T is ComparableWith(U).
impl forall [T:! type, U:! ComparableTo(T)] T as ComparableWith(U) {}

fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}

fn F() {
  Compare({} as C, {} as C);
  // CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE+13]]:3: error: cannot implicitly convert from `type` to `ComparableTo(C)` [ImplicitAsConversionFailure]
  // CHECK:STDERR:   Compare({} as C, {} as D);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE+10]]:3: note: type `type` does not implement interface `Core.ImplicitAs(ComparableTo(C))` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   Compare({} as C, {} as D);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE-14]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: impl forall [T:! type, U:! ComparableTo(T)] U as ComparableWith(T) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE-13]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Compare({} as C, {} as D);
  Compare({} as D, {} as C);
}
