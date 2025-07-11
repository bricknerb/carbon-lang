// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/facet_types.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/impl/lookup/impl_cycle.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/impl/lookup/impl_cycle.carbon

// --- fail_impl_simple_cycle.carbon
library "[[@TEST_NAME]]";

interface Z {}

// This creates a dependency cycle with itself.
impl forall [T:! Z] T as Z {}

class Point {}

fn F() {
  // TODO: We should just get one diagnostic here, either the conversion fails
  // with a note about the cycle as the reason for impl lookup failing, or the
  // cycle causes an error so the conversion is not diagnosed.

  // CHECK:STDERR: fail_impl_simple_cycle.carbon:[[@LINE+11]]:3: error: cycle found in search for impl of `Z` for type `Point` [ImplLookupCycle]
  // CHECK:STDERR:   Point as Z;
  // CHECK:STDERR:   ^~~~~~~~~~
  // CHECK:STDERR: fail_impl_simple_cycle.carbon:[[@LINE-12]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [T:! Z] T as Z {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_simple_cycle.carbon:[[@LINE+4]]:3: error: cannot convert type `Point` into type implementing `Z` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   Point as Z;
  // CHECK:STDERR:   ^~~~~~~~~~
  // CHECK:STDERR:
  Point as Z;
}

// --- fail_impl_simple_where_cycle.carbon
library "[[@TEST_NAME]]";

interface Y {}
interface Z {}

// This creates a dependency cycle with itself.
impl forall [T:! type where .Self impls Z] T as Z {}

class Point {}

fn F() {
  // CHECK:STDERR: fail_impl_simple_where_cycle.carbon:[[@LINE+11]]:3: error: cycle found in search for impl of `type where .Self impls Z` for type `Point` [ImplLookupCycle]
  // CHECK:STDERR:   Point as Z;
  // CHECK:STDERR:   ^~~~~~~~~~
  // CHECK:STDERR: fail_impl_simple_where_cycle.carbon:[[@LINE-8]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [T:! type where .Self impls Z] T as Z {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_simple_where_cycle.carbon:[[@LINE+4]]:3: error: cannot convert type `Point` into type implementing `Z` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   Point as Z;
  // CHECK:STDERR:   ^~~~~~~~~~
  // CHECK:STDERR:
  Point as Z;
}

// --- fail_impl_simple_two_interfaces.carbon
library "[[@TEST_NAME]]";

interface Z {}
interface Y {}

// This creates a dependency cycle with itself.
impl forall [T:! Z & Y] T as Z {}

class Point {}

fn F() {
  // CHECK:STDERR: fail_impl_simple_two_interfaces.carbon:[[@LINE+11]]:3: error: cycle found in search for impl of `Z & Y` for type `Point` [ImplLookupCycle]
  // CHECK:STDERR:   Point as Z;
  // CHECK:STDERR:   ^~~~~~~~~~
  // CHECK:STDERR: fail_impl_simple_two_interfaces.carbon:[[@LINE-8]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [T:! Z & Y] T as Z {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_simple_two_interfaces.carbon:[[@LINE+4]]:3: error: cannot convert type `Point` into type implementing `Z` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   Point as Z;
  // CHECK:STDERR:   ^~~~~~~~~~
  // CHECK:STDERR:
  Point as Z;
}

// --- fail_impl_long_cycle.carbon
library "[[@TEST_NAME]]";

interface X {}
interface Y {}
interface Z {}

// This creates a dependency cycle with itself.
impl forall [T:! X] T as Y {}
impl forall [T:! Y] T as Z {}
impl forall [T:! Z] T as X {}

class C {}

fn F() {
  // CHECK:STDERR: fail_impl_long_cycle.carbon:[[@LINE+17]]:3: error: cycle found in search for impl of `Z` for type `C` [ImplLookupCycle]
  // CHECK:STDERR:   C as Z;
  // CHECK:STDERR:   ^~~~~~
  // CHECK:STDERR: fail_impl_long_cycle.carbon:[[@LINE-9]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [T:! Y] T as Z {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_long_cycle.carbon:[[@LINE-13]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [T:! X] T as Y {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_long_cycle.carbon:[[@LINE-14]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [T:! Z] T as X {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_long_cycle.carbon:[[@LINE+4]]:3: error: cannot convert type `C` into type implementing `Z` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   C as Z;
  // CHECK:STDERR:   ^~~~~~
  // CHECK:STDERR:
  C as Z;
}

// --- fail_impl_cycle_one_generic_param.carbon
library "[[@TEST_NAME]]";

interface ComparableWith(T:! type) {}

// This creates a dependency cycle with itself.
impl forall [U:! type, T:! ComparableWith(U)]
    U as ComparableWith(T) {}

class C {}
class D {}

fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}

fn F() {
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+17]]:3: error: cycle found in search for impl of `ComparableWith(C)` for type `C` [ImplLookupCycle]
  // CHECK:STDERR:   Compare({} as C, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-12]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-9]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+7]]:3: error: cannot convert type `C` into type implementing `ComparableWith(C)` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   Compare({} as C, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-16]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Compare({} as C, {} as C);

  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+20]]:3: error: cycle found in search for impl of `ComparableWith(C)` for type `D` [ImplLookupCycle]
  // CHECK:STDERR:   Compare({} as C, {} as D);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-31]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-34]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-31]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+7]]:3: error: cannot convert type `D` into type implementing `ComparableWith(C)` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   Compare({} as C, {} as D);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-38]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Compare({} as C, {} as D);

  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+20]]:3: error: cycle found in search for impl of `ComparableWith(D)` for type `C` [ImplLookupCycle]
  // CHECK:STDERR:   Compare({} as D, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-53]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-56]]:1: note: determining if this impl clause matches [ImplLookupCycleNote]
  // CHECK:STDERR: impl forall [U:! type, T:! ComparableWith(U)]
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-53]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE+7]]:3: error: cannot convert type `C` into type implementing `ComparableWith(D)` [ConversionFailureTypeToFacet]
  // CHECK:STDERR:   Compare({} as D, {} as C);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_impl_cycle_one_generic_param.carbon:[[@LINE-60]]:1: note: while deducing parameters of generic declared here [DeductionGenericHere]
  // CHECK:STDERR: fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}
  // CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Compare({} as D, {} as C);
}

// --- impl_recurse_with_simpler_type_no_cycle.carbon
library "[[@TEST_NAME]]";

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

// TODO: These three `impl`s need to be placed in a match_first as they
// overlap.

// T is always ComparableWith(T).
impl forall [T:! type] T as ComparableWith(T) {}
// If U is ComparableTo(T), U is ComparableWith(T).
// CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE+7]]:1: error: found non-final `impl` with the same type structure as another non-final `impl` [ImplNonFinalSameTypeStructure]
// CHECK:STDERR: impl forall [T:! type, U:! ComparableTo(T)] U as ComparableWith(T) {}
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE-5]]:1: note: other `impl` here [ImplNonFinalSameTypeStructureNote]
// CHECK:STDERR: impl forall [T:! type] T as ComparableWith(T) {}
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR:
impl forall [T:! type, U:! ComparableTo(T)] U as ComparableWith(T) {}
// If U is ComparableTo(T), T is ComparableWith(U).
// CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE+7]]:1: error: found non-final `impl` with the same type structure as another non-final `impl` [ImplNonFinalSameTypeStructure]
// CHECK:STDERR: impl forall [T:! type, U:! ComparableTo(T)] T as ComparableWith(U) {}
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR: fail_todo_impl_recurse_with_simpler_type_in_generic_param_bidirectional_no_cycle.carbon:[[@LINE-14]]:1: note: other `impl` here [ImplNonFinalSameTypeStructureNote]
// CHECK:STDERR: impl forall [T:! type] T as ComparableWith(T) {}
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK:STDERR:
impl forall [T:! type, U:! ComparableTo(T)] T as ComparableWith(U) {}

fn Compare[T:! type, U:! ComparableWith(T)](t: T, u: U) {}

fn F() {
  Compare({} as C, {} as C);
  Compare({} as C, {} as D);
  Compare({} as D, {} as C);
}

// --- deduce_cycle_without_symbolic.carbon
library "[[@TEST_NAME]]";

interface First {}
interface Second {}

// When answering a query "does T impl Second", we must avoid trying to deduce
// parameters for this impl. Not only is doing so unnecessary, it would start a new
// "does T impl Second" query, leading to a "cycle in impl lookup" error.
impl forall [T:! Second] T as First {}
impl forall [T:! type] T as Second {}

class C {}

fn F() {
  ({} as C) as (C as Second);
  ({} as C) as (C as First);
}

// --- deduce_cycle_with_symbolic.carbon
library "[[@TEST_NAME]]";

interface First(T:! type) {}
interface Second {}

// When answering a query "does T impl Second", we must avoid trying to deduce
// parameters for this impl. Not only is doing so unnecessary, it would start a new
// "does T impl Second" query, leading to a "cycle in impl lookup" error.
impl forall [T:! Second, U:! type] T as First(U) {}
impl forall [T:! type] T as Second {}

class C {}

fn F() {
  ({} as C) as (C as Second);
  ({} as C) as (C as First(C));
}
