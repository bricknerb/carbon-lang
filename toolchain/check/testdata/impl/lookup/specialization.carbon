// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/facet_types.carbon
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/impl/lookup/specialization.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/impl/lookup/specialization.carbon

// --- specialized_self_first.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C {}
impl C as Z(C) where .X = C {}

impl forall [T:! type] T as Z(T) where .X = () {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: C.(Z(C).X) = {} as C;
}

// --- specialized_self_second.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

impl forall [T:! type] T as Z(T) where .X = () {}

class C {}
impl C as Z(C) where .X = C {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: C.(Z(C).X) = {} as C;
}

// --- specialized_constraint_first.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C {}

impl C as Z(C) where .X = C {}
impl forall [T:! type] C as Z(T) where .X = () {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: C.(Z(C).X) = {} as C;
}

// --- specialized_constraint_second.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C {}

impl forall [T:! type] C as Z(T) where .X = () {}
impl C as Z(C) where .X = C {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: C.(Z(C).X) = {} as C;
}

// --- specialized_self_vs_constraint_self_first.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C(T:! type) {}

impl forall [T:! type] C(()) as Z(T) where .X = C(()) {}
impl forall [T:! type] C(T) as Z(C(())) where .X = () {}

fn F() {
  // The specialization of `C(())` should match in preference to the blanket impl
  // of `C(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: C(()).(Z(C(())).X) = {} as C(());
}

// --- specialized_self_vs_constraint_self_second.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C(T:! type) {}

impl forall [T:! type] C(T) as Z(C(())) where .X = () {}
impl forall [T:! type] C(()) as Z(T) where .X = C(()) {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: C(()).(Z(C(())).X) = {} as C(());
}

// --- generic_class_with_fully_specified_impl.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

impl forall [T:! type] T as Z(T) where .X = {.a: ()} {}

class C(T:! type) {}
impl C(()) as Z(()) where .X = C(()) {}

impl forall [T:! type] C(T) as Z(T) where .X = {.b: ()} {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket
  // impls of `Z(T)`. If a blanket impl is chosen, then `a` will have a struct
  // type which will fail to typecheck here when constructed from a `C` value.
  let a: C(()).(Z(()).X) = {} as C(());
}

// --- generic_class_with_blanket_impl_first.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C(T:! type) {}

impl forall [T:! type] C(T) as Z(T) where .X = C(()) {}

impl forall [T:! type] T as Z(T) where .X = () {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket
  // impls of `Z(T)`. If a blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here when constructed from a `C` value.
  let a: C(()).(Z(()).X) = {} as C(());
}

// --- generic_class_with_blanket_impl_second.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

impl forall [T:! type] T as Z(T) where .X = () {}

class C(T:! type) {}

impl forall [T:! type] C(T) as Z(T) where .X = C(()) {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket
  // impls of `Z(T)`. If a blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here when constructed from a `C` value.
  let a: C(()).(Z(()).X) = {} as C(());
}

// --- specialized_class_with_facet_value_param.carbon
library "[[@TEST_NAME]]";

interface Z {
  let X:! type;
}

class D(T:! type) {}
impl forall [T:! type] D(T) as Z where .X = D(()) {}

class E {}
impl E as Z where .X = () {}

class C(T:! Z, U:! Z) {}

// This places a FacetValue of type FacetType(Z) at the position of `D` in the
// self type, because the class C requires a facet value satisfying Z. It tests
// that we correctly determine that this FacetType is not symbolic, and look at
// the parametes of D for symbolic references.
impl forall [T:! Z] C(T, D(E)) as Z where .X = () {}
impl forall [T:! Z] C(D(T), T) as Z where .X = () {}
// This is the best match, `T` is in the last position compared to the others.
impl forall [T:! Z] C(D(E), T) as Z where .X = C(E, E) {}
impl forall [T:! Z] C(T, T) as Z where .X = () {}

fn F() {
  let a: C(D(E), D(E)).(Z.X) = {} as C(E, E);
}

// --- fail_specialized_class_with_symbolic_facet_value_param.carbon

interface Z {
  let X:! type;
}

impl forall [T:! type] T as Z where .X = T {}

interface Y {}
class C(T:! Y) {}

class D {}
impl D as Y {}

// D can be either a concrete or symbolic FacetValue, depending on what the
// caller has.
impl forall [D:! Y] C(D) as Z where .X = () {}

fn F[D:! Y](d: D) {
  // The FacetValue deduced for the param of `C` will be a symbolic FacetValue
  // because we are in a generic where `D` is an unknown type, which will cause
  // the query and impl self type to be C(FacetValue) for a symbolic FacetValue.
  //
  // CHECK:STDERR: fail_specialized_class_with_symbolic_facet_value_param.carbon:[[@LINE+7]]:23: error: cannot implicitly convert expression of type `()` to `C(D).(Z.X)` [ConversionFailure]
  // CHECK:STDERR:   let a: C(D).(Z.X) = ();
  // CHECK:STDERR:                       ^~
  // CHECK:STDERR: fail_specialized_class_with_symbolic_facet_value_param.carbon:[[@LINE+4]]:23: note: type `()` does not implement interface `Core.ImplicitAs(C(D).(Z.X))` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   let a: C(D).(Z.X) = ();
  // CHECK:STDERR:                       ^~
  // CHECK:STDERR:
  let a: C(D).(Z.X) = ();
}

// --- pointer_specialization_first.carbon
library "[[@TEST_NAME]]";

interface Z {
  let X:! type;
}

class C {}
impl C* as Z where .X = C {}

impl forall [T:! type] T* as Z where .X = () {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: (C*).(Z.X) = {} as C;
}

// --- pointer_specialization_second.carbon
library "[[@TEST_NAME]]";

interface Z {
  let X:! type;
}

impl forall [T:! type] T* as Z where .X = () {}

class C {}
impl C* as Z where .X = C {}

fn F() {
  // The specialization of `Z(C)` should match in preference to the blanket impl
  // of `Z(T)`. If the blanket impl is chosen, then `a` will have type `()`
  // which will fail to typecheck here.
  let a: (C*).(Z.X) = {} as C;
}

// --- cycle_in_deduce_avoided_by_specialization.carbon
library "[[@TEST_NAME]]";

interface Z {
  let X:! type;
}

class C(T:! type) {}

// This impl makes a cycle, but it's not considered at all for `C(())` since
// there is another impl with a better type structure, so no diagnostic is
// emitted.
impl forall [T:! Z] T as Z where .X = () {}
// Also a cycle, and also a worse match for `C(())`.
impl forall [T:! Z] C(T) as Z where .X = () {}

impl C(()) as Z where .X = C(()) {}

fn F() {
  let a: C(()).(Z.X) = {} as C(());
}

// --- final_specialization_before_generic_use_of_type_constant.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C {}

impl forall [T:! type, U:! type] T as Z(U) where .X = () {}

final impl forall [T:! type] T as Z(C) where .X = C {}

fn F[U:! type](T:! Z(C)) {
  // The value of `.X` can be known to be `C` here when the impl `T as Z(C)` is
  // final.
  let a: T.X = {} as C;
}

// --- fail_specialization_written_after_generic_use_of_type_constant.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

class C {}

impl forall [T:! type, U:! type] T as Z(U) where .X = () {}

fn F[U:! type](T:! Z(C)) {
  // The value of `.X` is symbolic, it can't be assigned a value of type `C`.
  // CHECK:STDERR: fail_specialization_written_after_generic_use_of_type_constant.carbon:[[@LINE+7]]:16: error: cannot implicitly convert expression of type `C` to `T.(Z(C).X)` [ConversionFailure]
  // CHECK:STDERR:   let a: T.X = {} as C;
  // CHECK:STDERR:                ^~~~~~~
  // CHECK:STDERR: fail_specialization_written_after_generic_use_of_type_constant.carbon:[[@LINE+4]]:16: note: type `C` does not implement interface `Core.ImplicitAs(T.(Z(C).X))` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   let a: T.X = {} as C;
  // CHECK:STDERR:                ^~~~~~~
  // CHECK:STDERR:
  let a: T.X = {} as C;
}

final impl forall [T:! type] T as Z(C) where .X = C {}

// --- specialization_written_after_generic_use.carbon
library "[[@TEST_NAME]]";

interface Z {
  let V:! type;
  fn ZZ() -> V;
}

interface Y {}
impl forall [T:! Y] T as Z where .V = () {
  fn ZZ() -> () { return (); }
}

fn H(W:! Z, X: W.(Z.V)) -> W.(Z.V) {
  return X;
}

fn G(U:! Y) -> U.(Z.V) {
  return H(U, U.(Z.ZZ)());
}

class C {
  impl as Y {}
}

impl C as Z where .V = {} {
  fn ZZ() -> {} { return {}; }
}

fn F() {
  let x: {} = G(C);
}

// --- specialization_written_after_generic_use_with_generic_interface.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let V:! type;
  fn ZZ() -> V;
}

interface Y {}
impl forall [T:! Y] T as Z(T) where .V = () {
  fn ZZ() -> () { return (); }
}

fn H(U:! Y, W:! Y & Z(U), X: W.(Z(U).V)) -> W.(Z(U).V) {
  return X;
}

fn G(U:! Y) -> U.(Z(U).V) {
  return H(U, U, U.(Z(U).ZZ)());
}

class C {
  impl as Y {}
}

impl C as Z(C) where .V = {} {
  fn ZZ() -> {} { return {}; }
}

fn F() {
  let x: {} = G(C);
}

// --- type_structure_first_difference.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
    let X:! type;
    fn MakeX() -> X;
}

class C {}

// Type structure: "?(?)"
impl forall [T:! type] T as Z(T) where .X = {.less_good: ()} {
    fn MakeX() -> {.less_good: ()} { return {.less_good = ()}; }
}

// Type structure: "?(c)". Should outrank the previous impl.
impl forall [T:! type] T as Z(C) where .X = () {
    fn MakeX() -> () { return (); }
}

fn F(T:! Z(C)) -> T.(Z(C).X) {
    return T.MakeX();
}

fn G() {
    // This won't typecheck if the first impl is selected.
    let a: () = F(C);
}

// --- extend_impl_as_specialization.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

impl forall [T:! type, S:! type] T as Z(S) where .X = {} {}

class C(S:! type) {
  extend impl as Z(S) where .X = () {}
  fn CC(a: Self.(Z(S).X)) -> Self.(Z(S).X) { return a; }
}

fn F() {
  let a: () = C(()).CC(());
  let b: C(()).X = a;
}

// --- final_impl_as_specialization.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

impl forall [T:! type, S:! type] T as Z(S) where .X = {} {}

class C(S:! type) {
  final impl as Z(S) where .X = () {}
  fn CC() -> Self.(Z(S).X) { return (); }
}

fn F() {
  let a: () = C(()).CC();
}

// --- final_extend_impl_as_specialization.carbon
library "[[@TEST_NAME]]";

interface Z(T:! type) {
  let X:! type;
}

impl forall [T:! type, S:! type] T as Z(S) where .X = {} {}

class C(S:! type) {
  extend final impl as Z(S) where .X = () {}
  fn CC() -> Self.(Z(S).X) { return (); }
}

fn F() {
  let a: () = C(()).CC();
  let b: C(()).X = a;
}
