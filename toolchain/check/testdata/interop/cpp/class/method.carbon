// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/convert.carbon
// EXTRA-ARGS: --dump-sem-ir-ranges=ignore
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interop/cpp/class/method.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interop/cpp/class/method.carbon

// --- object_param_qualifiers.h

struct HasQualifiers {
  void plain();
  void const_this() const;
  void volatile_this() volatile;

  void ref_this() &;
  void const_ref_this() const&;

  void ref_ref_this() &&;
  void const_ref_ref_this() const&&;
};

// --- use_object_param_qualifiers.carbon

library "[[@TEST_NAME]]";

import Cpp library "object_param_qualifiers.h";

fn F(v: Cpp.HasQualifiers, p: Cpp.HasQualifiers*) {
  //@dump-sem-ir-begin
  v.const_this();
  v.const_ref_this();

  p->plain();
  p->ref_this();
  p->const_this();
  p->const_ref_this();
  //@dump-sem-ir-end
}

// --- fail_bad_object_param_qualifiers_by_value.carbon

library "[[@TEST_NAME]]";

import Cpp library "object_param_qualifiers.h";

fn Value(v: Cpp.HasQualifiers) {
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+5]]:3: error: `addr self` method cannot be invoked on a value [AddrSelfIsNonRef]
  // CHECK:STDERR:   v.plain();
  // CHECK:STDERR:   ^
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon: note: initializing function parameter [InCallToFunctionParam]
  // CHECK:STDERR:
  v.plain();

  // TODO: This should remain invalid once we support `volatile`.
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+12]]:3: error: semantics TODO: `Unsupported: qualified type: volatile struct HasQualifiers` [SemanticsTodo]
  // CHECK:STDERR:   v.volatile_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+9]]:3: note: in `Cpp` name lookup for `volatile_this` [InCppNameLookup]
  // CHECK:STDERR:   v.volatile_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~
  // CHECK:STDERR:
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+5]]:3: error: `addr self` method cannot be invoked on a value [AddrSelfIsNonRef]
  // CHECK:STDERR:   v.volatile_this();
  // CHECK:STDERR:   ^
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon: note: initializing function parameter [InCallToFunctionParam]
  // CHECK:STDERR:
  v.volatile_this();

  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+5]]:3: error: `addr self` method cannot be invoked on a value [AddrSelfIsNonRef]
  // CHECK:STDERR:   v.ref_this();
  // CHECK:STDERR:   ^
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon: note: initializing function parameter [InCallToFunctionParam]
  // CHECK:STDERR:
  v.ref_this();

  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+7]]:3: error: semantics TODO: `Unsupported: object parameter type: struct HasQualifiers &&` [SemanticsTodo]
  // CHECK:STDERR:   v.ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+4]]:3: note: in `Cpp` name lookup for `ref_ref_this` [InCppNameLookup]
  // CHECK:STDERR:   v.ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~
  // CHECK:STDERR:
  v.ref_ref_this();

  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+7]]:3: error: semantics TODO: `Unsupported: object parameter type: const struct HasQualifiers &&` [SemanticsTodo]
  // CHECK:STDERR:   v.const_ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_bad_object_param_qualifiers_by_value.carbon:[[@LINE+4]]:3: note: in `Cpp` name lookup for `const_ref_ref_this` [InCppNameLookup]
  // CHECK:STDERR:   v.const_ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  v.const_ref_ref_this();
}

// --- fail_todo_bad_object_param_qualifiers_by_ref.carbon

library "[[@TEST_NAME]]";

import Cpp library "object_param_qualifiers.h";

fn Ref(p: Cpp.HasQualifiers*) {
  // TODO: This should eventually be accepted if we support `volatile`.
  // CHECK:STDERR: fail_todo_bad_object_param_qualifiers_by_ref.carbon:[[@LINE+7]]:3: error: semantics TODO: `Unsupported: qualified type: volatile struct HasQualifiers` [SemanticsTodo]
  // CHECK:STDERR:   p->volatile_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_todo_bad_object_param_qualifiers_by_ref.carbon:[[@LINE+4]]:3: note: in `Cpp` name lookup for `volatile_this` [InCppNameLookup]
  // CHECK:STDERR:   p->volatile_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  p->volatile_this();
}

// --- fail_bad_object_param_qualifiers_ref_ref.carbon

library "[[@TEST_NAME]]";

import Cpp library "object_param_qualifiers.h";

fn Ref(p: Cpp.HasQualifiers*) {
  // CHECK:STDERR: fail_bad_object_param_qualifiers_ref_ref.carbon:[[@LINE+7]]:3: error: semantics TODO: `Unsupported: object parameter type: struct HasQualifiers &&` [SemanticsTodo]
  // CHECK:STDERR:   p->ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_bad_object_param_qualifiers_ref_ref.carbon:[[@LINE+4]]:3: note: in `Cpp` name lookup for `ref_ref_this` [InCppNameLookup]
  // CHECK:STDERR:   p->ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~
  // CHECK:STDERR:
  p->ref_ref_this();

  // CHECK:STDERR: fail_bad_object_param_qualifiers_ref_ref.carbon:[[@LINE+7]]:3: error: semantics TODO: `Unsupported: object parameter type: const struct HasQualifiers &&` [SemanticsTodo]
  // CHECK:STDERR:   p->const_ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_bad_object_param_qualifiers_ref_ref.carbon:[[@LINE+4]]:3: note: in `Cpp` name lookup for `const_ref_ref_this` [InCppNameLookup]
  // CHECK:STDERR:   p->const_ref_ref_this();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  p->const_ref_ref_this();
}

// CHECK:STDOUT: --- use_object_param_qualifiers.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %HasQualifiers: type = class_type @HasQualifiers [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %pattern_type.e15: type = pattern_type %HasQualifiers [concrete]
// CHECK:STDOUT:   %ptr.ec3: type = ptr_type %HasQualifiers [concrete]
// CHECK:STDOUT:   %pattern_type.bc1: type = pattern_type %ptr.ec3 [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT:   %HasQualifiers.const_this.type: type = fn_type @HasQualifiers.const_this [concrete]
// CHECK:STDOUT:   %HasQualifiers.const_this: %HasQualifiers.const_this.type = struct_value () [concrete]
// CHECK:STDOUT:   %HasQualifiers.const_ref_this.type: type = fn_type @HasQualifiers.const_ref_this [concrete]
// CHECK:STDOUT:   %HasQualifiers.const_ref_this: %HasQualifiers.const_ref_this.type = struct_value () [concrete]
// CHECK:STDOUT:   %pattern_type.f6d: type = pattern_type auto [concrete]
// CHECK:STDOUT:   %HasQualifiers.plain.type: type = fn_type @HasQualifiers.plain [concrete]
// CHECK:STDOUT:   %HasQualifiers.plain: %HasQualifiers.plain.type = struct_value () [concrete]
// CHECK:STDOUT:   %HasQualifiers.ref_this.type: type = fn_type @HasQualifiers.ref_this [concrete]
// CHECK:STDOUT:   %HasQualifiers.ref_this: %HasQualifiers.ref_this.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Cpp: <namespace> = namespace file.%Cpp.import_cpp, [concrete] {
// CHECK:STDOUT:     .HasQualifiers = %HasQualifiers.decl
// CHECK:STDOUT:     import Cpp//...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.decl: type = class_decl @HasQualifiers [concrete = constants.%HasQualifiers] {} {}
// CHECK:STDOUT:   %HasQualifiers.const_this.decl: %HasQualifiers.const_this.type = fn_decl @HasQualifiers.const_this [concrete = constants.%HasQualifiers.const_this] {
// CHECK:STDOUT:     %self.patt: %pattern_type.e15 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.e15 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %HasQualifiers = value_param call_param0
// CHECK:STDOUT:     %self: %HasQualifiers = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.const_ref_this.decl: %HasQualifiers.const_ref_this.type = fn_decl @HasQualifiers.const_ref_this [concrete = constants.%HasQualifiers.const_ref_this] {
// CHECK:STDOUT:     %self.patt: %pattern_type.e15 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.e15 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %HasQualifiers = value_param call_param0
// CHECK:STDOUT:     %self: %HasQualifiers = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.plain.decl: %HasQualifiers.plain.type = fn_decl @HasQualifiers.plain [concrete = constants.%HasQualifiers.plain] {
// CHECK:STDOUT:     %self.patt: %pattern_type.bc1 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.bc1 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.1: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ptr.ec3 = value_param call_param0
// CHECK:STDOUT:     %self: %ptr.ec3 = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.ref_this.decl: %HasQualifiers.ref_this.type = fn_decl @HasQualifiers.ref_this [concrete = constants.%HasQualifiers.ref_this] {
// CHECK:STDOUT:     %self.patt: %pattern_type.bc1 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.bc1 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.1: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ptr.ec3 = value_param call_param0
// CHECK:STDOUT:     %self: %ptr.ec3 = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Cpp = imports.%Cpp
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Cpp.import_cpp = import_cpp {
// CHECK:STDOUT:     import Cpp "object_param_qualifiers.h"
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {
// CHECK:STDOUT:     %v.patt: %pattern_type.e15 = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.param_patt: %pattern_type.e15 = value_param_pattern %v.patt, call_param0 [concrete]
// CHECK:STDOUT:     %p.patt: %pattern_type.bc1 = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: %pattern_type.bc1 = value_param_pattern %p.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %v.param: %HasQualifiers = value_param call_param0
// CHECK:STDOUT:     %.loc6_12: type = splice_block %HasQualifiers.ref.loc6_12 [concrete = constants.%HasQualifiers] {
// CHECK:STDOUT:       %Cpp.ref.loc6_9: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:       %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:       %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:       %HasQualifiers.ref.loc6_12: type = name_ref HasQualifiers, imports.%HasQualifiers.decl [concrete = constants.%HasQualifiers]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %v: %HasQualifiers = bind_name v, %v.param
// CHECK:STDOUT:     %p.param: %ptr.ec3 = value_param call_param1
// CHECK:STDOUT:     %.loc6_48: type = splice_block %ptr [concrete = constants.%ptr.ec3] {
// CHECK:STDOUT:       %Cpp.ref.loc6_31: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:       %HasQualifiers.ref.loc6_34: type = name_ref HasQualifiers, imports.%HasQualifiers.decl [concrete = constants.%HasQualifiers]
// CHECK:STDOUT:       %ptr: type = ptr_type %HasQualifiers.ref.loc6_34 [concrete = constants.%ptr.ec3]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: %ptr.ec3 = bind_name p, %p.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @HasQualifiers {
// CHECK:STDOUT:   complete_type_witness = @F.%complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%HasQualifiers
// CHECK:STDOUT:   .const_this = imports.%HasQualifiers.const_this.decl
// CHECK:STDOUT:   .const_ref_this = imports.%HasQualifiers.const_ref_this.decl
// CHECK:STDOUT:   .plain = imports.%HasQualifiers.plain.decl
// CHECK:STDOUT:   .ref_this = imports.%HasQualifiers.ref_this.decl
// CHECK:STDOUT:   import Cpp//...
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F(%v.param: %HasQualifiers, %p.param: %ptr.ec3) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %v.ref.loc8: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %const_this.ref.loc8: %HasQualifiers.const_this.type = name_ref const_this, imports.%HasQualifiers.const_this.decl [concrete = constants.%HasQualifiers.const_this]
// CHECK:STDOUT:   %HasQualifiers.const_this.bound.loc8: <bound method> = bound_method %v.ref.loc8, %const_this.ref.loc8
// CHECK:STDOUT:   %HasQualifiers.const_this.call.loc8: init %empty_tuple.type = call %HasQualifiers.const_this.bound.loc8(%v.ref.loc8)
// CHECK:STDOUT:   %v.ref.loc9: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %const_ref_this.ref.loc9: %HasQualifiers.const_ref_this.type = name_ref const_ref_this, imports.%HasQualifiers.const_ref_this.decl [concrete = constants.%HasQualifiers.const_ref_this]
// CHECK:STDOUT:   %HasQualifiers.const_ref_this.bound.loc9: <bound method> = bound_method %v.ref.loc9, %const_ref_this.ref.loc9
// CHECK:STDOUT:   %HasQualifiers.const_ref_this.call.loc9: init %empty_tuple.type = call %HasQualifiers.const_ref_this.bound.loc9(%v.ref.loc9)
// CHECK:STDOUT:   %p.ref.loc11: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc11: ref %HasQualifiers = deref %p.ref.loc11
// CHECK:STDOUT:   %plain.ref: %HasQualifiers.plain.type = name_ref plain, imports.%HasQualifiers.plain.decl [concrete = constants.%HasQualifiers.plain]
// CHECK:STDOUT:   %HasQualifiers.plain.bound: <bound method> = bound_method %.loc11, %plain.ref
// CHECK:STDOUT:   %addr.loc11: %ptr.ec3 = addr_of %.loc11
// CHECK:STDOUT:   %HasQualifiers.plain.call: init %empty_tuple.type = call %HasQualifiers.plain.bound(%addr.loc11)
// CHECK:STDOUT:   %p.ref.loc12: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc12: ref %HasQualifiers = deref %p.ref.loc12
// CHECK:STDOUT:   %ref_this.ref: %HasQualifiers.ref_this.type = name_ref ref_this, imports.%HasQualifiers.ref_this.decl [concrete = constants.%HasQualifiers.ref_this]
// CHECK:STDOUT:   %HasQualifiers.ref_this.bound: <bound method> = bound_method %.loc12, %ref_this.ref
// CHECK:STDOUT:   %addr.loc12: %ptr.ec3 = addr_of %.loc12
// CHECK:STDOUT:   %HasQualifiers.ref_this.call: init %empty_tuple.type = call %HasQualifiers.ref_this.bound(%addr.loc12)
// CHECK:STDOUT:   %p.ref.loc13: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc13_4.1: ref %HasQualifiers = deref %p.ref.loc13
// CHECK:STDOUT:   %const_this.ref.loc13: %HasQualifiers.const_this.type = name_ref const_this, imports.%HasQualifiers.const_this.decl [concrete = constants.%HasQualifiers.const_this]
// CHECK:STDOUT:   %HasQualifiers.const_this.bound.loc13: <bound method> = bound_method %.loc13_4.1, %const_this.ref.loc13
// CHECK:STDOUT:   %.loc13_4.2: %HasQualifiers = bind_value %.loc13_4.1
// CHECK:STDOUT:   %HasQualifiers.const_this.call.loc13: init %empty_tuple.type = call %HasQualifiers.const_this.bound.loc13(%.loc13_4.2)
// CHECK:STDOUT:   %p.ref.loc14: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc14_4.1: ref %HasQualifiers = deref %p.ref.loc14
// CHECK:STDOUT:   %const_ref_this.ref.loc14: %HasQualifiers.const_ref_this.type = name_ref const_ref_this, imports.%HasQualifiers.const_ref_this.decl [concrete = constants.%HasQualifiers.const_ref_this]
// CHECK:STDOUT:   %HasQualifiers.const_ref_this.bound.loc14: <bound method> = bound_method %.loc14_4.1, %const_ref_this.ref.loc14
// CHECK:STDOUT:   %.loc14_4.2: %HasQualifiers = bind_value %.loc14_4.1
// CHECK:STDOUT:   %HasQualifiers.const_ref_this.call.loc14: init %empty_tuple.type = call %HasQualifiers.const_ref_this.bound.loc14(%.loc14_4.2)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.const_this(%self.param: %HasQualifiers);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.const_ref_this(%self.param: %HasQualifiers);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.plain(%self.param: %ptr.ec3);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.ref_this(%self.param: %ptr.ec3);
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_bad_object_param_qualifiers_by_value.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %HasQualifiers: type = class_type @HasQualifiers [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type.357: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %pattern_type.e15: type = pattern_type %HasQualifiers [concrete]
// CHECK:STDOUT:   %Value.type: type = fn_type @Value [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %Value: %Value.type = struct_value () [concrete]
// CHECK:STDOUT:   %ptr.ec3: type = ptr_type %HasQualifiers [concrete]
// CHECK:STDOUT:   %pattern_type.bc1: type = pattern_type %ptr.ec3 [concrete]
// CHECK:STDOUT:   %pattern_type.f6d: type = pattern_type auto [concrete]
// CHECK:STDOUT:   %HasQualifiers.plain.type: type = fn_type @HasQualifiers.plain [concrete]
// CHECK:STDOUT:   %HasQualifiers.plain: %HasQualifiers.plain.type = struct_value () [concrete]
// CHECK:STDOUT:   %HasQualifiers.volatile_this.type: type = fn_type @HasQualifiers.volatile_this [concrete]
// CHECK:STDOUT:   %HasQualifiers.volatile_this: %HasQualifiers.volatile_this.type = struct_value () [concrete]
// CHECK:STDOUT:   %HasQualifiers.ref_this.type: type = fn_type @HasQualifiers.ref_this [concrete]
// CHECK:STDOUT:   %HasQualifiers.ref_this: %HasQualifiers.ref_this.type = struct_value () [concrete]
// CHECK:STDOUT:   %Destroy.type: type = facet_type <@Destroy> [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.type.e02: type = fn_type @T.as.Destroy.impl.Op, @T.as.Destroy.impl(%HasQualifiers) [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.b7c: %T.as.Destroy.impl.Op.type.e02 = struct_value () [concrete]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn: <specific function> = specific_function %T.as.Destroy.impl.Op.b7c, @T.as.Destroy.impl.Op(%HasQualifiers) [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .Destroy = %Core.Destroy
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Cpp: <namespace> = namespace file.%Cpp.import_cpp, [concrete] {
// CHECK:STDOUT:     .HasQualifiers = %HasQualifiers.decl
// CHECK:STDOUT:     import Cpp//...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.decl: type = class_decl @HasQualifiers [concrete = constants.%HasQualifiers] {} {}
// CHECK:STDOUT:   %HasQualifiers.plain.decl: %HasQualifiers.plain.type = fn_decl @HasQualifiers.plain [concrete = constants.%HasQualifiers.plain] {
// CHECK:STDOUT:     %self.patt: %pattern_type.bc1 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.bc1 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.1: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ptr.ec3 = value_param call_param0
// CHECK:STDOUT:     %self: %ptr.ec3 = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.volatile_this.decl: %HasQualifiers.volatile_this.type = fn_decl @HasQualifiers.volatile_this [concrete = constants.%HasQualifiers.volatile_this] {
// CHECK:STDOUT:     %self.patt: <error> = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: <error> = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.1: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: <error> = value_param call_param0
// CHECK:STDOUT:     %self: <error> = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.ref_this.decl: %HasQualifiers.ref_this.type = fn_decl @HasQualifiers.ref_this [concrete = constants.%HasQualifiers.ref_this] {
// CHECK:STDOUT:     %self.patt: %pattern_type.bc1 = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type.bc1 = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.1: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %ptr.ec3 = value_param call_param0
// CHECK:STDOUT:     %self: %ptr.ec3 = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.Destroy: type = import_ref Core//prelude/parts/destroy, Destroy, loaded [concrete = constants.%Destroy.type]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Cpp = imports.%Cpp
// CHECK:STDOUT:     .Value = %Value.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Cpp.import_cpp = import_cpp {
// CHECK:STDOUT:     import Cpp "object_param_qualifiers.h"
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Value.decl: %Value.type = fn_decl @Value [concrete = constants.%Value] {
// CHECK:STDOUT:     %v.patt: %pattern_type.e15 = binding_pattern v [concrete]
// CHECK:STDOUT:     %v.param_patt: %pattern_type.e15 = value_param_pattern %v.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %v.param: %HasQualifiers = value_param call_param0
// CHECK:STDOUT:     %.loc6: type = splice_block %HasQualifiers.ref [concrete = constants.%HasQualifiers] {
// CHECK:STDOUT:       %Cpp.ref: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:       %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:       %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type.357]
// CHECK:STDOUT:       %HasQualifiers.ref: type = name_ref HasQualifiers, imports.%HasQualifiers.decl [concrete = constants.%HasQualifiers]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %v: %HasQualifiers = bind_name v, %v.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @HasQualifiers {
// CHECK:STDOUT:   complete_type_witness = @Value.%complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%HasQualifiers
// CHECK:STDOUT:   .plain = imports.%HasQualifiers.plain.decl
// CHECK:STDOUT:   .volatile_this = imports.%HasQualifiers.volatile_this.decl
// CHECK:STDOUT:   .ref_this = imports.%HasQualifiers.ref_this.decl
// CHECK:STDOUT:   .ref_ref_this = <error>
// CHECK:STDOUT:   .const_ref_ref_this = <error>
// CHECK:STDOUT:   import Cpp//...
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Value(%v.param: %HasQualifiers) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %v.ref.loc12: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %plain.ref: %HasQualifiers.plain.type = name_ref plain, imports.%HasQualifiers.plain.decl [concrete = constants.%HasQualifiers.plain]
// CHECK:STDOUT:   %HasQualifiers.plain.bound: <bound method> = bound_method %v.ref.loc12, %plain.ref
// CHECK:STDOUT:   %.loc12: ref %HasQualifiers = temporary_storage
// CHECK:STDOUT:   %addr.loc12_3.1: %ptr.ec3 = addr_of %.loc12
// CHECK:STDOUT:   %HasQualifiers.plain.call: init %empty_tuple.type = call %HasQualifiers.plain.bound(%addr.loc12_3.1)
// CHECK:STDOUT:   %v.ref.loc27: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %volatile_this.ref: %HasQualifiers.volatile_this.type = name_ref volatile_this, imports.%HasQualifiers.volatile_this.decl [concrete = constants.%HasQualifiers.volatile_this]
// CHECK:STDOUT:   %HasQualifiers.volatile_this.bound: <bound method> = bound_method %v.ref.loc27, %volatile_this.ref
// CHECK:STDOUT:   %.loc27: ref %HasQualifiers = temporary_storage
// CHECK:STDOUT:   %addr.loc27_3.1: %ptr.ec3 = addr_of %.loc27
// CHECK:STDOUT:   %HasQualifiers.volatile_this.call: init %empty_tuple.type = call %HasQualifiers.volatile_this.bound(<error>)
// CHECK:STDOUT:   %v.ref.loc34: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %ref_this.ref: %HasQualifiers.ref_this.type = name_ref ref_this, imports.%HasQualifiers.ref_this.decl [concrete = constants.%HasQualifiers.ref_this]
// CHECK:STDOUT:   %HasQualifiers.ref_this.bound: <bound method> = bound_method %v.ref.loc34, %ref_this.ref
// CHECK:STDOUT:   %.loc34: ref %HasQualifiers = temporary_storage
// CHECK:STDOUT:   %addr.loc34_3.1: %ptr.ec3 = addr_of %.loc34
// CHECK:STDOUT:   %HasQualifiers.ref_this.call: init %empty_tuple.type = call %HasQualifiers.ref_this.bound(%addr.loc34_3.1)
// CHECK:STDOUT:   %v.ref.loc43: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %ref_ref_this.ref: <error> = name_ref ref_ref_this, <error> [concrete = <error>]
// CHECK:STDOUT:   %v.ref.loc52: %HasQualifiers = name_ref v, %v
// CHECK:STDOUT:   %const_ref_ref_this.ref: <error> = name_ref const_ref_ref_this, <error> [concrete = <error>]
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc34: <bound method> = bound_method %.loc34, constants.%T.as.Destroy.impl.Op.b7c
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.1: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.b7c, @T.as.Destroy.impl.Op(constants.%HasQualifiers) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn]
// CHECK:STDOUT:   %bound_method.loc34: <bound method> = bound_method %.loc34, %T.as.Destroy.impl.Op.specific_fn.1
// CHECK:STDOUT:   %addr.loc34_3.2: %ptr.ec3 = addr_of %.loc34
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc34: init %empty_tuple.type = call %bound_method.loc34(%addr.loc34_3.2)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc27: <bound method> = bound_method %.loc27, constants.%T.as.Destroy.impl.Op.b7c
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.2: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.b7c, @T.as.Destroy.impl.Op(constants.%HasQualifiers) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn]
// CHECK:STDOUT:   %bound_method.loc27: <bound method> = bound_method %.loc27, %T.as.Destroy.impl.Op.specific_fn.2
// CHECK:STDOUT:   %addr.loc27_3.2: %ptr.ec3 = addr_of %.loc27
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc27: init %empty_tuple.type = call %bound_method.loc27(%addr.loc27_3.2)
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.bound.loc12: <bound method> = bound_method %.loc12, constants.%T.as.Destroy.impl.Op.b7c
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.specific_fn.3: <specific function> = specific_function constants.%T.as.Destroy.impl.Op.b7c, @T.as.Destroy.impl.Op(constants.%HasQualifiers) [concrete = constants.%T.as.Destroy.impl.Op.specific_fn]
// CHECK:STDOUT:   %bound_method.loc12: <bound method> = bound_method %.loc12, %T.as.Destroy.impl.Op.specific_fn.3
// CHECK:STDOUT:   %addr.loc12_3.2: %ptr.ec3 = addr_of %.loc12
// CHECK:STDOUT:   %T.as.Destroy.impl.Op.call.loc12: init %empty_tuple.type = call %bound_method.loc12(%addr.loc12_3.2)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.plain(%self.param: %ptr.ec3);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.volatile_this(%self.param: <error>);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.ref_this(%self.param: %ptr.ec3);
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_todo_bad_object_param_qualifiers_by_ref.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %HasQualifiers: type = class_type @HasQualifiers [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %ptr.ec3: type = ptr_type %HasQualifiers [concrete]
// CHECK:STDOUT:   %pattern_type.bc1: type = pattern_type %ptr.ec3 [concrete]
// CHECK:STDOUT:   %Ref.type: type = fn_type @Ref [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %Ref: %Ref.type = struct_value () [concrete]
// CHECK:STDOUT:   %pattern_type.f6d: type = pattern_type auto [concrete]
// CHECK:STDOUT:   %HasQualifiers.volatile_this.type: type = fn_type @HasQualifiers.volatile_this [concrete]
// CHECK:STDOUT:   %HasQualifiers.volatile_this: %HasQualifiers.volatile_this.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Cpp: <namespace> = namespace file.%Cpp.import_cpp, [concrete] {
// CHECK:STDOUT:     .HasQualifiers = %HasQualifiers.decl
// CHECK:STDOUT:     import Cpp//...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.decl: type = class_decl @HasQualifiers [concrete = constants.%HasQualifiers] {} {}
// CHECK:STDOUT:   %HasQualifiers.volatile_this.decl: %HasQualifiers.volatile_this.type = fn_decl @HasQualifiers.volatile_this [concrete = constants.%HasQualifiers.volatile_this] {
// CHECK:STDOUT:     %self.patt: <error> = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: <error> = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:     %.1: %pattern_type.f6d = addr_pattern %self.param_patt [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: <error> = value_param call_param0
// CHECK:STDOUT:     %self: <error> = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Cpp = imports.%Cpp
// CHECK:STDOUT:     .Ref = %Ref.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Cpp.import_cpp = import_cpp {
// CHECK:STDOUT:     import Cpp "object_param_qualifiers.h"
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Ref.decl: %Ref.type = fn_decl @Ref [concrete = constants.%Ref] {
// CHECK:STDOUT:     %p.patt: %pattern_type.bc1 = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: %pattern_type.bc1 = value_param_pattern %p.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %p.param: %ptr.ec3 = value_param call_param0
// CHECK:STDOUT:     %.loc6: type = splice_block %ptr [concrete = constants.%ptr.ec3] {
// CHECK:STDOUT:       %Cpp.ref: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:       %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:       %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:       %HasQualifiers.ref: type = name_ref HasQualifiers, imports.%HasQualifiers.decl [concrete = constants.%HasQualifiers]
// CHECK:STDOUT:       %ptr: type = ptr_type %HasQualifiers.ref [concrete = constants.%ptr.ec3]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: %ptr.ec3 = bind_name p, %p.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @HasQualifiers {
// CHECK:STDOUT:   complete_type_witness = @Ref.%complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%HasQualifiers
// CHECK:STDOUT:   .volatile_this = imports.%HasQualifiers.volatile_this.decl
// CHECK:STDOUT:   import Cpp//...
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Ref(%p.param: %ptr.ec3) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %p.ref: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc15: ref %HasQualifiers = deref %p.ref
// CHECK:STDOUT:   %volatile_this.ref: %HasQualifiers.volatile_this.type = name_ref volatile_this, imports.%HasQualifiers.volatile_this.decl [concrete = constants.%HasQualifiers.volatile_this]
// CHECK:STDOUT:   %HasQualifiers.volatile_this.bound: <bound method> = bound_method %.loc15, %volatile_this.ref
// CHECK:STDOUT:   %addr: %ptr.ec3 = addr_of %.loc15
// CHECK:STDOUT:   %HasQualifiers.volatile_this.call: init %empty_tuple.type = call %HasQualifiers.volatile_this.bound(<error>)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @HasQualifiers.volatile_this(%self.param: <error>);
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_bad_object_param_qualifiers_ref_ref.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %HasQualifiers: type = class_type @HasQualifiers [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %ptr.ec3: type = ptr_type %HasQualifiers [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %ptr.ec3 [concrete]
// CHECK:STDOUT:   %Ref.type: type = fn_type @Ref [concrete]
// CHECK:STDOUT:   %Ref: %Ref.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Cpp: <namespace> = namespace file.%Cpp.import_cpp, [concrete] {
// CHECK:STDOUT:     .HasQualifiers = %HasQualifiers.decl
// CHECK:STDOUT:     import Cpp//...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %HasQualifiers.decl: type = class_decl @HasQualifiers [concrete = constants.%HasQualifiers] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Cpp = imports.%Cpp
// CHECK:STDOUT:     .Ref = %Ref.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Cpp.import_cpp = import_cpp {
// CHECK:STDOUT:     import Cpp "object_param_qualifiers.h"
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Ref.decl: %Ref.type = fn_decl @Ref [concrete = constants.%Ref] {
// CHECK:STDOUT:     %p.patt: %pattern_type = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: %pattern_type = value_param_pattern %p.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %p.param: %ptr.ec3 = value_param call_param0
// CHECK:STDOUT:     %.loc6: type = splice_block %ptr [concrete = constants.%ptr.ec3] {
// CHECK:STDOUT:       %Cpp.ref: <namespace> = name_ref Cpp, imports.%Cpp [concrete = imports.%Cpp]
// CHECK:STDOUT:       %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:       %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:       %HasQualifiers.ref: type = name_ref HasQualifiers, imports.%HasQualifiers.decl [concrete = constants.%HasQualifiers]
// CHECK:STDOUT:       %ptr: type = ptr_type %HasQualifiers.ref [concrete = constants.%ptr.ec3]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: %ptr.ec3 = bind_name p, %p.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @HasQualifiers {
// CHECK:STDOUT:   complete_type_witness = @Ref.%complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%HasQualifiers
// CHECK:STDOUT:   .ref_ref_this = <error>
// CHECK:STDOUT:   .const_ref_ref_this = <error>
// CHECK:STDOUT:   import Cpp//...
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @Ref(%p.param: %ptr.ec3) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %p.ref.loc14: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc14: ref %HasQualifiers = deref %p.ref.loc14
// CHECK:STDOUT:   %ref_ref_this.ref: <error> = name_ref ref_ref_this, <error> [concrete = <error>]
// CHECK:STDOUT:   %p.ref.loc23: %ptr.ec3 = name_ref p, %p
// CHECK:STDOUT:   %.loc23: ref %HasQualifiers = deref %p.ref.loc23
// CHECK:STDOUT:   %const_ref_ref_this.ref: <error> = name_ref const_ref_ref_this, <error> [concrete = <error>]
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
