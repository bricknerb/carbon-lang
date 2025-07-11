// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/convert.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/pointer/fail_type_mismatch.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/pointer/fail_type_mismatch.carbon

fn ConstMismatch(p: const {}*) -> const ({}*) {
  // CHECK:STDERR: fail_type_mismatch.carbon:[[@LINE+7]]:3: error: cannot implicitly convert expression of type `const {}*` to `const ({}*)` [ConversionFailure]
  // CHECK:STDERR:   return p;
  // CHECK:STDERR:   ^~~~~~~~~
  // CHECK:STDERR: fail_type_mismatch.carbon:[[@LINE+4]]:3: note: type `const {}*` does not implement interface `Core.ImplicitAs(const ({}*))` [MissingImplInMemberAccessNote]
  // CHECK:STDERR:   return p;
  // CHECK:STDERR:   ^~~~~~~~~
  // CHECK:STDERR:
  return p;
}

// CHECK:STDOUT: --- fail_type_mismatch.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %const.c48: type = const_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %ptr.bf9: type = ptr_type %const.c48 [concrete]
// CHECK:STDOUT:   %pattern_type.db0: type = pattern_type %ptr.bf9 [concrete]
// CHECK:STDOUT:   %ptr.c28: type = ptr_type %empty_struct_type [concrete]
// CHECK:STDOUT:   %const.987: type = const_type %ptr.c28 [concrete]
// CHECK:STDOUT:   %pattern_type.3c6: type = pattern_type %const.987 [concrete]
// CHECK:STDOUT:   %ConstMismatch.type: type = fn_type @ConstMismatch [concrete]
// CHECK:STDOUT:   %ConstMismatch: %ConstMismatch.type = struct_value () [concrete]
// CHECK:STDOUT:   %ImplicitAs.type.cc7: type = generic_interface_type @ImplicitAs [concrete]
// CHECK:STDOUT:   %ImplicitAs.generic: %ImplicitAs.type.cc7 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     .ImplicitAs = %Core.ImplicitAs
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.ImplicitAs: %ImplicitAs.type.cc7 = import_ref Core//prelude/parts/as, ImplicitAs, loaded [concrete = constants.%ImplicitAs.generic]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .ConstMismatch = %ConstMismatch.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %ConstMismatch.decl: %ConstMismatch.type = fn_decl @ConstMismatch [concrete = constants.%ConstMismatch] {
// CHECK:STDOUT:     %p.patt: %pattern_type.db0 = binding_pattern p [concrete]
// CHECK:STDOUT:     %p.param_patt: %pattern_type.db0 = value_param_pattern %p.patt, call_param0 [concrete]
// CHECK:STDOUT:     %return.patt: %pattern_type.3c6 = return_slot_pattern [concrete]
// CHECK:STDOUT:     %return.param_patt: %pattern_type.3c6 = out_param_pattern %return.patt, call_param1 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %.loc15_43: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:     %.loc15_44: type = converted %.loc15_43, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:     %ptr.loc15_44: type = ptr_type %.loc15_44 [concrete = constants.%ptr.c28]
// CHECK:STDOUT:     %const.loc15_35: type = const_type %ptr.loc15_44 [concrete = constants.%const.987]
// CHECK:STDOUT:     %p.param: %ptr.bf9 = value_param call_param0
// CHECK:STDOUT:     %.loc15_29: type = splice_block %ptr.loc15_29 [concrete = constants.%ptr.bf9] {
// CHECK:STDOUT:       %.loc15_28: %empty_struct_type = struct_literal ()
// CHECK:STDOUT:       %.loc15_21: type = converted %.loc15_28, constants.%empty_struct_type [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:       %const.loc15_21: type = const_type %.loc15_21 [concrete = constants.%const.c48]
// CHECK:STDOUT:       %ptr.loc15_29: type = ptr_type %const.loc15_21 [concrete = constants.%ptr.bf9]
// CHECK:STDOUT:     }
// CHECK:STDOUT:     %p: %ptr.bf9 = bind_name p, %p.param
// CHECK:STDOUT:     %return.param: ref %const.987 = out_param call_param1
// CHECK:STDOUT:     %return: ref %const.987 = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @ConstMismatch(%p.param: %ptr.bf9) -> %const.987 {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %p.ref: %ptr.bf9 = name_ref p, %p
// CHECK:STDOUT:   %.loc23: %const.987 = converted %p.ref, <error> [concrete = <error>]
// CHECK:STDOUT:   return <error>
// CHECK:STDOUT: }
// CHECK:STDOUT:
