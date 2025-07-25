// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/none.carbon
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/interface/fail_redeclare_member.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/interface/fail_redeclare_member.carbon

interface Interface {
  fn F();
  // CHECK:STDERR: fail_redeclare_member.carbon:[[@LINE+7]]:6: error: duplicate name `F` being declared in the same scope [NameDeclDuplicate]
  // CHECK:STDERR:   fn F();
  // CHECK:STDERR:      ^
  // CHECK:STDERR: fail_redeclare_member.carbon:[[@LINE-4]]:3: note: name is previously declared here [NameDeclPrevious]
  // CHECK:STDERR:   fn F();
  // CHECK:STDERR:   ^~~~~~~
  // CHECK:STDERR:
  fn F();
}

// CHECK:STDOUT: --- fail_redeclare_member.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Interface.type: type = facet_type <@Interface> [concrete]
// CHECK:STDOUT:   %Self: %Interface.type = bind_symbolic_name Self, 0 [symbolic]
// CHECK:STDOUT:   %Interface.F.type.1ad64d.1: type = fn_type @Interface.F.loc16 [concrete]
// CHECK:STDOUT:   %Interface.F.5d382e.1: %Interface.F.type.1ad64d.1 = struct_value () [concrete]
// CHECK:STDOUT:   %Interface.assoc_type: type = assoc_entity_type @Interface [concrete]
// CHECK:STDOUT:   %assoc0: %Interface.assoc_type = assoc_entity element0, @Interface.%Interface.F.decl.loc16 [concrete]
// CHECK:STDOUT:   %Interface.F.type.1ad64d.2: type = fn_type @Interface.F.loc24 [concrete]
// CHECK:STDOUT:   %Interface.F.5d382e.2: %Interface.F.type.1ad64d.2 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Interface = %Interface.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Interface.decl: type = interface_decl @Interface [concrete = constants.%Interface.type] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: interface @Interface {
// CHECK:STDOUT:   %Self: %Interface.type = bind_symbolic_name Self, 0 [symbolic = constants.%Self]
// CHECK:STDOUT:   %Interface.F.decl.loc16: %Interface.F.type.1ad64d.1 = fn_decl @Interface.F.loc16 [concrete = constants.%Interface.F.5d382e.1] {} {}
// CHECK:STDOUT:   %assoc0: %Interface.assoc_type = assoc_entity element0, %Interface.F.decl.loc16 [concrete = constants.%assoc0]
// CHECK:STDOUT:   %Interface.F.decl.loc24: %Interface.F.type.1ad64d.2 = fn_decl @Interface.F.loc24 [concrete = constants.%Interface.F.5d382e.2] {} {}
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = %Self
// CHECK:STDOUT:   .F = %assoc0
// CHECK:STDOUT:   witness = (%Interface.F.decl.loc16)
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Interface.F.loc16(@Interface.%Self: %Interface.type) {
// CHECK:STDOUT:   fn();
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: generic fn @Interface.F.loc24(@Interface.%Self: %Interface.type) {
// CHECK:STDOUT:   fn();
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Interface.F.loc16(constants.%Self) {}
// CHECK:STDOUT:
// CHECK:STDOUT: specific @Interface.F.loc24(constants.%Self) {}
// CHECK:STDOUT:
