// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: Add ranges and switch to "--dump-sem-ir-ranges=only".
// EXTRA-ARGS: --dump-sem-ir-ranges=if-present
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/class/fail_method.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/class/fail_method.carbon

class Class {
  fn NoSelf();
  fn WithSelf[self: Class]();
}

alias A = Class.WithSelf;

fn F(c: Class) {
  c.NoSelf();
  c.WithSelf();

  Class.NoSelf();
  // CHECK:STDERR: fail_method.carbon:[[@LINE+7]]:3: error: missing object argument in method call [MissingObjectInMethodCall]
  // CHECK:STDERR:   Class.WithSelf();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_method.carbon:[[@LINE-13]]:3: note: calling function declared here [InCallToFunction]
  // CHECK:STDERR:   fn WithSelf[self: Class]();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Class.WithSelf();
  // CHECK:STDERR: fail_method.carbon:[[@LINE+7]]:3: error: 1 argument passed to function expecting 0 arguments [CallArgCountMismatch]
  // CHECK:STDERR:   Class.WithSelf(c);
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~
  // CHECK:STDERR: fail_method.carbon:[[@LINE-21]]:3: note: calling function declared here [InCallToEntity]
  // CHECK:STDERR:   fn WithSelf[self: Class]();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  Class.WithSelf(c);

  // CHECK:STDERR: fail_method.carbon:[[@LINE+7]]:3: error: missing object argument in method call [MissingObjectInMethodCall]
  // CHECK:STDERR:   A();
  // CHECK:STDERR:   ^~~
  // CHECK:STDERR: fail_method.carbon:[[@LINE-30]]:3: note: calling function declared here [InCallToFunction]
  // CHECK:STDERR:   fn WithSelf[self: Class]();
  // CHECK:STDERR:   ^~~~~~~~~~~~~~~~~~~~~~~~~~~
  // CHECK:STDERR:
  A();
}

// CHECK:STDOUT: --- fail_method.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %Class: type = class_type @Class [concrete]
// CHECK:STDOUT:   %NoSelf.type: type = fn_type @NoSelf [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %NoSelf: %NoSelf.type = struct_value () [concrete]
// CHECK:STDOUT:   %pattern_type: type = pattern_type %Class [concrete]
// CHECK:STDOUT:   %WithSelf.type: type = fn_type @WithSelf [concrete]
// CHECK:STDOUT:   %WithSelf: %WithSelf.type = struct_value () [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Core: <namespace> = namespace file.%Core.import, [concrete] {
// CHECK:STDOUT:     import Core//prelude
// CHECK:STDOUT:     import Core//prelude/...
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .Core = imports.%Core
// CHECK:STDOUT:     .Class = %Class.decl
// CHECK:STDOUT:     .A = %A
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Core.import = import Core
// CHECK:STDOUT:   %Class.decl: type = class_decl @Class [concrete = constants.%Class] {} {}
// CHECK:STDOUT:   %Class.ref: type = name_ref Class, %Class.decl [concrete = constants.%Class]
// CHECK:STDOUT:   %WithSelf.ref: %WithSelf.type = name_ref WithSelf, @Class.%WithSelf.decl [concrete = constants.%WithSelf]
// CHECK:STDOUT:   %A: %WithSelf.type = bind_alias A, @Class.%WithSelf.decl [concrete = constants.%WithSelf]
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {
// CHECK:STDOUT:     %c.patt: %pattern_type = binding_pattern c [concrete]
// CHECK:STDOUT:     %c.param_patt: %pattern_type = value_param_pattern %c.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %c.param: %Class = value_param call_param0
// CHECK:STDOUT:     %Class.ref.loc21: type = name_ref Class, file.%Class.decl [concrete = constants.%Class]
// CHECK:STDOUT:     %c: %Class = bind_name c, %c.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @Class {
// CHECK:STDOUT:   %NoSelf.decl: %NoSelf.type = fn_decl @NoSelf [concrete = constants.%NoSelf] {} {}
// CHECK:STDOUT:   %WithSelf.decl: %WithSelf.type = fn_decl @WithSelf [concrete = constants.%WithSelf] {
// CHECK:STDOUT:     %self.patt: %pattern_type = binding_pattern self [concrete]
// CHECK:STDOUT:     %self.param_patt: %pattern_type = value_param_pattern %self.patt, call_param0 [concrete]
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %self.param: %Class = value_param call_param0
// CHECK:STDOUT:     %Class.ref: type = name_ref Class, file.%Class.decl [concrete = constants.%Class]
// CHECK:STDOUT:     %self: %Class = bind_name self, %self.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete = constants.%empty_struct_type]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%Class
// CHECK:STDOUT:   .NoSelf = %NoSelf.decl
// CHECK:STDOUT:   .Class = <poisoned>
// CHECK:STDOUT:   .WithSelf = %WithSelf.decl
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @NoSelf();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @WithSelf(%self.param: %Class);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F(%c.param: %Class) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %c.ref.loc22: %Class = name_ref c, %c
// CHECK:STDOUT:   %NoSelf.ref.loc22: %NoSelf.type = name_ref NoSelf, @Class.%NoSelf.decl [concrete = constants.%NoSelf]
// CHECK:STDOUT:   %NoSelf.call.loc22: init %empty_tuple.type = call %NoSelf.ref.loc22()
// CHECK:STDOUT:   %c.ref.loc23: %Class = name_ref c, %c
// CHECK:STDOUT:   %WithSelf.ref.loc23: %WithSelf.type = name_ref WithSelf, @Class.%WithSelf.decl [concrete = constants.%WithSelf]
// CHECK:STDOUT:   %WithSelf.bound: <bound method> = bound_method %c.ref.loc23, %WithSelf.ref.loc23
// CHECK:STDOUT:   %WithSelf.call.loc23: init %empty_tuple.type = call %WithSelf.bound(%c.ref.loc23)
// CHECK:STDOUT:   %Class.ref.loc25: type = name_ref Class, file.%Class.decl [concrete = constants.%Class]
// CHECK:STDOUT:   %NoSelf.ref.loc25: %NoSelf.type = name_ref NoSelf, @Class.%NoSelf.decl [concrete = constants.%NoSelf]
// CHECK:STDOUT:   %NoSelf.call.loc25: init %empty_tuple.type = call %NoSelf.ref.loc25()
// CHECK:STDOUT:   %Class.ref.loc33: type = name_ref Class, file.%Class.decl [concrete = constants.%Class]
// CHECK:STDOUT:   %WithSelf.ref.loc33: %WithSelf.type = name_ref WithSelf, @Class.%WithSelf.decl [concrete = constants.%WithSelf]
// CHECK:STDOUT:   %WithSelf.call.loc33: init %empty_tuple.type = call %WithSelf.ref.loc33(<error>)
// CHECK:STDOUT:   %Class.ref.loc41: type = name_ref Class, file.%Class.decl [concrete = constants.%Class]
// CHECK:STDOUT:   %WithSelf.ref.loc41: %WithSelf.type = name_ref WithSelf, @Class.%WithSelf.decl [concrete = constants.%WithSelf]
// CHECK:STDOUT:   %c.ref.loc41: %Class = name_ref c, %c
// CHECK:STDOUT:   %A.ref: %WithSelf.type = name_ref A, file.%A [concrete = constants.%WithSelf]
// CHECK:STDOUT:   %WithSelf.call.loc50: init %empty_tuple.type = call %A.ref(<error>)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
