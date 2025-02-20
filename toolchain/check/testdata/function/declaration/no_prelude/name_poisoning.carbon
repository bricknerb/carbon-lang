// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/function/declaration/no_prelude/name_poisoning.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/function/declaration/no_prelude/name_poisoning.carbon

// --- no_poison.carbon

library "[[@TEST_NAME]]";

fn F1();

// N.F2 uses N.F1 and not F1.
namespace N;
fn N.F1();
alias N.F2 = F1;

// --- poison.carbon

library "[[@TEST_NAME]]";

fn F1();

// Use F1 and poison N.F1.
namespace N;
alias N.F2 = F1;

// --- fail_declare_after_poison.carbon

library "[[@TEST_NAME]]";

fn F1();

namespace N;
// Use F1 and poison N.F1.
// CHECK:STDERR: fail_declare_after_poison.carbon:[[@LINE+3]]:14: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N.F2 = F1;
// CHECK:STDERR:              ^~
alias N.F2 = F1;

// Failure: N.F1 declared after it was poisoned.
// CHECK:STDERR: fail_declare_after_poison.carbon:[[@LINE+4]]:6: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn N.F1();
// CHECK:STDERR:      ^~
// CHECK:STDERR:
fn N.F1();

// --- fail_use_poison.carbon

library "[[@TEST_NAME]]";

fn F1();

// Use F1 and poison N.F1.
namespace N;
alias N.F2 = F1;

// CHECK:STDERR: fail_use_poison.carbon:[[@LINE+4]]:14: error: member name `F1` not found in `N` [MemberNameNotFoundInScope]
// CHECK:STDERR: alias N.F3 = N.F1;
// CHECK:STDERR:              ^~~~
// CHECK:STDERR:
alias N.F3 = N.F1;

// --- fail_use_declaration_after_poison.carbon

library "[[@TEST_NAME]]";

fn F1();

namespace N;
// Use F1 and poison N.F1.
// CHECK:STDERR: fail_use_declaration_after_poison.carbon:[[@LINE+3]]:14: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N.F2 = F1;
// CHECK:STDERR:              ^~
alias N.F2 = F1;

// Failure: N.F1 declared after it was poisoned.
// CHECK:STDERR: fail_use_declaration_after_poison.carbon:[[@LINE+4]]:6: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn N.F1();
// CHECK:STDERR:      ^~
// CHECK:STDERR:
fn N.F1();

// Failure: N.F1 used after declaration failed.
// CHECK:STDERR: fail_use_declaration_after_poison.carbon:[[@LINE+4]]:14: error: member name `F1` not found in `N` [MemberNameNotFoundInScope]
// CHECK:STDERR: alias N.F3 = N.F1;
// CHECK:STDERR:              ^~~~
// CHECK:STDERR:
alias N.F3 = N.F1;

// --- fail_poison_multiple_scopes.carbon

library "[[@TEST_NAME]]";

fn F1();

namespace N1;
namespace N1.N2;
namespace N1.N2.N3;
// Use F1 and poison:
// * N1.F1
// * N1.N2.F1
// * N1.N2.N3.F1
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+3]]:21: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N1.N2.N3.F2 = F1;
// CHECK:STDERR:                     ^~
alias N1.N2.N3.F2 = F1;

// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+7]]:7: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn N1.F1();
// CHECK:STDERR:       ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE-6]]:21: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N1.N2.N3.F2 = F1;
// CHECK:STDERR:                     ^~
fn N1.F1();
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+7]]:10: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn N1.N2.F1();
// CHECK:STDERR:          ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE-14]]:21: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N1.N2.N3.F2 = F1;
// CHECK:STDERR:                     ^~
fn N1.N2.F1();
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:13: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn N1.N2.N3.F1();
// CHECK:STDERR:             ^~
// CHECK:STDERR:
fn N1.N2.N3.F1();

// --- fail_alias.carbon

library "[[@TEST_NAME]]";

fn F();

namespace N;
// CHECK:STDERR: fail_alias.carbon:[[@LINE+7]]:13: error: name `F` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N.F = F;
// CHECK:STDERR:             ^
// CHECK:STDERR: fail_alias.carbon:[[@LINE+4]]:9: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: alias N.F = F;
// CHECK:STDERR:         ^
// CHECK:STDERR:
alias N.F = F;

// --- ignored_poison_in_import.carbon

library "[[@TEST_NAME]]";
import library "poison";

// This doesn't fail.
fn N.F1();

// --- poison.impl.carbon

impl library "[[@TEST_NAME]]";

// TODO: This should fail since N.F1 was poisoned in the api.
fn N.F1() {}

// --- fail_poison_when_lookup_fails.carbon

library "[[@TEST_NAME]]";

namespace N;
// N.F1 poisoned when not found.
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+7]]:14: error: name `F1` not found [NameNotFound]
// CHECK:STDERR: alias N.F2 = F1;
// CHECK:STDERR:              ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+3]]:14: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N.F2 = F1;
// CHECK:STDERR:              ^~
alias N.F2 = F1;

// TODO: We should ideally only produce one diagnostic here.
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+7]]:4: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn F1();
// CHECK:STDERR:    ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE-7]]:14: error: name `F1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N.F2 = F1;
// CHECK:STDERR:              ^~
fn F1();
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+4]]:6: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: fn N.F1();
// CHECK:STDERR:      ^~
// CHECK:STDERR:
fn N.F1();

// --- fail_poison_with_lexical_result.carbon

library "[[@TEST_NAME]]";

fn F1() {
  fn F2();

  class C {
    // CHECK:STDERR: fail_poison_with_lexical_result.carbon:[[@LINE+3]]:16: error: name `F2` used before it was declared [NameUseBeforeDecl]
    // CHECK:STDERR:     alias F3 = F2;
    // CHECK:STDERR:                ^~
    alias F3 = F2;

    // CHECK:STDERR: fail_poison_with_lexical_result.carbon:[[@LINE+4]]:8: note: declared here [NameUseBeforeDeclNote]
    // CHECK:STDERR:     fn F2();
    // CHECK:STDERR:        ^~
    // CHECK:STDERR:
    fn F2();
  }
}

// CHECK:STDOUT: --- no_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type.75d: type = fn_type @F1.1 [concrete]
// CHECK:STDOUT:   %F1.afe: %F1.type.75d = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.fe6: type = fn_type @F1.2 [concrete]
// CHECK:STDOUT:   %F1.cc1: %F1.type.fe6 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl.loc4
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl.loc4: %F1.type.75d = fn_decl @F1.1 [concrete = constants.%F1.afe] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl.loc8
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl.loc8: %F1.type.fe6 = fn_decl @F1.2 [concrete = constants.%F1.cc1] {} {}
// CHECK:STDOUT:   %F1.ref: %F1.type.fe6 = name_ref F1, %F1.decl.loc8 [concrete = constants.%F1.cc1]
// CHECK:STDOUT:   %F2: %F1.type.fe6 = bind_alias F2, %F1.decl.loc8 [concrete = constants.%F1.cc1]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.1();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.2();
// CHECK:STDOUT:
// CHECK:STDOUT: --- poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [concrete]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [concrete = constants.%F1] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.ref: %F1.type = name_ref F1, %F1.decl [concrete = constants.%F1]
// CHECK:STDOUT:   %F2: %F1.type = bind_alias F2, %F1.decl [concrete = constants.%F1]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1();
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_declare_after_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type.75d: type = fn_type @F1.1 [concrete]
// CHECK:STDOUT:   %F1.afe: %F1.type.75d = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.fe6: type = fn_type @F1.2 [concrete]
// CHECK:STDOUT:   %F1.cc1: %F1.type.fe6 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl.loc4
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl.loc4: %F1.type.75d = fn_decl @F1.1 [concrete = constants.%F1.afe] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.ref: %F1.type.75d = name_ref F1, %F1.decl.loc4 [concrete = constants.%F1.afe]
// CHECK:STDOUT:   %F2: %F1.type.75d = bind_alias F2, %F1.decl.loc4 [concrete = constants.%F1.afe]
// CHECK:STDOUT:   %F1.decl.loc18: %F1.type.fe6 = fn_decl @F1.2 [concrete = constants.%F1.cc1] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.1();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.2();
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_use_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [concrete]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [concrete = constants.%F1] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:     .N = <poisoned>
// CHECK:STDOUT:     .F3 = %F3
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.ref.loc8: %F1.type = name_ref F1, %F1.decl [concrete = constants.%F1]
// CHECK:STDOUT:   %F2: %F1.type = bind_alias F2, %F1.decl [concrete = constants.%F1]
// CHECK:STDOUT:   %N.ref: <namespace> = name_ref N, %N [concrete = %N]
// CHECK:STDOUT:   %F1.ref.loc14: <error> = name_ref F1, <error> [concrete = <error>]
// CHECK:STDOUT:   %F3: <error> = bind_alias F3, <error> [concrete = <error>]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1();
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_use_declaration_after_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type.75d: type = fn_type @F1.1 [concrete]
// CHECK:STDOUT:   %F1.afe: %F1.type.75d = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.fe6: type = fn_type @F1.2 [concrete]
// CHECK:STDOUT:   %F1.cc1: %F1.type.fe6 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl.loc4
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl.loc4: %F1.type.75d = fn_decl @F1.1 [concrete = constants.%F1.afe] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:     .N = <poisoned>
// CHECK:STDOUT:     .F3 = %F3
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.ref.loc11: %F1.type.75d = name_ref F1, %F1.decl.loc4 [concrete = constants.%F1.afe]
// CHECK:STDOUT:   %F2: %F1.type.75d = bind_alias F2, %F1.decl.loc4 [concrete = constants.%F1.afe]
// CHECK:STDOUT:   %F1.decl.loc18: %F1.type.fe6 = fn_decl @F1.2 [concrete = constants.%F1.cc1] {} {}
// CHECK:STDOUT:   %N.ref: <namespace> = name_ref N, %N [concrete = %N]
// CHECK:STDOUT:   %F1.ref.loc25: <error> = name_ref F1, <error> [concrete = <error>]
// CHECK:STDOUT:   %F3: <error> = bind_alias F3, <error> [concrete = <error>]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.1();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.2();
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_multiple_scopes.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type.75d: type = fn_type @F1.1 [concrete]
// CHECK:STDOUT:   %F1.afe: %F1.type.75d = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.1f8: type = fn_type @F1.2 [concrete]
// CHECK:STDOUT:   %F1.19a: %F1.type.1f8 = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.73a: type = fn_type @F1.3 [concrete]
// CHECK:STDOUT:   %F1.727: %F1.type.73a = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.fe3: type = fn_type @F1.4 [concrete]
// CHECK:STDOUT:   %F1.6a8: %F1.type.fe3 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl.loc4
// CHECK:STDOUT:     .N1 = %N1
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl.loc4: %F1.type.75d = fn_decl @F1.1 [concrete = constants.%F1.afe] {} {}
// CHECK:STDOUT:   %N1: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N3: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.ref: %F1.type.75d = name_ref F1, %F1.decl.loc4 [concrete = constants.%F1.afe]
// CHECK:STDOUT:   %F2: %F1.type.75d = bind_alias F2, %F1.decl.loc4 [concrete = constants.%F1.afe]
// CHECK:STDOUT:   %F1.decl.loc25: %F1.type.1f8 = fn_decl @F1.2 [concrete = constants.%F1.19a] {} {}
// CHECK:STDOUT:   %F1.decl.loc33: %F1.type.73a = fn_decl @F1.3 [concrete = constants.%F1.727] {} {}
// CHECK:STDOUT:   %F1.decl.loc38: %F1.type.fe3 = fn_decl @F1.4 [concrete = constants.%F1.6a8] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.1();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.2();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.3();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.4();
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_alias.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F.type: type = fn_type @F [concrete]
// CHECK:STDOUT:   %F: %F.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F = %F.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [concrete = constants.%F] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F.ref: %F.type = name_ref F, %F.decl [concrete = constants.%F]
// CHECK:STDOUT:   %F: %F.type = bind_alias F, %F.decl [concrete = constants.%F]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F();
// CHECK:STDOUT:
// CHECK:STDOUT: --- ignored_poison_in_import.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [concrete]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.F1 = import_ref Main//poison, F1, unloaded
// CHECK:STDOUT:   %Main.N: <namespace> = import_ref Main//poison, N, loaded
// CHECK:STDOUT:   %N: <namespace> = namespace %Main.N, [concrete] {
// CHECK:STDOUT:     .F2 = %Main.F2
// CHECK:STDOUT:     .F1 = file.%F1.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = imports.%Main.F1
// CHECK:STDOUT:     .N = imports.%N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [concrete = constants.%F1] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1();
// CHECK:STDOUT:
// CHECK:STDOUT: --- poison.impl.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [concrete]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.F1 = import_ref Main//poison, F1, unloaded
// CHECK:STDOUT:   %Main.N: <namespace> = import_ref Main//poison, N, loaded
// CHECK:STDOUT:   %N: <namespace> = namespace %Main.N, [concrete] {
// CHECK:STDOUT:     .F2 = %Main.F2
// CHECK:STDOUT:     .F1 = file.%F1.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = imports.%Main.F1
// CHECK:STDOUT:     .N = imports.%N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import.loc2_6.1 = import <none>
// CHECK:STDOUT:   %default.import.loc2_6.2 = import <none>
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [concrete = constants.%F1] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_when_lookup_fails.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type.75d: type = fn_type @F1.1 [concrete]
// CHECK:STDOUT:   %F1.afe: %F1.type.75d = struct_value () [concrete]
// CHECK:STDOUT:   %F1.type.fe6: type = fn_type @F1.2 [concrete]
// CHECK:STDOUT:   %F1.cc1: %F1.type.fe6 = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = <poisoned>
// CHECK:STDOUT:     .F2 = %F2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.ref: <error> = name_ref F1, <error> [concrete = <error>]
// CHECK:STDOUT:   %F2: <error> = bind_alias F2, <error> [concrete = <error>]
// CHECK:STDOUT:   %F1.decl.loc23: %F1.type.75d = fn_decl @F1.1 [concrete = constants.%F1.afe] {} {}
// CHECK:STDOUT:   %F1.decl.loc28: %F1.type.fe6 = fn_decl @F1.2 [concrete = constants.%F1.cc1] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.1();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1.2();
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_with_lexical_result.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [concrete]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [concrete]
// CHECK:STDOUT:   %F2.type.028: type = fn_type @F2.1 [concrete]
// CHECK:STDOUT:   %F2.18a: %F2.type.028 = struct_value () [concrete]
// CHECK:STDOUT:   %C: type = class_type @C [concrete]
// CHECK:STDOUT:   %F2.type.c9e: type = fn_type @F2.2 [concrete]
// CHECK:STDOUT:   %F2.c1c: %F2.type.c9e = struct_value () [concrete]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [concrete]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [concrete = constants.%F1] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %F2.ref: %F2.type.028 = name_ref F2, @F1.%F2.decl [concrete = constants.%F2.18a]
// CHECK:STDOUT:   %F3: %F2.type.028 = bind_alias F3, @F1.%F2.decl [concrete = constants.%F2.18a]
// CHECK:STDOUT:   %F2.decl: %F2.type.c9e = fn_decl @F2.2 [concrete = constants.%F2.c1c] {} {}
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [concrete = constants.%complete_type]
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   .F2 = <poisoned>
// CHECK:STDOUT:   .F3 = %F3
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %F2.decl: %F2.type.028 = fn_decl @F2.1 [concrete = constants.%F2.18a] {} {}
// CHECK:STDOUT:   %C.decl: type = class_decl @C [concrete = constants.%C] {} {}
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F2.1();
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F2.2();
// CHECK:STDOUT:
