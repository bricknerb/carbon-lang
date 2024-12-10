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

class C {};

// Both N.F1 and N.F2 use N.C and not C.
namespace N;
class N.C {}
fn N.F1(x: C);
fn N.F2(x: C) { N.F1(x); }

// --- poison.carbon

library "[[@TEST_NAME]]";

class C {};

namespace N;
// Here we use C and poison N.C.
fn N.F1(x: C);

// --- fail_poison_without_usage.carbon

library "[[@TEST_NAME]]";

class C {};

namespace N;
// Here we use C and poison N.C.
fn N.F1(x: C);

// Should fail here since C was poisoned for namespace N when it was used in N
// context without qualification.
// CHECK:STDERR: fail_poison_without_usage.carbon:[[@LINE+4]]:1: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
// CHECK:STDERR: class N.C {}
// CHECK:STDERR: ^~~~~~~~~~~
// CHECK:STDERR:
class N.C {}

// --- fail_use_undefined_poisoned_name.carbon

library "[[@TEST_NAME]]";

class C {};

namespace N;
// Here we use C and poison N.C.
fn N.F1() -> C;

// Try to use N.C which was never defined and poisoned.
// CHECK:STDERR: fail_use_undefined_poisoned_name.carbon:[[@LINE+4]]:14: error: name `C` not found [NameNotFound]
// CHECK:STDERR: fn N.F2() -> N.C;
// CHECK:STDERR:              ^~~
// CHECK:STDERR:
fn N.F2() -> N.C;

// --- fail_poison_with_usage.carbon

library "[[@TEST_NAME]]";

class C {};

namespace N;
// Here we use C and poison N.C.
fn N.F1(x: C);

// Should fail here since C was poisoned for namespace N when it was used in N
// context without qualification.
// CHECK:STDERR: fail_poison_with_usage.carbon:[[@LINE+4]]:1: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
// CHECK:STDERR: class N.C {}
// CHECK:STDERR: ^~~~~~~~~~~
// CHECK:STDERR:
class N.C {}

// Should not fail here since both N.F2() and N.F1() input is the class C and
// not class N.C.
fn N.F2(x: C) { N.F1(x); }

// --- fail_poison_multiple_scopes.carbon

library "[[@TEST_NAME]]";

class C {};

namespace N1;
namespace N1.N2;
namespace N1.N2.N3;
class N1.N2.N3.D1 {
  class D2 {
    class D3 {
      // Here we use C and poison:
      // * N1.C
      // * N1.N2.C
      // * N1.N2.N3.C
      // * N1.N2.N3.D1.C
      // * N1.N2.N3.D1.D2.C
      // * N1.N2.N3.D1.D2.D3.C
      fn F(x: C);

      // CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:7: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
      // CHECK:STDERR:       class C {}
      // CHECK:STDERR:       ^~~~~~~~~
      // CHECK:STDERR:
      class C {}
    }
    // CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:5: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
    // CHECK:STDERR:     class C {}
    // CHECK:STDERR:     ^~~~~~~~~
    // CHECK:STDERR:
    class C {}
  }
  // CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:3: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
  // CHECK:STDERR:   class C {}
  // CHECK:STDERR:   ^~~~~~~~~
  // CHECK:STDERR:
  class C {}
}

// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:1: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
// CHECK:STDERR: class N1.C {}
// CHECK:STDERR: ^~~~~~~~~~~~
// CHECK:STDERR:
class N1.C {}

// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:1: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
// CHECK:STDERR: class N1.N2.C {}
// CHECK:STDERR: ^~~~~~~~~~~~~~~
// CHECK:STDERR:
class N1.N2.C {}

// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:1: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
// CHECK:STDERR: class N1.N2.N3.C {}
// CHECK:STDERR: ^~~~~~~~~~~~~~~~~~
// CHECK:STDERR:
class N1.N2.N3.C {}

// --- fail_alias.carbon

library "[[@TEST_NAME]]";

class C {}

namespace N;
// CHECK:STDERR: fail_alias.carbon:[[@LINE+3]]:9: error: cannot declare this name in this scope since is was already used without qualification in the context of this scope [NameDeclPoisoned]
// CHECK:STDERR: alias N.C = C;
// CHECK:STDERR:         ^
alias N.C = C;

// --- ignored_poison_in_import.carbon

library "[[@TEST_NAME]]";
import library "poison";

// This doesn't fail.
class N.C {}

// --- poison.impl.carbon

impl library "[[@TEST_NAME]]";

// TODO: This should fail since N.C was poisoned in the api.
class N.C {}

// CHECK:STDOUT: --- no_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C.1: type = class_type @C.1 [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT:   %C.2: type = class_type @C.2 [template]
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [template]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [template]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [template]
// CHECK:STDOUT:   %F2.type: type = fn_type @F2 [template]
// CHECK:STDOUT:   %F2: %F2.type = struct_value () [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl.loc4
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl.loc4: type = class_decl @C.1 [template = constants.%C.1] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl.loc8
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:     .F2 = %F2.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl.loc8: type = class_decl @C.2 [template = constants.%C.2] {} {}
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [template = constants.%F1] {
// CHECK:STDOUT:     %x.patt: %C.2 = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C.2 = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl.loc8 [template = constants.%C.2]
// CHECK:STDOUT:     %x.param: %C.2 = value_param runtime_param0
// CHECK:STDOUT:     %x: %C.2 = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F2.decl: %F2.type = fn_decl @F2 [template = constants.%F2] {
// CHECK:STDOUT:     %x.patt: %C.2 = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C.2 = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl.loc8 [template = constants.%C.2]
// CHECK:STDOUT:     %x.param: %C.2 = value_param runtime_param0
// CHECK:STDOUT:     %x: %C.2 = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C.1 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C.1
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C.2 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C.2
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1(%x.param_patt: %C.2);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F2(%x.param_patt: %C.2) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %N.ref: <namespace> = name_ref N, file.%N [template = file.%N]
// CHECK:STDOUT:   %F1.ref: %F1.type = name_ref F1, file.%F1.decl [template = constants.%F1]
// CHECK:STDOUT:   %x.ref: %C.2 = name_ref x, %x
// CHECK:STDOUT:   %F1.call: init %empty_tuple.type = call %F1.ref(%x.ref)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [template]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [template] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [template = constants.%F1] {
// CHECK:STDOUT:     %x.patt: %C = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [template = constants.%C]
// CHECK:STDOUT:     %x.param: %C = value_param runtime_param0
// CHECK:STDOUT:     %x: %C = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1(%x.param_patt: %C);
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_without_usage.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [template]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [template]
// CHECK:STDOUT:   %.1: type = class_type @.1 [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [template] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [template = constants.%F1] {
// CHECK:STDOUT:     %x.patt: %C = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [template = constants.%C]
// CHECK:STDOUT:     %x.param: %C = value_param runtime_param0
// CHECK:STDOUT:     %x: %C = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.decl: type = class_decl @.1 [template = constants.%.1] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.1 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.1
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1(%x.param_patt: %C);
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_use_undefined_poisoned_name.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [template]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [template]
// CHECK:STDOUT:   %F2.type: type = fn_type @F2 [template]
// CHECK:STDOUT:   %F2: %F2.type = struct_value () [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [template] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:     .F2 = %F2.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [template = constants.%F1] {
// CHECK:STDOUT:     %return.patt: %C = return_slot_pattern
// CHECK:STDOUT:     %return.param_patt: %C = out_param_pattern %return.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [template = constants.%C]
// CHECK:STDOUT:     %return.param: ref %C = out_param runtime_param0
// CHECK:STDOUT:     %return: ref %C = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F2.decl: %F2.type = fn_decl @F2 [template = constants.%F2] {
// CHECK:STDOUT:     %return.patt: <error> = return_slot_pattern
// CHECK:STDOUT:     %return.param_patt: <error> = out_param_pattern %return.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %N.ref: <namespace> = name_ref N, file.%N [template = file.%N]
// CHECK:STDOUT:     %C.ref: <error> = name_ref C, <error> [template = <error>]
// CHECK:STDOUT:     %return.param: ref <error> = out_param runtime_param0
// CHECK:STDOUT:     %return: ref <error> = return_slot %return.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1() -> %C;
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F2() -> <error>;
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_with_usage.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT:   %F1.type: type = fn_type @F1 [template]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [template]
// CHECK:STDOUT:   %F1: %F1.type = struct_value () [template]
// CHECK:STDOUT:   %.1: type = class_type @.1 [template]
// CHECK:STDOUT:   %F2.type: type = fn_type @F2 [template]
// CHECK:STDOUT:   %F2: %F2.type = struct_value () [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [template] {
// CHECK:STDOUT:     .F1 = %F1.decl
// CHECK:STDOUT:     .F2 = %F2.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %F1.decl: %F1.type = fn_decl @F1 [template = constants.%F1] {
// CHECK:STDOUT:     %x.patt: %C = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [template = constants.%C]
// CHECK:STDOUT:     %x.param: %C = value_param runtime_param0
// CHECK:STDOUT:     %x: %C = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.decl: type = class_decl @.1 [template = constants.%.1] {} {}
// CHECK:STDOUT:   %F2.decl: %F2.type = fn_decl @F2 [template = constants.%F2] {
// CHECK:STDOUT:     %x.patt: %C = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [template = constants.%C]
// CHECK:STDOUT:     %x.param: %C = value_param runtime_param0
// CHECK:STDOUT:     %x: %C = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.1 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.1
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F1(%x.param_patt: %C);
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F2(%x.param_patt: %C) {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %N.ref: <namespace> = name_ref N, file.%N [template = file.%N]
// CHECK:STDOUT:   %F1.ref: %F1.type = name_ref F1, file.%F1.decl [template = constants.%F1]
// CHECK:STDOUT:   %x.ref: %C = name_ref x, %x
// CHECK:STDOUT:   %F1.call: init %empty_tuple.type = call %F1.ref(%x.ref)
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_multiple_scopes.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT:   %D1: type = class_type @D1 [template]
// CHECK:STDOUT:   %D2: type = class_type @D2 [template]
// CHECK:STDOUT:   %D3: type = class_type @D3 [template]
// CHECK:STDOUT:   %F.type: type = fn_type @F [template]
// CHECK:STDOUT:   %F: %F.type = struct_value () [template]
// CHECK:STDOUT:   %.1: type = class_type @.1 [template]
// CHECK:STDOUT:   %.2: type = class_type @.2 [template]
// CHECK:STDOUT:   %.3: type = class_type @.3 [template]
// CHECK:STDOUT:   %.4: type = class_type @.4 [template]
// CHECK:STDOUT:   %.5: type = class_type @.5 [template]
// CHECK:STDOUT:   %.6: type = class_type @.6 [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .N1 = %N1
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT:   %N1: <namespace> = namespace [template] {
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N2: <namespace> = namespace [template] {
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N3: <namespace> = namespace [template] {
// CHECK:STDOUT:     .D1 = %D1.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %D1.decl: type = class_decl @D1 [template = constants.%D1] {} {}
// CHECK:STDOUT:   %.decl.loc44: type = class_decl @.4 [template = constants.%.4] {} {}
// CHECK:STDOUT:   %.decl.loc50: type = class_decl @.5 [template = constants.%.5] {} {}
// CHECK:STDOUT:   %.decl.loc56: type = class_decl @.6 [template = constants.%.6] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @D1 {
// CHECK:STDOUT:   %D2.decl: type = class_decl @D2 [template = constants.%D2] {} {}
// CHECK:STDOUT:   %.decl: type = class_decl @.3 [template = constants.%.3] {} {}
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%D1
// CHECK:STDOUT:   .D2 = %D2.decl
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @D2 {
// CHECK:STDOUT:   %D3.decl: type = class_decl @D3 [template = constants.%D3] {} {}
// CHECK:STDOUT:   %.decl: type = class_decl @.2 [template = constants.%.2] {} {}
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%D2
// CHECK:STDOUT:   .D3 = %D3.decl
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @D3 {
// CHECK:STDOUT:   %F.decl: %F.type = fn_decl @F [template = constants.%F] {
// CHECK:STDOUT:     %x.patt: %C = binding_pattern x
// CHECK:STDOUT:     %x.param_patt: %C = value_param_pattern %x.patt, runtime_param0
// CHECK:STDOUT:   } {
// CHECK:STDOUT:     %C.ref: type = name_ref C, file.%C.decl [template = constants.%C]
// CHECK:STDOUT:     %x.param: %C = value_param runtime_param0
// CHECK:STDOUT:     %x: %C = bind_name x, %x.param
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %.decl: type = class_decl @.1 [template = constants.%.1] {} {}
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%D3
// CHECK:STDOUT:   .F = %F.decl
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.1 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.1
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.2 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.2
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.3 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.3
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.4 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.4
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.5 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.5
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @.6 {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%.6
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @F(%x.param_patt: %C);
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_alias.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = %C.decl
// CHECK:STDOUT:     .N = %N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT:   %N: <namespace> = namespace [template] {}
// CHECK:STDOUT:   %C.ref: type = name_ref C, %C.decl [template = constants.%C]
// CHECK:STDOUT:   %.loc10: type = bind_alias <invalid>, %C.decl [template = constants.%C]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- ignored_poison_in_import.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %import_ref.1 = import_ref Main//poison, C, unloaded
// CHECK:STDOUT:   %import_ref.2: <namespace> = import_ref Main//poison, N, loaded
// CHECK:STDOUT:   %N: <namespace> = namespace %import_ref.2, [template] {
// CHECK:STDOUT:     .F1 = %import_ref.3
// CHECK:STDOUT:     .C = file.%C.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = imports.%import_ref.1
// CHECK:STDOUT:     .N = imports.%N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <invalid>
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- poison.impl.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %C: type = class_type @C [template]
// CHECK:STDOUT:   %empty_struct_type: type = struct_type {} [template]
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %import_ref.1 = import_ref Main//poison, C, unloaded
// CHECK:STDOUT:   %import_ref.2: <namespace> = import_ref Main//poison, N, loaded
// CHECK:STDOUT:   %N: <namespace> = namespace %import_ref.2, [template] {
// CHECK:STDOUT:     .F1 = %import_ref.3
// CHECK:STDOUT:     .C = file.%C.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [template] {
// CHECK:STDOUT:     .C = imports.%import_ref.1
// CHECK:STDOUT:     .N = imports.%N
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import.loc2_6.1 = import <invalid>
// CHECK:STDOUT:   %default.import.loc2_6.2 = import <invalid>
// CHECK:STDOUT:   %C.decl: type = class_decl @C [template = constants.%C] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: class @C {
// CHECK:STDOUT:   %complete_type: <witness> = complete_type_witness %empty_struct_type [template = constants.%complete_type]
// CHECK:STDOUT:
// CHECK:STDOUT: !members:
// CHECK:STDOUT:   .Self = constants.%C
// CHECK:STDOUT:   complete_type_witness = %complete_type
// CHECK:STDOUT: }
// CHECK:STDOUT:
