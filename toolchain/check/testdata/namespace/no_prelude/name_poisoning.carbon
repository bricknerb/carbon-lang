// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/namespace/no_prelude/name_poisoning.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/namespace/no_prelude/name_poisoning.carbon

// --- no_poison.carbon

library "[[@TEST_NAME]]";

namespace N1;

// N2.N3 uses N2.N1 and not N1.
namespace N2;
namespace N2.N1;
alias N2.N3 = N1;

// --- poison.carbon

library "[[@TEST_NAME]]";

namespace N1;

// Use N1 and poison N2.N1.
namespace N2;
alias N2.N3 = N1;

// --- fail_declare_after_poison.carbon

library "[[@TEST_NAME]]";

namespace N1;

// Use N1 and poison N2.N1.
namespace N2;
// CHECK:STDERR: fail_declare_after_poison.carbon:[[@LINE+3]]:15: error: name `N1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N2.N3 = N1;
// CHECK:STDERR:               ^~
alias N2.N3 = N1;

// Failure: N2.N1 declared after it was poisoned.
// CHECK:STDERR: fail_declare_after_poison.carbon:[[@LINE+4]]:14: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N2.N1;
// CHECK:STDERR:              ^~
// CHECK:STDERR:
namespace N2.N1;

// --- fail_use_poison.carbon

library "[[@TEST_NAME]]";

namespace N1;

// Use N1 and poison N2.N1.
namespace N2;
alias N2.N3 = N1;

// CHECK:STDERR: fail_use_poison.carbon:[[@LINE+4]]:15: error: member name `N1` not found in `N2` [MemberNameNotFoundInScope]
// CHECK:STDERR: alias N2.N4 = N2.N1;
// CHECK:STDERR:               ^~~~~
// CHECK:STDERR:
alias N2.N4 = N2.N1;

// --- fail_use_declaration_after_poison.carbon

library "[[@TEST_NAME]]";

namespace N1;

// Use N1 and poison N2.N1.
namespace N2;
// CHECK:STDERR: fail_use_declaration_after_poison.carbon:[[@LINE+3]]:15: error: name `N1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N2.N3 = N1;
// CHECK:STDERR:               ^~
alias N2.N3 = N1;

// Failure: N2.N1 declared after it was poisoned.
// CHECK:STDERR: fail_use_declaration_after_poison.carbon:[[@LINE+4]]:14: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N2.N1;
// CHECK:STDERR:              ^~
// CHECK:STDERR:
namespace N2.N1;

// CHECK:STDERR: fail_use_declaration_after_poison.carbon:[[@LINE+4]]:15: error: member name `N1` not found in `N2` [MemberNameNotFoundInScope]
// CHECK:STDERR: alias N2.N4 = N2.N1;
// CHECK:STDERR:               ^~~~~
// CHECK:STDERR:
alias N2.N4 = N2.N1;

// --- fail_alias.carbon

namespace N1;

namespace N2;
// CHECK:STDERR: fail_alias.carbon:[[@LINE+7]]:15: error: name `N1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N2.N1 = N1;
// CHECK:STDERR:               ^~
// CHECK:STDERR: fail_alias.carbon:[[@LINE+4]]:10: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: alias N2.N1 = N1;
// CHECK:STDERR:          ^~
// CHECK:STDERR:
alias N2.N1 = N1;

// --- fail_poison_multiple_scopes.carbon

library "[[@TEST_NAME]]";

namespace N1;
namespace N2;
namespace N2.N3;
namespace N2.N3.N4;
// Use N1 and poison:
// * N2.N1
// * N2.N3.N1
// * N2.N3.N4.N1
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+3]]:21: error: name `N1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N2.N3.N4.N5 = N1;
// CHECK:STDERR:                     ^~
alias N2.N3.N4.N5 = N1;

// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+7]]:20: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N2.N3.N4.N1;
// CHECK:STDERR:                    ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE-6]]:21: error: name `N1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N2.N3.N4.N5 = N1;
// CHECK:STDERR:                     ^~
namespace N2.N3.N4.N1;
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+7]]:17: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N2.N3.N1;
// CHECK:STDERR:                 ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE-14]]:21: error: name `N1` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N2.N3.N4.N5 = N1;
// CHECK:STDERR:                     ^~
namespace N2.N3.N1;
// CHECK:STDERR: fail_poison_multiple_scopes.carbon:[[@LINE+4]]:14: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N2.N1;
// CHECK:STDERR:              ^~
// CHECK:STDERR:
namespace N2.N1;

// --- ignored_poison_in_import.carbon

library "[[@TEST_NAME]]";
import library "poison";

// This doesn't fail.
namespace N2.N1;

// --- poison.impl.carbon

impl library "[[@TEST_NAME]]";

// TODO: This should fail since N2.N1 was poisoned in the api.
namespace N2.N1;

// --- fail_poison_when_lookup_fails.carbon

library "[[@TEST_NAME]]";

namespace N1;
// N1.N3 poisoned when not found.
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+7]]:15: error: name `N3` not found [NameNotFound]
// CHECK:STDERR: alias N1.N2 = N3;
// CHECK:STDERR:               ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+3]]:15: error: name `N3` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N1.N2 = N3;
// CHECK:STDERR:               ^~
alias N1.N2 = N3;

// TODO: We should ideally only produce one diagnostic here.
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+7]]:11: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N3;
// CHECK:STDERR:           ^~
// CHECK:STDERR:
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE-7]]:15: error: name `N3` used before it was declared [NameUseBeforeDecl]
// CHECK:STDERR: alias N1.N2 = N3;
// CHECK:STDERR:               ^~
namespace N3;
// CHECK:STDERR: fail_poison_when_lookup_fails.carbon:[[@LINE+4]]:14: note: declared here [NameUseBeforeDeclNote]
// CHECK:STDERR: namespace N1.N3;
// CHECK:STDERR:              ^~
// CHECK:STDERR:
namespace N1.N3;

// CHECK:STDOUT: --- no_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1.loc4
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.loc4: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1.loc8
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.loc8: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N1.ref: <namespace> = name_ref N1, %N1.loc8 [concrete = %N1.loc8]
// CHECK:STDOUT:   %N3: <namespace> = bind_alias N3, %N1.loc8 [concrete = %N1.loc8]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.ref: <namespace> = name_ref N1, %N1 [concrete = %N1]
// CHECK:STDOUT:   %N3: <namespace> = bind_alias N3, %N1 [concrete = %N1]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_declare_after_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1.loc4
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.loc4: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.ref: <namespace> = name_ref N1, %N1.loc4 [concrete = %N1.loc4]
// CHECK:STDOUT:   %N3: <namespace> = bind_alias N3, %N1.loc4 [concrete = %N1.loc4]
// CHECK:STDOUT:   %N1.loc18: <namespace> = namespace [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_use_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:     .N2 = <poisoned>
// CHECK:STDOUT:     .N4 = %N4
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.ref.loc8: <namespace> = name_ref N1, %N1 [concrete = %N1]
// CHECK:STDOUT:   %N3: <namespace> = bind_alias N3, %N1 [concrete = %N1]
// CHECK:STDOUT:   %N2.ref: <namespace> = name_ref N2, %N2 [concrete = %N2]
// CHECK:STDOUT:   %N1.ref.loc14: <error> = name_ref N1, <error> [concrete = <error>]
// CHECK:STDOUT:   %N4: <error> = bind_alias N4, <error> [concrete = <error>]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_use_declaration_after_poison.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1.loc4
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.loc4: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:     .N2 = <poisoned>
// CHECK:STDOUT:     .N4 = %N4
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.ref.loc11: <namespace> = name_ref N1, %N1.loc4 [concrete = %N1.loc4]
// CHECK:STDOUT:   %N3: <namespace> = bind_alias N3, %N1.loc4 [concrete = %N1.loc4]
// CHECK:STDOUT:   %N1.loc18: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2.ref: <namespace> = name_ref N2, %N2 [concrete = %N2]
// CHECK:STDOUT:   %N1.ref.loc24: <error> = name_ref N1, <error> [concrete = <error>]
// CHECK:STDOUT:   %N4: <error> = bind_alias N4, <error> [concrete = <error>]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_alias.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1.loc2
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.loc2: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.ref: <namespace> = name_ref N1, %N1.loc2 [concrete = %N1.loc2]
// CHECK:STDOUT:   %N1.loc12: <namespace> = bind_alias N1, %N1.loc2 [concrete = %N1.loc2]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_multiple_scopes.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1.loc4
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.loc4: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N2: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N3 = %N3
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N3: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N4 = %N4
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N4: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = <poisoned>
// CHECK:STDOUT:     .N5 = %N5
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1.ref: <namespace> = name_ref N1, %N1.loc4 [concrete = %N1.loc4]
// CHECK:STDOUT:   %N5: <namespace> = bind_alias N5, %N1.loc4 [concrete = %N1.loc4]
// CHECK:STDOUT:   %N1.loc24: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N1.loc32: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N1.loc37: <namespace> = namespace [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- ignored_poison_in_import.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.N1: <namespace> = import_ref Main//poison, N1, loaded
// CHECK:STDOUT:   %N1: <namespace> = namespace %Main.N1, [concrete] {}
// CHECK:STDOUT:   %Main.N2: <namespace> = import_ref Main//poison, N2, loaded
// CHECK:STDOUT:   %N2: <namespace> = namespace %Main.N2, [concrete] {
// CHECK:STDOUT:     .N3 = %Main.N3
// CHECK:STDOUT:     .N1 = file.%N1
// CHECK:STDOUT:   }
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = imports.%N1
// CHECK:STDOUT:     .N2 = imports.%N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import = import <none>
// CHECK:STDOUT:   %N1: <namespace> = namespace [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- poison.impl.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %Main.N2: <namespace> = import_ref Main//poison, N2, loaded
// CHECK:STDOUT:   %N2: <namespace> = namespace %Main.N2, [concrete] {
// CHECK:STDOUT:     .N3 = %Main.N3
// CHECK:STDOUT:     .N1 = file.%N1
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %Main.N1: <namespace> = import_ref Main//poison, N1, loaded
// CHECK:STDOUT:   %N1: <namespace> = namespace %Main.N1, [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N2 = imports.%N2
// CHECK:STDOUT:     .N1 = imports.%N1
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %default.import.loc2_6.1 = import <none>
// CHECK:STDOUT:   %default.import.loc2_6.2 = import <none>
// CHECK:STDOUT:   %N1: <namespace> = namespace [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: --- fail_poison_when_lookup_fails.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N1 = %N1
// CHECK:STDOUT:     .N3 = <poisoned>
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N1: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .N3 = <poisoned>
// CHECK:STDOUT:     .N2 = %N2
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %N3.ref: <error> = name_ref N3, <error> [concrete = <error>]
// CHECK:STDOUT:   %N2: <error> = bind_alias N2, <error> [concrete = <error>]
// CHECK:STDOUT:   %N3.loc23: <namespace> = namespace [concrete] {}
// CHECK:STDOUT:   %N3.loc28: <namespace> = namespace [concrete] {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
