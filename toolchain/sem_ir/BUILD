# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

load("//bazel/cc_rules:defs.bzl", "cc_library", "cc_test")

package(default_visibility = ["//visibility:public"])

cc_library(
    name = "typed_insts",
    srcs = [
        "ids.cpp",
        "inst_kind.cpp",
    ],
    hdrs = [
        "id_kind.h",
        "ids.h",
        "inst_categories.h",
        "inst_kind.h",
        "singleton_insts.h",
        "specific_interface.h",
        "typed_insts.h",
    ],
    textual_hdrs = ["inst_kind.def"],
    deps = [
        "//common:check",
        "//common:enum_base",
        "//common:ostream",
        "//common:type_enum",
        "//toolchain/base:canonical_value_store",
        "//toolchain/base:index_base",
        "//toolchain/base:int",
        "//toolchain/base:value_ids",
        "//toolchain/diagnostics:diagnostic_emitter",
        "//toolchain/parse:node_kind",
        "@llvm-project//llvm:Support",
    ],
)

cc_test(
    name = "ids_test",
    srcs = ["ids_test.cpp"],
    deps = [
        ":typed_insts",
        "//testing/base:gtest_main",
        "@googletest//:gtest",
    ],
)

cc_library(
    name = "clang_decl",
    srcs = ["clang_decl.cpp"],
    hdrs = ["clang_decl.h"],
    deps = [
        ":typed_insts",
        "//common:hashtable_key_context",
        "//common:ostream",
        "//toolchain/base:canonical_value_store",
        "@llvm-project//clang:ast",
    ],
)

cc_library(
    name = "file",
    srcs = [
        "builtin_function_kind.cpp",
        "class.cpp",
        "constant.cpp",
        "facet_type_info.cpp",
        "file.cpp",
        "function.cpp",
        "generic.cpp",
        "impl.cpp",
        "import_ir.cpp",
        "inst.cpp",
        "name.cpp",
        "name_scope.cpp",
        "pattern.cpp",
        "type.cpp",
        "type_info.cpp",
        "type_iterator.cpp",
    ],
    hdrs = [
        "associated_constant.h",
        "builtin_function_kind.h",
        "class.h",
        "constant.h",
        "copy_on_write_block.h",
        "entity_name.h",
        "entity_with_params_base.h",
        "facet_type_info.h",
        "file.h",
        "function.h",
        "generic.h",
        "impl.h",
        "import_cpp.h",
        "import_ir.h",
        "inst.h",
        "interface.h",
        "name.h",
        "name_scope.h",
        "pattern.h",
        "struct_type_field.h",
        "type.h",
        "type_info.h",
        "type_iterator.h",
        "vtable.h",
    ],
    textual_hdrs = [
        "builtin_function_kind.def",
    ],
    deps = [
        ":clang_decl",
        ":typed_insts",
        "//common:check",
        "//common:enum_base",
        "//common:error",
        "//common:hashing",
        "//common:map",
        "//common:ostream",
        "//common:raw_string_ostream",
        "//common:set",
        "//common:struct_reflection",
        "//toolchain/base:block_value_store",
        "//toolchain/base:canonical_value_store",
        "//toolchain/base:index_base",
        "//toolchain/base:int",
        "//toolchain/base:kind_switch",
        "//toolchain/base:relational_value_store",
        "//toolchain/base:shared_value_stores",
        "//toolchain/base:value_ids",
        "//toolchain/base:value_store",
        "//toolchain/base:yaml",
        "//toolchain/lex:token_kind",
        "//toolchain/parse:node_kind",
        "//toolchain/parse:tree",
        "@llvm-project//clang:ast",
        "@llvm-project//clang:frontend",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "stringify",
    srcs = ["stringify.cpp"],
    hdrs = ["stringify.h"],
    deps = [
        ":file",
        ":typed_insts",
        "//common:check",
        "//common:concepts",
        "//common:raw_string_ostream",
        "//toolchain/base:kind_switch",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "inst_namer",
    srcs = [
        "inst_fingerprinter.cpp",
        "inst_namer.cpp",
    ],
    hdrs = [
        "inst_fingerprinter.h",
        "inst_namer.h",
    ],
    deps = [
        ":file",
        ":typed_insts",
        "//common:concepts",
        "//common:ostream",
        "//common:raw_string_ostream",
        "//common:type_enum",
        "//toolchain/base:kind_switch",
        "//toolchain/base:shared_value_stores",
        "//toolchain/base:value_ids",
        "//toolchain/lex:tokenized_buffer",
        "//toolchain/parse:tree",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "formatter",
    srcs = ["formatter.cpp"],
    hdrs = ["formatter.h"],
    deps = [
        ":expr_info",
        ":file",
        ":inst_namer",
        ":typed_insts",
        "//common:concepts",
        "//common:ostream",
        "//toolchain/base:fixed_size_value_store",
        "//toolchain/base:kind_switch",
        "//toolchain/base:shared_value_stores",
        "//toolchain/lex:tokenized_buffer",
        "//toolchain/parse:tree",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "entry_point",
    srcs = ["entry_point.cpp"],
    hdrs = ["entry_point.h"],
    deps = [
        ":file",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "absolute_node_id",
    srcs = ["absolute_node_id.cpp"],
    hdrs = ["absolute_node_id.h"],
    deps = [
        ":file",
        ":typed_insts",
        "//toolchain/parse:tree",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "expr_info",
    srcs = ["expr_info.cpp"],
    hdrs = ["expr_info.h"],
    deps = [
        ":file",
        ":typed_insts",
        "//common:check",
        "//toolchain/base:kind_switch",
    ],
)

cc_library(
    name = "diagnostic_loc_converter",
    srcs = ["diagnostic_loc_converter.cpp"],
    hdrs = ["diagnostic_loc_converter.h"],
    deps = [
        ":absolute_node_id",
        ":file",
        ":typed_insts",
        "//toolchain/diagnostics:diagnostic_emitter",
        "//toolchain/parse:tree",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "dump",
    srcs = ["dump.cpp"],
    hdrs = ["dump.h"],
    deps = [
        ":file",
        ":stringify",
        "//common:ostream",
        "//common:raw_string_ostream",
    ],
    # Always link dump methods so they are callable from a debugger
    # even though they are never called.
    alwayslink = 1,
)

cc_test(
    name = "name_scope_test",
    size = "small",
    srcs = ["name_scope_test.cpp"],
    deps = [
        ":file",
        "//testing/base:gtest_main",
        "@googletest//:gtest",
    ],
)

cc_test(
    name = "typed_insts_test",
    size = "small",
    srcs = ["typed_insts_test.cpp"],
    deps = [
        ":file",
        ":typed_insts",
        "//testing/base:gtest_main",
        "@googletest//:gtest",
    ],
)

cc_test(
    name = "yaml_test",
    size = "small",
    srcs = ["yaml_test.cpp"],
    deps = [
        "//common:all_llvm_targets",
        "//common:ostream",
        "//common:raw_string_ostream",
        "//testing/base:global_exe_path",
        "//testing/base:gtest_main",
        "//toolchain/driver",
        "//toolchain/testing:yaml_test_helpers",
        "@googletest//:gtest",
        "@llvm-project//llvm:Support",
    ],
)
