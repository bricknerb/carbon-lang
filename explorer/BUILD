# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")
load("//bazel/cc_toolchains:defs.bzl", "cc_env")
load("//testing/file_test:rules.bzl", "file_test")

package(default_visibility = [
    "//bazel/check_deps:__pkg__",
    "//explorer:__subpackages__",
    "//installers:__subpackages__",
])

filegroup(
    name = "standard_libraries",
    srcs = ["data/prelude.carbon"],
)

cc_library(
    name = "main",
    srcs = ["main.cpp"],
    hdrs = ["main.h"],
    # Running clang-tidy is slow, and explorer is currently feature frozen, so
    # don't spend time linting it.
    tags = ["no-clang-tidy"],
    deps = [
        "//common:error",
        "//common:ostream",
        "//explorer/base:trace_stream",
        "//explorer/parse_and_execute",
        "@llvm-project//llvm:Support",
    ],
)

cc_binary(
    name = "explorer",
    srcs = ["main_bin.cpp"],
    env = cc_env(),
    # Running clang-tidy is slow, and explorer is currently feature frozen, so
    # don't spend time linting it.
    tags = ["no-clang-tidy"],
    deps = [
        ":main",
        "//common:bazel_working_dir",
        "@llvm-project//llvm:Support",
    ],
)

cc_binary(
    name = "file_test",
    testonly = 1,
    srcs = ["file_test.cpp"],
    # Running clang-tidy is slow, and explorer is currently feature frozen, so
    # don't spend time linting it.
    tags = ["no-clang-tidy"],
    deps = [
        ":main",
        "//common:check",
        "//common:raw_string_ostream",
        "//testing/base:file_helpers",
        "//testing/file_test:file_test_base",
        "//testing/file_test:manifest",
        "@abseil-cpp//absl/flags:flag",
        "@abseil-cpp//absl/strings",
        "@re2",
    ],
)

file_test(
    name = "file_test.notrace",
    size = "small",
    prebuilt_binary = ":file_test",
    shard_count = 20,
    tests = glob(["testdata/**/*.carbon"]),
)

file_test(
    name = "file_test.trace",
    size = "small",
    args = ["--trace"],
    prebuilt_binary = ":file_test",
    shard_count = 30,
    tests = glob(
        ["testdata/**/*.carbon"],
        exclude = [
            # `limits` tests check for various limit conditions (such as an
            # infinite loop). The tests collectively don't test tracing
            # because it creates substantial additional overhead.
            "testdata/limits/**",
            # `trace` tests do tracing by default.
            "testdata/trace/**",
            # Expensive tests to trace.
            "testdata/assoc_const/rewrite_large_type.carbon",
            "testdata/linked_list/typed_linked_list.carbon",
        ],
    ),
)

filegroup(
    name = "tree_sitter_testdata",
    srcs = glob(
        ["testdata/**/*.carbon"],
        exclude = [
            "testdata/**/fail_*",
        ],
    ),
    visibility = ["//utils/tree_sitter:__pkg__"],
)
