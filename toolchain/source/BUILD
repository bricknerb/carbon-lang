# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

load("//bazel/cc_rules:defs.bzl", "cc_library", "cc_test")

package(default_visibility = ["//visibility:public"])

cc_library(
    name = "source_buffer",
    srcs = ["source_buffer.cpp"],
    hdrs = ["source_buffer.h"],
    deps = [
        "//common:error",
        "//toolchain/diagnostics:diagnostic_emitter",
        "//toolchain/diagnostics:file_diagnostics",
        "//toolchain/diagnostics:format_providers",
        "@llvm-project//llvm:Support",
    ],
)

cc_test(
    name = "source_buffer_test",
    size = "small",
    srcs = ["source_buffer_test.cpp"],
    deps = [
        ":source_buffer",
        "//common:check",
        "//testing/base:gtest_main",
        "//toolchain/diagnostics:diagnostic_emitter",
        "@googletest//:gtest",
        "@llvm-project//llvm:Support",
    ],
)
