# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

load("//bazel/carbon_rules:defs.bzl", "carbon_binary")

utils = [
    "io_utils.carbon",
    "sort.carbon",
]

carbon_binary(
    name = "day1_part1",
    srcs = [
        "day1_common.carbon",
        "day1_part1.carbon",
    ] + utils,
)

carbon_binary(
    name = "day1_part2",
    srcs = [
        "day1_common.carbon",
        "day1_part2.carbon",
    ] + utils,
)
