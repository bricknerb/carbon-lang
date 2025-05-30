# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

load("@bazel_skylib//lib:selects.bzl", "selects")

package(default_visibility = ["//visibility:public"])

# For use by defs.bzl.
# Matches when asan is enabled on a macOS platform.
selects.config_setting_group(
    name = "macos_asan",
    match_all = [
        ":is_macos",
        ":macos_asan_build_modes",
    ],
)

# For use by defs.bzl.
# Matches macOS platforms.
config_setting(
    name = "is_macos",
    constraint_values = ["@platforms//os:macos"],
)

# For use by defs.bzl.
# Matches build modes where asan is enabled.
selects.config_setting_group(
    name = "macos_asan_build_modes",
    match_any = [
        ":dbg",
        ":fastbuild",
    ],
)

# For use by defs.bzl.
# Matches dbg.
config_setting(
    name = "dbg",
    values = {"compilation_mode": "dbg"},
)

# For use by defs.bzl.
# Matches fastbuild.
config_setting(
    name = "fastbuild",
    values = {"compilation_mode": "fastbuild"},
)
