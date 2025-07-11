# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

load(
    "@llvm-project//:vars.bzl",
    "LLVM_VERSION_MAJOR",
)
load("@rules_python//python:defs.bzl", "py_test")
load("//bazel/cc_rules:defs.bzl", "cc_binary", "cc_library", "cc_test")
load("//bazel/manifest:defs.bzl", "manifest")
load("//toolchain/base:llvm_tools.bzl", "LLVM_MAIN_TOOLS", "LLVM_TOOL_ALIASES")
load("install_filegroups.bzl", "install_filegroup", "install_symlink", "install_target", "make_install_filegroups")
load("pkg_helpers.bzl", "pkg_naming_variables", "pkg_tar_and_test")

package(default_visibility = ["//visibility:public"])

# Build rules supporting the install data tree for the Carbon toolchain.
#
# This populates a synthetic Carbon toolchain installation under the
# `prefix_root` directory. For details on its layout, see `install_dirs` below.

# A library for computing install paths for the toolchain. Note that this
# library does *not* include the data itself, as that would form a dependency
# cycle. Each part of the toolchain should add the narrow data file groups to
# their data dependencies, and then use this library to locate them.
cc_library(
    name = "install_paths",
    srcs = ["install_paths.cpp"],
    hdrs = ["install_paths.h"],
    deps = [
        "//common:check",
        "//common:error",
        "//toolchain/base:llvm_tools",
        "@bazel_tools//tools/cpp/runfiles",
        "@llvm-project//llvm:Support",
    ],
)

cc_binary(
    name = "test_binary",
    testonly = 1,
    srcs = ["test_binary.cpp"],
    data = [":install_data"],
)

cc_test(
    name = "install_paths_test",
    size = "small",
    srcs = ["install_paths_test.cpp"],
    data = [
        ":install_data",
        ":test_binary",
    ],
    deps = [
        ":install_paths",
        "//common:check",
        "//common:ostream",
        "//testing/base:global_exe_path",
        "//testing/base:gtest_main",
        "@bazel_tools//tools/cpp/runfiles",
        "@googletest//:gtest",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "install_paths_test_helpers",
    testonly = 1,
    srcs = ["install_paths_test_helpers.cpp"],
    hdrs = ["install_paths_test_helpers.h"],
    deps = [
        ":install_paths",
        "//testing/base:global_exe_path",
        "@llvm-project//llvm:Support",
    ],
)

cc_library(
    name = "busybox_info",
    srcs = ["busybox_info.cpp"],
    hdrs = ["busybox_info.h"],
    deps = [
        "//common:error",
        "//common:exe_path",
        "@llvm-project//llvm:Support",
    ],
)

cc_test(
    name = "busybox_info_test",
    size = "small",
    srcs = ["busybox_info_test.cpp"],
    deps = [
        ":busybox_info",
        "//common:check",
        "//testing/base:gtest_main",
        "@googletest//:gtest",
        "@llvm-project//llvm:Support",
    ],
)

# This target doesn't include prelude libraries. To get a target that has the
# prelude available, use //toolchain.
cc_binary(
    name = "carbon-busybox",
    srcs = ["busybox_main.cpp"],
    deps = [
        ":busybox_info",
        ":install_paths",
        "//common:all_llvm_targets",
        "//common:bazel_working_dir",
        "//common:error",
        "//common:exe_path",
        "//common:init_llvm",
        "//common:version_stamp",
        "//toolchain/base:llvm_tools_def",
        "//toolchain/driver",
        "@llvm-project//llvm:Support",
    ],
)

clang_aliases = [
    "clang",
    "clang++",
    "clang-cl",
    "clang-cpp",
]

# TODO: Add remaining aliases of LLD for Windows and WASM when we have support
# for them wired up through the busybox.
lld_aliases = [
    "ld.lld",
    "ld64.lld",
]

llvm_binaries = clang_aliases + lld_aliases + [
    tool.bin_name
    for tool in LLVM_MAIN_TOOLS.values()
] + [
    "llvm-" + alias
    for (_, aliases) in LLVM_TOOL_ALIASES.items()
    for alias in aliases
]

filegroup(
    name = "clang_headers",
    srcs = ["@llvm-project//clang:builtin_headers_gen"],
)

# Given a root `prefix_root`, the hierarchy looks like:
#
# - prefix_root/bin: Binaries intended for direct use.
# - prefix_root/lib/carbon: Private data and files.
# - prefix_root/lib/carbon/core: The `Core` package files.
# - prefix_root/lib/carbon/llvm/bin: LLVM binaries.
#
# This will be how installs are provided on Unix-y platforms, and is loosely
# based on the FHS (Filesystem Hierarchy Standard).
install_dirs = {
    "bin": [
        install_symlink(
            "carbon",
            "../lib/carbon/carbon-busybox",
            is_driver = True,
        ),
    ],
    "lib/carbon": [
        install_target("carbon_install.txt", "carbon_install.txt"),
        install_target(
            "carbon-busybox",
            ":carbon-busybox",
            executable = True,
            is_driver = True,
        ),
        install_filegroup("core", "//core:prelude"),
    ],
    "lib/carbon/llvm/bin": [install_symlink(
        name,
        "../../carbon-busybox",
        is_driver = True,
    ) for name in llvm_binaries],
    "lib/carbon/llvm/lib/clang/" + LLVM_VERSION_MAJOR: [
        install_filegroup(
            "include",
            ":clang_headers",
            label = "installed_clang_headers",
            remove_prefix = "staging/include/",
        ),
    ],
}

make_install_filegroups(
    name = "install_data",
    install_dirs = install_dirs,
    no_driver_name = "install_data.no_driver",
    pkg_name = "pkg_data",
    prefix = "prefix_root",
)

py_test(
    name = "llvm_symlinks_test",
    size = "small",
    srcs = ["llvm_symlinks_test.py"],
    data = [":install_data"],
)

manifest(
    name = "install_data_manifest.txt",
    srcs = [":install_data"],
)

# A list of clang's installed builtin header files.
# This is consumed by //toolchain/testing:file_test.
manifest(
    name = "clang_headers_manifest.txt",
    srcs = [":installed_clang_headers"],
    strip_package_dir = True,
)

pkg_naming_variables(
    name = "packaging_variables",
)

# We build both a compressed and uncompressed tar file with the same code here.
# This lets us use the tar file in testing as it is fast to create, but ship the
# compressed version as a release.
#
# For manual tests, the tar rules are `carbon_toolchain_tar_rule` and
# `carbon_toolchain_tar_gz_rule`.
pkg_tar_and_test(
    srcs = [":pkg_data"],
    install_data_manifest = ":install_data_manifest.txt",
    name_base = "carbon_toolchain",
    package_dir = "carbon_toolchain-$(version)",
    package_file_name_base = "carbon_toolchain-$(version)",
    package_variables = ":packaging_variables",
    stamp = -1,  # Allow `--stamp` builds to produce file timestamps.
)
