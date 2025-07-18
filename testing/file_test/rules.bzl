# Part of the Carbon Language project, under the Apache License v2.0 with LLVM
# Exceptions. See /LICENSE for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

"""Rules for building file tests.

file_test uses the tests_as_input_file rule to transform test dependencies into
a file which can be accessed as a list. This avoids long argument parsing.
"""

load("//bazel/cc_rules:defs.bzl", "cc_test")
load("//bazel/manifest:defs.bzl", "manifest_as_cpp")

def file_test(
        name,
        tests,
        srcs = [],
        deps = [],
        data = [],
        args = [],
        **kwargs):
    """Generates tests using the file_test base.

    There will be one main test using `name` that can be sharded, and includes
    all files. Additionally, per-file tests will be generated as
    `name.file_path`; these per-file tests will be manual.

    Args:
      name: The base name of the tests.
      tests: The list of test files to use as data, typically a glob.
      srcs: Passed to cc_test.
      deps: Passed to cc_test.
      data: Passed to cc_test.
      args: Passed to cc_test.
      **kwargs: Passed to cc_test.
    """

    # Ensure tests are always a filegroup for tests_as_input_file_rule.
    manifest_cpp = "{0}_autogen_manifest.cpp".format(name)
    manifest_as_cpp(
        name = manifest_cpp,
        var_name = "CarbonFileTestManifest",
        srcs = tests,
        testonly = 1,
    )
    cc_test(
        name = name,
        srcs = srcs + [":" + manifest_cpp],
        deps = deps + ["//testing/file_test:manifest_impl"],
        data = tests + data,
        args = args,
        **kwargs
    )
