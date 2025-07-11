// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "common/exe_path.h"

#include <gtest/gtest.h>

#include <string>
#include <system_error>

#include "llvm/ADT/SmallString.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

namespace Carbon {
namespace {

TEST(ExePath, FailureFallback) {
  static int static_for_main_addr;
  std::string running_binary =
      llvm::sys::fs::getMainExecutable("exe_path_test", &static_for_main_addr);

  llvm::SmallString<128> path = llvm::StringRef(getenv("TEST_TMPDIR"));
  llvm::sys::path::append(path, "non_existant_binary");
  std::string exe_path = FindExecutablePath(path.c_str());
  EXPECT_EQ(running_binary, exe_path);
}

TEST(ExePath, Symlink) {
  static int static_for_main_addr;
  std::string running_binary =
      llvm::sys::fs::getMainExecutable("exe_path_test", &static_for_main_addr);

  llvm::SmallString<128> path = llvm::StringRef(getenv("TEST_TMPDIR"));
  llvm::sys::path::append(path, "test_binary");
  std::error_code ec;
  std::filesystem::create_symlink(running_binary, path.c_str(), ec);
  ASSERT_TRUE(!ec) << "Error code: " << ec;

  std::string exe_path = FindExecutablePath(path.c_str());
  EXPECT_EQ(path, exe_path);
}

TEST(ExePath, PathLookup) {
  // TODO: This is not likely to work well on Windows (outside of WSL). But some
  // of that may be hidden by Bazel's test environment. Regardless, we should
  // revisit this when we have good coverage of Windows build with something
  // appropriate for that platform.
  std::string exe_path = FindExecutablePath("bash");
  EXPECT_NE(exe_path, "bash");
  EXPECT_TRUE(llvm::sys::fs::exists(exe_path));
  EXPECT_TRUE(llvm::sys::fs::can_execute(exe_path));
}

}  // namespace
}  // namespace Carbon
