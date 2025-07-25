// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/install/busybox_info.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <cstdlib>
#include <fstream>
#include <optional>
#include <system_error>

#include "common/check.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/Support/FileSystem.h"

namespace Carbon {
namespace {

using ::testing::Eq;

class BusyboxInfoTest : public ::testing::Test {
 public:
  explicit BusyboxInfoTest() {
    // Set up a temp directory for the test case.
    const char* tmpdir = std::getenv("TEST_TMPDIR");
    CARBON_CHECK(tmpdir);
    dir_ = MakeDir(std::filesystem::absolute(
        std::filesystem::path(tmpdir) /
        ::testing::UnitTest::GetInstance()->current_test_info()->name()));

    // Most tests need the running binary for `MakeBusyboxFile`.
    static int static_for_main_addr;
    running_binary_ = llvm::sys::fs::getMainExecutable("busybox_info_test",
                                                       &static_for_main_addr);
  }

  // Delete the test case's temp directory.
  ~BusyboxInfoTest() override {
    std::error_code ec;
    std::filesystem::remove_all(dir_, ec);
    CARBON_CHECK(!ec, "error removing {0}: {1}", dir_, ec.message());
  }

  // Creates a stub file. Returns the input file for easier use.
  auto MakeFile(std::filesystem::path file) -> std::filesystem::path {
    std::ofstream out(file.c_str());
    out << "stub";
    CARBON_CHECK(out, "error creating {0}", file);
    return file;
  }

  // Creates a symlink to the target. Returns the input file for easier use.
  auto MakeSymlink(std::filesystem::path file, auto target)
      -> std::filesystem::path {
    std::error_code ec;
    std::filesystem::create_symlink(target, file, ec);
    CARBON_CHECK(!ec, "error creating {0}: {1}", file, ec.message());
    return file;
  }

  // Creates a directory, recursively if needed. Returns the input file for
  // easier use.
  auto MakeDir(std::filesystem::path dir) -> std::filesystem::path {
    std::error_code ec;
    std::filesystem::create_directories(dir, ec);
    CARBON_CHECK(!ec, "error creating {0}: {1}", dir, ec.message());
    return dir;
  }

  // Creates a synthetic install tree to test a batch of interactions.
  // Optionally accepts a symlink target for the busybox in the install tree.
  // Returns the input prefix for easy use.
  auto MakeInstallTree(std::filesystem::path prefix,
                       std::optional<std::filesystem::path> busybox_target = {})
      -> std::filesystem::path {
    MakeDir(prefix / "lib/carbon");
    if (busybox_target) {
      MakeSymlink(prefix / "lib/carbon/carbon-busybox", *busybox_target);
    } else {
      MakeBusyboxFile(prefix / "lib/carbon/carbon-busybox");
    }
    MakeDir(prefix / "lib/carbon/llvm/bin");
    MakeSymlink(prefix / "lib/carbon/llvm/bin/clang++", "clang");
    MakeSymlink(prefix / "lib/carbon/llvm/bin/clang", "../../carbon-busybox");
    MakeDir(prefix / "bin");
    MakeSymlink(prefix / "bin/carbon", "../lib/carbon/carbon-busybox");
    return prefix;
  }

  // Makes a fake busybox file. This is a symlink to the running binary because
  // we validate to make sure the running binary is one and the same.
  auto MakeBusyboxFile(std::filesystem::path file) -> std::filesystem::path {
    return MakeSymlink(file, running_binary_);
  }

  // The path to the running binary, `busybox_info_test`. This is provided
  // because `GetExecutablePath` can fall back to it.
  std::string running_binary_;

  // The test's temp directory, deleted on destruction.
  std::filesystem::path dir_;
};

TEST_F(BusyboxInfoTest, Direct) {
  auto busybox = MakeBusyboxFile(dir_ / "carbon-busybox");

  auto info = GetBusyboxInfo(busybox.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(busybox));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

TEST_F(BusyboxInfoTest, SymlinkInCurrentDirectory) {
  MakeBusyboxFile(dir_ / "carbon-busybox");
  auto target = MakeSymlink(dir_ / "carbon", "carbon-busybox");

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "carbon-busybox"));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

TEST_F(BusyboxInfoTest, SymlinkInCurrentDirectoryWithDot) {
  MakeBusyboxFile(dir_ / "carbon-busybox");
  auto target = MakeSymlink(dir_ / "carbon", "./carbon-busybox");

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "./carbon-busybox"));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

TEST_F(BusyboxInfoTest, ExtraSymlink) {
  MakeBusyboxFile(dir_ / "carbon-busybox");
  MakeSymlink(dir_ / "c", "carbon-busybox");
  auto target = MakeSymlink(dir_ / "carbon", "c");

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "carbon-busybox"));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

TEST_F(BusyboxInfoTest, OriginalSymlinkNameFormsMode) {
  MakeBusyboxFile(dir_ / "carbon-busybox");
  MakeSymlink(dir_ / "carbon", "carbon-busybox");
  auto clang_target = MakeSymlink(dir_ / "clang", "carbon");
  auto clang_plusplus_target = MakeSymlink(dir_ / "clang++", "clang");

  auto info = GetBusyboxInfo(clang_target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "carbon-busybox"));
  EXPECT_THAT(info->mode, Eq("clang"));

  info = GetBusyboxInfo(clang_plusplus_target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "carbon-busybox"));
  EXPECT_THAT(info->mode, Eq("clang++"));
}

TEST_F(BusyboxInfoTest, BusyboxIsSymlinkToNowhere) {
  auto target = MakeSymlink(dir_ / "carbon-busybox", "nonexistent");

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_FALSE(info.ok());
  EXPECT_THAT(info.error().message(),
              Eq(llvm::formatv("expected carbon-busybox symlink at `{0}`",
                               running_binary_)
                     .str()));
}

TEST_F(BusyboxInfoTest, BusyboxIsWrongFile) {
  // This has the correct name, but it doesn't map back to the running binary
  // and so is ignored.
  auto target = MakeFile(dir_ / "carbon-busybox");

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_FALSE(info.ok());
  EXPECT_THAT(info.error().message(),
              Eq(llvm::formatv("expected carbon-busybox symlink at `{0}`",
                               running_binary_)
                     .str()));
}

TEST_F(BusyboxInfoTest, RelativeSymlink) {
  MakeDir(dir_ / "dir1");
  MakeBusyboxFile(dir_ / "dir1/carbon-busybox");
  MakeDir(dir_ / "dir2");
  auto target = MakeSymlink(dir_ / "dir2/carbon", "../dir1/carbon-busybox");

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "dir2/../dir1/carbon-busybox"));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

TEST_F(BusyboxInfoTest, AbsoluteSymlink) {
  MakeDir(dir_ / "dir1");
  auto busybox = MakeBusyboxFile(dir_ / "dir1/carbon-busybox");
  ASSERT_TRUE(busybox.is_absolute());
  MakeDir(dir_ / "dir2");
  auto target = MakeSymlink(dir_ / "dir2/carbon", busybox);

  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(busybox));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

TEST_F(BusyboxInfoTest, NotBusyboxFile) {
  auto target = MakeFile(dir_ / "file");

  auto info = GetBusyboxInfo(target.c_str());
  EXPECT_FALSE(info.ok());
}

TEST_F(BusyboxInfoTest, NotBusyboxSymlink) {
  MakeFile(dir_ / "file");
  auto target = MakeSymlink(dir_ / "carbon", "file");

  auto info = GetBusyboxInfo(target.c_str());
  EXPECT_FALSE(info.ok());
}

TEST_F(BusyboxInfoTest, LayerSymlinksInstallTree) {
  auto actual_busybox = MakeBusyboxFile(dir_ / "actual-busybox");

  // Create a facsimile of the install prefix with even the busybox as a
  // symlink. Also include potential relative sibling symlinks like `clang++` to
  // `clang`.
  auto prefix = MakeInstallTree(dir_ / "test_prefix", actual_busybox);

  auto info = GetBusyboxInfo((prefix / "bin/carbon").c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(prefix / "bin/../lib/carbon/carbon-busybox"));
  EXPECT_THAT(info->mode, Eq(std::nullopt));

  info = GetBusyboxInfo((prefix / "lib/carbon/llvm/bin/clang").c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path,
              Eq(prefix / "lib/carbon/llvm/bin/../../carbon-busybox"));
  EXPECT_THAT(info->mode, Eq("clang"));

  info = GetBusyboxInfo((prefix / "lib/carbon/llvm/bin/clang++").c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path,
              Eq(prefix / "lib/carbon/llvm/bin/../../carbon-busybox"));
  EXPECT_THAT(info->mode, Eq("clang++"));
}

TEST_F(BusyboxInfoTest, StopSearchAtFirstSymlinkWithRelativeBusybox) {
  // Some install of Carbon under `opt`.
  auto opt_prefix = MakeInstallTree(dir_ / "opt");

  // A second install, but with its symlinks pointing into the `opt` tree rather
  // than at its busybox.
  MakeDir(dir_ / "lib/carbon");
  MakeBusyboxFile(dir_ / "lib/carbon/carbon-busybox");
  MakeDir(dir_ / "bin");
  auto target = MakeSymlink(dir_ / "bin/carbon", "../opt/bin/carbon");
  MakeDir(dir_ / "lib/carbon/llvm/bin");
  auto clang_target = MakeSymlink(dir_ / "lib/carbon/llvm/bin/clang",
                                  opt_prefix / "lib/carbon/llvm/bin/clang");

  // Starting from the second install uses the relative busybox rather than
  // traversing the symlink further.
  auto info = GetBusyboxInfo(target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(dir_ / "bin/../lib/carbon/carbon-busybox"));
  info = GetBusyboxInfo(clang_target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path,
              Eq(dir_ / "lib/carbon/llvm/bin/../../carbon-busybox"));
}

TEST_F(BusyboxInfoTest, RejectSymlinkInUnrelatedInstall) {
  // Add two installs of Carbon nested inside each other in a realistic
  // scenario: `/usr` and `/usr/local`.
  MakeInstallTree(dir_ / "usr");
  MakeInstallTree(dir_ / "usr/local");

  // Now add a stray symlink directly in `.../usr/local` to the local install.
  //
  // This has the interesting property that both of these "work" and find the
  // same busybox but probably wanted to find different ones:
  // - `.../usr/local/../lib/carbon/carbon-busybox`
  // - `.../usr/bin/../lib/carbon/carbon-busybox`
  auto stray_target = MakeSymlink(dir_ / "usr/local/carbon", "bin/carbon");

  // Check that the busybox doesn't use the relative busybox in this case, and
  // walks the symlink to find the correct installation.
  auto info = GetBusyboxInfo(stray_target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path,
              Eq(dir_ / "usr/local/bin/../lib/carbon/carbon-busybox"));

  // Ensure this works even with intervening `.` directory components.
  stray_target = MakeSymlink(dir_ / "usr/local/carbon2", "bin/././carbon");

  // Check that the busybox doesn't use the relative busybox in this case, and
  // walks the symlink to find the correct installation.
  info = GetBusyboxInfo(stray_target.c_str());
  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path,
              Eq(dir_ / "usr/local/bin/../lib/carbon/carbon-busybox"));
}

TEST_F(BusyboxInfoTest, EnvBinaryPathOverride) {
  // The test should not have this environment variable set.
  ASSERT_THAT(getenv(Argv0OverrideEnv), Eq(nullptr));

  // Set the environment to our actual busybox.
  auto busybox = MakeBusyboxFile(dir_ / "carbon-busybox");

  setenv(Argv0OverrideEnv, busybox.c_str(), /*overwrite=*/1);
  auto info = GetBusyboxInfo("/some/nonexistent/path");
  if (getenv(Argv0OverrideEnv)) {
    unsetenv(Argv0OverrideEnv);
    ADD_FAILURE() << "GetBusyboxInfo should unset Argv0OverrideEnv";
  }

  ASSERT_TRUE(info.ok()) << info.error();
  EXPECT_THAT(info->bin_path, Eq(busybox));
  EXPECT_THAT(info->mode, Eq(std::nullopt));
}

}  // namespace
}  // namespace Carbon
