// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include <unistd.h>

#include <cstdlib>
#include <string>

#include "common/bazel_working_dir.h"
#include "common/error.h"
#include "common/init_llvm.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/LLVMDriver.h"
#include "toolchain/driver/driver.h"
#include "toolchain/install/busybox_info.h"
#include "toolchain/install/install_paths.h"

namespace Carbon {

// The actual `main` implementation. Can return an exit code or an `Error`
// (which causes EXIT_FAILRUE).
static auto Main(int argc, char** argv) -> ErrorOr<int> {
  InitLLVM init_llvm(argc, argv);

  // Start by resolving any symlinks.
  CARBON_ASSIGN_OR_RETURN(auto busybox_info, GetBusyboxInfo(argv[0]));

  auto fs = llvm::vfs::getRealFileSystem();

  // Resolve paths before calling SetWorkingDirForBazel.
  std::string exe_path = busybox_info.bin_path.string();
  const auto install_paths = InstallPaths::MakeExeRelative(exe_path);
  if (install_paths.error()) {
    return Error(*install_paths.error());
  }

  // If `LLVM_SYMBOLIZER_PATH` is unset, sets it. Signals.cpp would do some more
  // path resolution which this overrides in favor of using the busybox itself
  // for symbolization.
  setenv("LLVM_SYMBOLIZER_PATH",
         (install_paths.llvm_install_bin() + "llvm-symbolizer").c_str(),
         /*overwrite=*/0);

  SetWorkingDirForBazel();

  llvm::SmallVector<llvm::StringRef> args;
  args.reserve(argc + 1);
  if (busybox_info.mode) {
    // Map busybox modes to the relevant subcommands with any flags needed to
    // emulate the requested command. Typically, our busyboxed binaries redirect
    // to a specific subcommand with some flags set and then pass the remaining
    // busybox arguments as positional arguments to that subcommand.
    //
    // TODO: Add relevant flags to the `clang` subcommand and add `clang`-based
    // symlinks to this like `clang++`.
    auto subcommand_args =
        llvm::StringSwitch<llvm::SmallVector<llvm::StringRef>>(
            *busybox_info.mode)
            // The `clang` program name used configures the default for its
            // `--driver-mode` flag. The first of these is redundant with the
            // default, but we group it here for clarity.
            .Case("clang", {"clang", "--"})
            .Case("clang++", {"clang", "--", "--driver-mode=g++"})
            .Case("clang-cl", {"clang", "--", "--driver-mode=cl"})
            .Case("clang-cpp", {"clang", "--", "--driver-mode=cpp"})

            // LLD has platform-specific program names that we translate into
            // platform flags.
            .Case("ld.lld", {"lld", "--platform=gnu", "--"})
            .Case("ld64.lld", {"lld", "--platform=darwin", "--"})

    // We also support a number of LLVM tools with a trivial translation
    // to subcommands. If any of these end up needing more advanced
    // translation, that can be factored into the `.def` file to provide custom
    // expansion here.
#define CARBON_LLVM_TOOL(Id, Name, BinName, MainFn) \
  .Case(BinName, {"llvm", Name, "--"})
#include "toolchain/base/llvm_tools.def"

            .Default({*busybox_info.mode, "--"});
    args.append(subcommand_args);
  }
  args.append(argv + 1, argv + argc);

  Driver driver(fs, &install_paths, stdin, &llvm::outs(), &llvm::errs(),
                /*fuzzing=*/false, /*enable_leaking=*/true);
  bool success = driver.RunCommand(args).success;
  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}

}  // namespace Carbon

auto main(int argc, char** argv) -> int {
  auto result = Carbon::Main(argc, argv);
  if (result.ok()) {
    return *result;
  } else {
    llvm::errs() << "error: " << result.error() << "\n";
    return EXIT_FAILURE;
  }
}
