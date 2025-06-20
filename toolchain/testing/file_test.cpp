// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_DRIVER_DRIVER_FILE_TEST_BASE_H_
#define CARBON_TOOLCHAIN_DRIVER_DRIVER_FILE_TEST_BASE_H_

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "absl/strings/str_replace.h"
#include "common/error.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "testing/file_test/file_test_base.h"
#include "toolchain/driver/driver.h"

namespace Carbon::Testing {
namespace {

// Provides common test support for the driver. This is used by file tests in
// component subdirectories.
class ToolchainFileTest : public FileTestBase {
 public:
  explicit ToolchainFileTest(llvm::StringRef exe_path,
                             llvm::StringRef test_name);

  // Loads files into the VFS and runs the driver.
  auto Run(const llvm::SmallVector<llvm::StringRef>& test_args,
           llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem>& fs,
           FILE* input_stream, llvm::raw_pwrite_stream& output_stream,
           llvm::raw_pwrite_stream& error_stream) const
      -> ErrorOr<RunResult> override;

  // Sets different default flags based on the component being tested.
  auto GetDefaultArgs() const -> llvm::SmallVector<std::string> override;

  // Generally uses the parent implementation, with special handling for lex.
  auto GetDefaultFileRE(llvm::ArrayRef<llvm::StringRef> filenames) const
      -> std::optional<RE2> override;

  // Generally uses the parent implementation, with special handling for lex.
  auto GetLineNumberReplacements(llvm::ArrayRef<llvm::StringRef> filenames)
      const -> llvm::SmallVector<LineNumberReplacement> override;

  // Generally uses the parent implementation, with special handling for lex and
  // driver.
  auto DoExtraCheckReplacements(std::string& check_line) const -> void override;

  // Most tests can be run in parallel, but clangd has a global for its logging
  // system so we need language-server tests to be run in serial.
  auto AllowParallelRun() const -> bool override {
    return component_ != "language_server";
  }

 private:
  // The toolchain component subdirectory, such as `lex` or `language_server`.
  const llvm::StringRef component_;
  // The toolchain install information.
  const InstallPaths installation_;
};

}  // namespace

CARBON_FILE_TEST_FACTORY(ToolchainFileTest)

// Returns the toolchain subdirectory being tested.
static auto GetComponent(llvm::StringRef test_name) -> llvm::StringRef {
  // This handles cases where the toolchain directory may be copied into a
  // repository that doesn't put it at the root.
  auto pos = test_name.find("toolchain/");
  CARBON_CHECK(pos != llvm::StringRef::npos, "{0}", test_name);
  test_name = test_name.drop_front(pos + strlen("toolchain/"));
  test_name = test_name.take_front(test_name.find("/"));
  return test_name;
}

ToolchainFileTest::ToolchainFileTest(llvm::StringRef exe_path,
                                     llvm::StringRef test_name)
    : FileTestBase(test_name),
      component_(GetComponent(test_name)),
      installation_(InstallPaths::MakeForBazelRunfiles(exe_path)) {}

// Adds a file to the fs.
static auto AddFile(llvm::vfs::InMemoryFileSystem& fs, llvm::StringRef path)
    -> ErrorOr<Success> {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> file =
      llvm::MemoryBuffer::getFile(path);
  if (file.getError()) {
    return ErrorBuilder() << "Getting `" << path
                          << "`: " << file.getError().message();
  }
  if (!fs.addFile(path, /*ModificationTime=*/0, std::move(*file))) {
    return ErrorBuilder() << "Duplicate file: `" << path << "`";
  }
  return Success();
}

auto ToolchainFileTest::Run(
    const llvm::SmallVector<llvm::StringRef>& test_args,
    llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem>& fs,
    FILE* input_stream, llvm::raw_pwrite_stream& output_stream,
    llvm::raw_pwrite_stream& error_stream) const -> ErrorOr<RunResult> {
  llvm::SmallVector<std::string> prelude_files;
  // Lex and parse shouldn't ever access the prelude.
  if (component_ != "lex" && component_ != "parse") {
    // TODO: Try providing the prelude as an overlay.
    CARBON_ASSIGN_OR_RETURN(prelude_files, installation_.ReadPreludeManifest());
    for (const auto& file : prelude_files) {
      CARBON_RETURN_IF_ERROR(AddFile(*fs, file));
    }
  }

  Driver driver(fs, &installation_, input_stream, &output_stream,
                &error_stream);
  auto driver_result = driver.RunCommand(test_args);
  // If any diagnostics have been produced, add a trailing newline to make the
  // last diagnostic match intermediate diagnostics (that have a newline
  // separator between them). This reduces churn when adding new diagnostics
  // to test cases.
  if (error_stream.tell() > 0) {
    error_stream << '\n';
  }

  RunResult result{
      .success = driver_result.success,
      .per_file_success = std::move(driver_result.per_file_success)};
  // Drop entries that don't look like a file, and entries corresponding to
  // the prelude. Note this can empty out the list.
  llvm::erase_if(result.per_file_success,
                 [&](std::pair<llvm::StringRef, bool> entry) {
                   return entry.first == "." || entry.first == "-" ||
                          entry.first.starts_with("not_file") ||
                          llvm::is_contained(prelude_files, entry.first);
                 });

  if (component_ == "language_server") {
    // The language server doesn't always add a suffix newline, so add one for
    // tests to be happy.
    output_stream << "\n";
  }
  return result;
}

auto ToolchainFileTest::GetDefaultArgs() const
    -> llvm::SmallVector<std::string> {
  llvm::SmallVector<std::string> args = {"--include-diagnostic-kind"};

  if (component_ == "format") {
    args.insert(args.end(), {"format", "%s"});
    return args;
  } else if (component_ == "language_server") {
    args.insert(args.end(), {"language-server"});
    return args;
  }

  args.insert(args.end(),
              {
                  "compile",
                  "--phase=" + component_.str(),
                  // Use the install path to exclude prelude files.
                  "--exclude-dump-file-prefix=" + installation_.core_package(),
              });

  if (component_ == "lex") {
    args.insert(args.end(), {"--no-prelude-import", "--dump-tokens",
                             "--omit-file-boundary-tokens"});
  } else if (component_ == "parse") {
    args.insert(args.end(), {"--no-prelude-import", "--dump-parse-tree"});
  } else if (component_ == "check") {
    args.insert(args.end(), {"--dump-sem-ir", "--dump-sem-ir-ranges=only"});
  } else if (component_ == "lower") {
    args.insert(args.end(), {"--dump-llvm-ir", "--target=x86_64-linux-gnu"});
  } else if (component_ == "codegen") {
    // codegen tests specify flags as needed.
  } else {
    CARBON_FATAL("Unexpected test component {0}: {1}", component_, test_name());
  }

  args.push_back("%s");
  return args;
}

auto ToolchainFileTest::GetDefaultFileRE(
    llvm::ArrayRef<llvm::StringRef> filenames) const -> std::optional<RE2> {
  if (component_ == "lex") {
    return std::make_optional<RE2>(
        llvm::formatv(R"(^- filename: ({0})$)", llvm::join(filenames, "|")));
  }
  return FileTestBase::GetDefaultFileRE(filenames);
}

auto ToolchainFileTest::GetLineNumberReplacements(
    llvm::ArrayRef<llvm::StringRef> filenames) const
    -> llvm::SmallVector<LineNumberReplacement> {
  auto replacements = FileTestBase::GetLineNumberReplacements(filenames);
  if (component_ == "lex") {
    replacements.push_back({.has_file = false,
                            .re = std::make_shared<RE2>(R"(line: (\s*\d+))"),
                            // The `{{{{` becomes `{{`.
                            .line_formatv = "{{{{ *}}{0}"});
  }
  return replacements;
}

auto ToolchainFileTest::DoExtraCheckReplacements(std::string& check_line) const
    -> void {
  if (component_ == "driver") {
    // TODO: Disable token output, it's not interesting for these tests.
    if (llvm::StringRef(check_line).starts_with("// CHECK:STDOUT: {")) {
      check_line = "// CHECK:STDOUT: {{.*}}";
    }
  } else if (component_ == "lex") {
    // Both FileStart and FileEnd regularly have locations on CHECK
    // comment lines that don't work correctly. The line happens to be correct
    // for the FileEnd, but we need to avoid checking the column.
    // The column happens to be right for FileStart, but the line is wrong.
    static RE2 file_token_re(R"((FileEnd.*column: |FileStart.*line: )( *\d+))");
    RE2::Replace(&check_line, file_token_re, R"(\1{{ *\\d+}})");
  } else if (component_ == "check") {
    // The path to the core package appears in some check diagnostics, and will
    // differ between testing environments, so don't test it.
    // TODO: Consider adding a content keyword to name the core package, and
    // replace with that instead. Alternatively, consider adding the core
    // package to the VFS with a fixed name.
    absl::StrReplaceAll({{installation_.core_package(), "{{.*}}"}},
                        &check_line);
  } else {
    FileTestBase::DoExtraCheckReplacements(check_line);
  }
}

}  // namespace Carbon::Testing

#endif  // CARBON_TOOLCHAIN_DRIVER_DRIVER_FILE_TEST_BASE_H_
