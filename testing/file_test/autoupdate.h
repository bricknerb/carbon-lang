// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TESTING_FILE_TEST_AUTOUPDATE_H_
#define CARBON_TESTING_FILE_TEST_AUTOUPDATE_H_

#include <filesystem>
#include <utility>

#include "common/check.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "re2/re2.h"
#include "testing/file_test/line.h"

namespace Carbon::Testing {

class FileTestAutoupdater {
 public:
  struct LineNumberReplacement {
    bool has_file;

    // The line replacement. The pattern should match lines. If has_file,
    // pattern should have a file and line group; otherwise, only a line group,
    // but default_file_re should be provided.
    //
    // Uses shared_ptr for storage in SmallVector.
    std::shared_ptr<RE2> re;

    // line_formatv should provide {0} to substitute with [[@LINE...]] deltas.
    std::string line_formatv;
  };

  explicit FileTestAutoupdater(
      const std::filesystem::path& file_test_path, std::string test_command,
      std::string dump_command, llvm::StringRef input_content,
      const llvm::SmallVector<llvm::StringRef>& filenames,
      int autoupdate_line_number, bool autoupdate_split,
      const llvm::SmallVector<FileTestLine>& non_check_lines,
      llvm::StringRef actual_stdout, llvm::StringRef actual_stderr,
      const std::optional<RE2>& default_file_re,
      const llvm::SmallVector<LineNumberReplacement>& line_number_replacements,
      std::function<auto(std::string&)->void> do_extra_check_replacements)
      : file_test_path_(file_test_path),
        test_command_(std::move(test_command)),
        dump_command_(std::move(dump_command)),
        input_content_(input_content),
        autoupdate_line_number_(autoupdate_line_number),
        non_check_lines_(non_check_lines),
        default_file_re_(default_file_re),
        line_number_replacements_(line_number_replacements),
        do_extra_check_replacements_(std::move(do_extra_check_replacements)),
        autoupdate_split_file_(
            autoupdate_split ? std::optional(filenames.size()) : std::nullopt),
        file_to_number_map_(BuildFileToNumberMap(filenames)),
        // BuildCheckLines should only be called after other member
        // initialization.
        stdout_(BuildCheckLines(actual_stdout, "STDOUT")),
        stderr_(BuildCheckLines(actual_stderr, "STDERR")),
        any_attached_stdout_lines_(llvm::any_of(
            stdout_.lines,
            [&](const CheckLine& line) { return line.line_number() != -1; })),
        non_check_line_(non_check_lines_.begin()) {
    for (const auto& replacement : line_number_replacements_) {
      CARBON_CHECK(replacement.has_file || default_file_re_,
                   "For replacement with pattern `{0}` to have has_file=false, "
                   "override GetDefaultFileRE.",
                   replacement.re->pattern());
      CARBON_CHECK(replacement.re->ok(), "Invalid line replacement RE2: {0}",
                   replacement.re->error());
    }
  }

  // Automatically updates CHECKs in the provided file when dry_run=false.
  // Returns true if generated file content differs from actual file content.
  auto Run(bool dry_run) -> bool;

 private:
  // The file and line number that a CHECK line refers to, and the
  // replacement from which they were determined, if any.
  struct FileAndLineNumber {
    explicit FileAndLineNumber(int file_number) : file_number(file_number) {}

    explicit FileAndLineNumber(const LineNumberReplacement* replacement,
                               int file_number, absl::string_view line_number);

    const LineNumberReplacement* replacement = nullptr;
    int file_number;
    int line_number = -1;
  };

  // A TIP line added by autoupdate. Not associated with any line in output.
  class TipLine : public FileTestLineBase {
   public:
    explicit TipLine(std::string line)
        : FileTestLineBase(-1, -1), line_(std::move(line)) {}

    auto Print(llvm::raw_ostream& out) const -> void override { out << line_; }

    auto is_blank() const -> bool override { return line_.empty(); }

   private:
    std::string line_;
  };

  // A CHECK line which is integrated into autoupdate output.
  //
  // `final` because we use pointer arithmetic on this type.
  class CheckLine final : public FileTestLineBase {
   public:
    // RE2 is passed by a pointer because it doesn't support std::optional.
    explicit CheckLine(FileAndLineNumber file_and_line_number, std::string line)
        : FileTestLineBase(file_and_line_number.file_number,
                           file_and_line_number.line_number),
          replacement_(file_and_line_number.replacement),
          line_(std::move(line)) {}

    auto Print(llvm::raw_ostream& out) const -> void override {
      out << indent_ << line_;
    }

    // When the location of the CHECK in output is known, we can set the indent
    // and its line.
    auto SetOutputLine(llvm::StringRef indent, int output_file_number,
                       int output_line_number) -> void {
      indent_ = indent;
      output_file_number_ = output_file_number;
      output_line_number_ = output_line_number;
    }

    // When the location of all lines in a file are known, we can set the line
    // offset based on the target line.
    auto RemapLineNumbers(
        const llvm::DenseMap<llvm::StringRef, int>& file_to_number_map,
        const llvm::DenseMap<std::pair<int, int>, int>& output_line_remap,
        const llvm::SmallVector<int>& new_last_line_numbers) -> void;

    auto is_blank() const -> bool override { return false; }

   private:
    const LineNumberReplacement* replacement_;
    std::string line_;
    llvm::StringRef indent_;
    int output_file_number_ = -1;
    int output_line_number_ = -1;
  };

  // Clusters information for stdout and stderr.
  struct CheckLines {
    explicit CheckLines(llvm::SmallVector<CheckLine> lines)
        : lines(std::move(lines)), cursor(this->lines.begin()) {}

    // The full list of check lines.
    llvm::SmallVector<CheckLine> lines;
    // An iterator into check_lines.
    CheckLine* cursor;
  };

  // Looks for the patterns in the line. Returns the first match, or defaulted
  // information if not found.
  auto GetFileAndLineNumber(
      const llvm::DenseMap<llvm::StringRef, int>& file_to_number_map,
      int default_file_number, const std::string& check_line)
      -> FileAndLineNumber;

  // Builds a mapping from file name to file number.
  auto BuildFileToNumberMap(const llvm::SmallVector<llvm::StringRef>& filenames)
      -> llvm::DenseMap<llvm::StringRef, int> {
    llvm::DenseMap<llvm::StringRef, int> file_to_number_map;
    for (auto [number, name] : llvm::enumerate(filenames)) {
      file_to_number_map.insert({name, number});
    }
    return file_to_number_map;
  }

  // Builds CheckLine lists for autoupdate.
  auto BuildCheckLines(llvm::StringRef output, const char* label) -> CheckLines;

  // Adds a non-check line to the new_lines and output_line_remap. The caller
  // still needs to advance the cursor when ready.
  auto AddRemappedNonCheckLine() -> void;

  // Adds TIP lines for file_test usage.
  auto AddTips() -> void;

  // Returns true if there's a CheckLine that should be added at
  // `to_line_number`.
  auto ShouldAddCheckLine(const CheckLines& check_lines, bool to_file_end) const
      -> bool;

  // Adds check_lines until output reaches:
  // - If not to_file_end, non_check_line.
  // - If to_file_end, the end of the file.
  auto AddCheckLines(CheckLines& check_lines, bool to_file_end) -> void;

  // Adds remaining check lines for the current file. stderr is always included,
  // but stdout is only included when either any_attached_stdout_lines_ or
  // is_last_file is true.
  auto FinishFile(bool is_last_file) -> void;

  // Starts a new split file, updating file and line numbers. Advances past the
  // split line.
  auto StartSplitFile() -> void;

  // Passed-in state.
  const std::filesystem::path& file_test_path_;
  std::string test_command_;
  std::string dump_command_;
  llvm::StringRef input_content_;
  int autoupdate_line_number_;
  const llvm::SmallVector<FileTestLine>& non_check_lines_;
  const std::optional<RE2>& default_file_re_;
  const llvm::SmallVector<LineNumberReplacement>& line_number_replacements_;
  std::function<auto(std::string&)->void> do_extra_check_replacements_;

  // If we have an autoupdate split that still needs to be processed, the file
  // number of the autoupdate split. Otherwise, this is nullopt.
  std::optional<int> autoupdate_split_file_;

  // Generated TIP lines, from AddTips.
  llvm::SmallVector<TipLine> tips_;

  // Mapping from file names to file numbers.
  llvm::DenseMap<llvm::StringRef, int> file_to_number_map_;

  // The constructed CheckLine list and cursor.
  CheckLines stdout_;
  CheckLines stderr_;

  // Whether any stdout lines have an associated line number.
  bool any_attached_stdout_lines_;

  // Iterators for the main Run loop.
  const FileTestLine* non_check_line_;

  // Tracks the new last line numbers for each file.
  llvm::SmallVector<int> new_last_line_numbers_;

  // A reusable blank line. new_lines_ can contain a reference back to it.
  const FileTestLine blank_line_ = FileTestLine(-1, -1, "");

  // Stitched-together content.
  llvm::SmallVector<const FileTestLineBase*> new_lines_;

  // Maps {file_number, original line number} to a new line number.
  llvm::DenseMap<std::pair<int, int>, int> output_line_remap_;

  // The current output file number; mainly used for tracking progression.
  int output_file_number_ = 0;
  // The current output line number in stitched content.
  int output_line_number_ = 0;
};

}  // namespace Carbon::Testing

#endif  // CARBON_TESTING_FILE_TEST_AUTOUPDATE_H_
