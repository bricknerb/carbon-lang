// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "testing/file_test/autoupdate.h"

#include <fstream>
#include <string>
#include <utility>

#include "absl/strings/str_replace.h"
#include "absl/strings/string_view.h"
#include "common/check.h"
#include "common/ostream.h"
#include "common/raw_string_ostream.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/FormatVariadic.h"
#include "testing/base/file_helpers.h"

namespace Carbon::Testing {

// Converts a matched line number to an int, trimming whitespace. Returns 0 if
// there is no line number, to assist early placement.
static auto ParseLineNumber(absl::string_view matched_line_number) -> int {
  llvm::StringRef trimmed = matched_line_number;
  trimmed = trimmed.trim();
  if (trimmed.empty()) {
    return 0;
  }
  // NOLINTNEXTLINE(google-runtime-int): API requirement.
  long long val;
  CARBON_CHECK(!llvm::getAsSignedInteger(trimmed, 10, val), "{0}",
               matched_line_number);
  return val;
}

FileTestAutoupdater::FileAndLineNumber::FileAndLineNumber(
    const LineNumberReplacement* replacement, int file_number,
    absl::string_view line_number)
    : replacement(replacement),
      file_number(file_number),
      line_number(ParseLineNumber(line_number)) {}

auto FileTestAutoupdater::CheckLine::RemapLineNumbers(
    const llvm::DenseMap<llvm::StringRef, int>& file_to_number_map,
    const llvm::DenseMap<std::pair<int, int>, int>& output_line_remap,
    const llvm::SmallVector<int>& new_last_line_numbers) -> void {
  // Only need to do remappings when there's a line number replacement.
  if (!replacement_) {
    return;
  }

  // Use a cursor for the line so that we can't keep matching the same
  // content, which may occur when we keep a literal line number.
  int line_offset = 0;
  while (true) {
    // Rebuild the cursor each time because we're editing the line, which
    // could cause a reallocation.
    absl::string_view line_cursor = line_;
    line_cursor.remove_prefix(line_offset);
    // Look for a line number to replace. There may be multiple, so we
    // repeatedly check.
    absl::string_view matched_filename;
    absl::string_view matched_line_number;
    if (replacement_->has_file) {
      RE2::PartialMatch(line_cursor, *replacement_->re, &matched_filename,
                        &matched_line_number);
    } else {
      RE2::PartialMatch(line_cursor, *replacement_->re, &matched_line_number);
    }
    if (matched_line_number.empty()) {
      return;
    }

    // Map the matched filename to its file number.
    auto matched_file_number = file_number();
    if (replacement_->has_file) {
      auto it = file_to_number_map.find(matched_filename);
      if (it != file_to_number_map.end()) {
        matched_file_number = it->second;
      }
    }

    // Calculate the new line number (possibly with new CHECK lines added, or
    // some removed).
    int old_line_number = ParseLineNumber(matched_line_number);
    int new_line_number = -1;
    if (auto remapped =
            output_line_remap.find({matched_file_number, old_line_number});
        remapped != output_line_remap.end()) {
      // Map old non-check lines to their new line numbers.
      new_line_number = remapped->second;
    } else {
      // We assume unmapped references point to the end-of-file.
      new_line_number = new_last_line_numbers[matched_file_number];
    }

    std::string replacement;
    if (matched_file_number == output_file_number_) {
      int offset = new_line_number - output_line_number_;
      // Update the line offset in the CHECK line.
      const char* offset_prefix = offset < 0 ? "" : "+";
      replacement = llvm::formatv(
          replacement_->line_formatv.c_str(),
          llvm::formatv("[[@LINE{0}{1}]]", offset_prefix, offset));
    } else {
      // If the CHECK was written to a different file from the file that it
      // refers to, leave behind an absolute line reference rather than a
      // cross-file offset.
      replacement =
          llvm::formatv(replacement_->line_formatv.c_str(), new_line_number);
    }
    auto line_number_offset = matched_line_number.data() - line_.data();
    line_.replace(line_number_offset, matched_line_number.size(), replacement);

    // Resume matching from the end of the replacement line number.
    line_offset = line_number_offset + replacement.size();
  }
}

auto FileTestAutoupdater::GetFileAndLineNumber(
    const llvm::DenseMap<llvm::StringRef, int>& file_to_number_map,
    int default_file_number, const std::string& check_line)
    -> FileAndLineNumber {
  for (const auto& replacement : line_number_replacements_) {
    if (replacement.has_file) {
      absl::string_view filename;
      absl::string_view line_number;
      if (RE2::PartialMatch(check_line, *replacement.re, &filename,
                            &line_number)) {
        if (auto it = file_to_number_map.find(filename);
            it != file_to_number_map.end()) {
          return FileAndLineNumber(&replacement, it->second, line_number);
        } else {
          return FileAndLineNumber(default_file_number);
        }
      }
    } else {
      // There's no file association, so we only look at the line, and assume
      // it refers to the default file.
      absl::string_view line_number;
      if (RE2::PartialMatch(check_line, *replacement.re, &line_number)) {
        return FileAndLineNumber(&replacement, default_file_number,
                                 line_number);
      }
    }
  }
  return FileAndLineNumber(default_file_number);
}

auto FileTestAutoupdater::BuildCheckLines(llvm::StringRef output,
                                          const char* label) -> CheckLines {
  if (output.empty()) {
    return CheckLines({});
  }

  // %t substitution means we may see the temporary directory's path in output.
  std::filesystem::path tmpdir_path = GetTempDirectory();
  llvm::StringRef tmpdir = tmpdir_path.native();

  llvm::SmallVector<llvm::StringRef> lines(llvm::split(output, '\n'));
  // It's typical that output ends with a newline, but we don't want to add a
  // blank CHECK for it.
  if (lines.back().empty()) {
    lines.pop_back();
  }

  // The default file number for when no specific file is found.
  int default_file_number = 0;

  llvm::SmallVector<CheckLine> check_lines;
  for (const auto& line : lines) {
    // This code is relatively hot in our testing, and because when testing it
    // isn't run with an optimizer we benefit from making it use simple
    // constructs. For this reason, we avoid `llvm::formatv` and similar tools.
    std::string check_line;
    check_line.reserve(line.size() + strlen(label) + strlen("// CHECK:: "));
    check_line.append("// CHECK:");
    check_line.append(label);
    check_line.append(":");
    if (!line.empty()) {
      check_line.append(" ");
      check_line.append(line);
    }

    // \r and \t are invisible characters worth marking.
    // {{ and [[ are autoupdate syntax which we need to escape.
    check_line = absl::StrReplaceAll(check_line, {{"\r", R"({{\r}})"},
                                                  {"\t", R"({{\t}})"},
                                                  {"{{", R"({{\{\{}})"},
                                                  {"[[", R"({{\[\[}})"}});
    // Add an empty regex to call out end-of-line whitespace.
    if (check_line.ends_with(' ')) {
      check_line.append("{{}}");
    }

    // Ignore mentions of the temporary directory in output.
    if (auto pos = check_line.find(tmpdir); pos != std::string::npos) {
      check_line.replace(pos, tmpdir.size(), "{{.+}}");
    }

    do_extra_check_replacements_(check_line);

    if (default_file_re_) {
      absl::string_view filename;
      if (RE2::PartialMatch(line, *default_file_re_, &filename)) {
        auto it = file_to_number_map_.find(filename);
        CARBON_CHECK(it != file_to_number_map_.end(),
                     "default_file_re had unexpected match in '{0}' (`{1}`)",
                     line, default_file_re_->pattern());
        default_file_number = it->second;
      }
    }
    auto file_and_line = GetFileAndLineNumber(file_to_number_map_,
                                              default_file_number, check_line);
    check_lines.push_back(CheckLine(file_and_line, check_line));
  }
  return CheckLines(check_lines);
}

auto FileTestAutoupdater::AddRemappedNonCheckLine() -> void {
  new_lines_.push_back(non_check_line_);
  CARBON_CHECK(output_line_remap_
                   .insert({{non_check_line_->file_number(),
                             non_check_line_->line_number()},
                            ++output_line_number_})
                   .second);
}

auto FileTestAutoupdater::AddTips() -> void {
  CARBON_CHECK(tips_.empty(), "Should only add tips once");

  tips_.reserve(4);
  // This puts commands on a single line so that they can be easily copied.
  tips_.emplace_back("// TIP: To test this file alone, run:");
  tips_.emplace_back("// TIP:   " + test_command_);
  tips_.emplace_back("// TIP: To dump output, run:");
  tips_.emplace_back("// TIP:   " + dump_command_);

  for (const auto& tip : tips_) {
    new_lines_.push_back(&tip);
    ++output_line_number_;
  }
}

auto FileTestAutoupdater::ShouldAddCheckLine(const CheckLines& check_lines,
                                             bool to_file_end) const -> bool {
  return !autoupdate_split_file_ &&
         check_lines.cursor != check_lines.lines.end() &&
         (check_lines.cursor->file_number() < output_file_number_ ||
          (check_lines.cursor->file_number() == output_file_number_ &&
           (to_file_end || check_lines.cursor->line_number() <=
                               non_check_line_->line_number())));
}

auto FileTestAutoupdater::AddCheckLines(CheckLines& check_lines,
                                        bool to_file_end) -> void {
  for (; ShouldAddCheckLine(check_lines, to_file_end); ++check_lines.cursor) {
    new_lines_.push_back(check_lines.cursor);
    check_lines.cursor->SetOutputLine(
        to_file_end ? "" : non_check_line_->indent(), output_file_number_,
        ++output_line_number_);
  }
}

auto FileTestAutoupdater::FinishFile(bool is_last_file) -> void {
  bool include_stdout = any_attached_stdout_lines_ || is_last_file;

  // At the end of each file, print any remaining lines which are associated
  // with the file.
  if (ShouldAddCheckLine(stderr_, /*to_file_end=*/true) ||
      (include_stdout && ShouldAddCheckLine(stdout_, /*to_file_end=*/true))) {
    // Ensure there's a blank line before any trailing CHECKs.
    if (!new_lines_.empty() && !new_lines_.back()->is_blank()) {
      new_lines_.push_back(&blank_line_);
      ++output_line_number_;
    }

    AddCheckLines(stderr_, /*to_file_end=*/true);
    if (include_stdout) {
      AddCheckLines(stdout_, /*to_file_end=*/true);
    }
  }

  new_last_line_numbers_.push_back(output_line_number_);
}

auto FileTestAutoupdater::StartSplitFile() -> void {
  // Advance the file.
  ++output_file_number_;
  output_line_number_ = 0;
  CARBON_CHECK(output_file_number_ == non_check_line_->file_number(),
               "Non-sequential file: {0}", non_check_line_->file_number());

  // Each following file has precisely one split line.
  CARBON_CHECK(non_check_line_->line_number() < 1,
               "Expected a split line, got {0}", *non_check_line_);
  // The split line is ignored when calculating line counts.
  new_lines_.push_back(non_check_line_);

  // Add any file-specific but line-unattached STDOUT messages here. STDERR is
  // handled through the main loop because it's before the next line.
  if (any_attached_stdout_lines_) {
    AddCheckLines(stdout_, /*to_file_end=*/false);
  }

  ++non_check_line_;
}

auto FileTestAutoupdater::Run(bool dry_run) -> bool {
  // Print everything until the autoupdate line.
  while (non_check_line_->line_number() != autoupdate_line_number_) {
    CARBON_CHECK(non_check_line_ != non_check_lines_.end() &&
                     non_check_line_->file_number() == 0,
                 "Missed autoupdate?");
    AddRemappedNonCheckLine();
    ++non_check_line_;
  }

  // Add the AUTOUPDATE line along with any early STDERR lines, so that the
  // initial batch of CHECK lines have STDERR before STDOUT. This also ensures
  // we don't insert a blank line before the STDERR checks if there are no more
  // lines after AUTOUPDATE.
  AddRemappedNonCheckLine();
  AddTips();
  if (!autoupdate_split_file_) {
    AddCheckLines(stderr_, /*to_file_end=*/false);
    if (any_attached_stdout_lines_) {
      AddCheckLines(stdout_, /*to_file_end=*/false);
    }
  }
  ++non_check_line_;

  // Loop through remaining content.
  while (non_check_line_ != non_check_lines_.end()) {
    if (output_file_number_ < non_check_line_->file_number()) {
      FinishFile(/*is_last_file=*/false);
      StartSplitFile();
      if (output_file_number_ == autoupdate_split_file_) {
        break;
      }
      continue;
    }

    // STDERR check lines are placed before the line they refer to, or as
    // early as possible if they don't refer to a line. Include all STDERR
    // lines until we find one that wants to go later in the file.
    AddCheckLines(stderr_, /*to_file_end=*/false);
    AddRemappedNonCheckLine();

    // STDOUT check lines are placed after the line they refer to, or at the
    // end of the file if none of them refers to a line.
    if (any_attached_stdout_lines_) {
      AddCheckLines(stdout_, /*to_file_end=*/false);
    }

    ++non_check_line_;
  }

  // Clear out the autoupdate split, which would otherwise prevent check lines
  // being written to the autoupdate file. When autoupdate_split_file_ was set,
  // this will result in all check lines (and only check lines) being added to
  // the split by FinishFile. We don't use autoupdate_split_file_ past this
  // point.
  autoupdate_split_file_ = std::nullopt;

  FinishFile(/*is_last_file=*/true);

  for (auto& check_line : stdout_.lines) {
    check_line.RemapLineNumbers(file_to_number_map_, output_line_remap_,
                                new_last_line_numbers_);
  }
  for (auto& check_line : stderr_.lines) {
    check_line.RemapLineNumbers(file_to_number_map_, output_line_remap_,
                                new_last_line_numbers_);
  }

  // Generate the autoupdated file.
  RawStringOstream new_content_stream;
  for (const auto& line : new_lines_) {
    new_content_stream << *line << '\n';
  }
  std::string new_content = new_content_stream.TakeStr();

  // Update the file on disk if needed.
  if (new_content == input_content_) {
    return false;
  }
  if (!dry_run) {
    std::ofstream out(file_test_path_);
    out << new_content;
  }
  return true;
}

}  // namespace Carbon::Testing
