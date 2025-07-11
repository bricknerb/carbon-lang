// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/testing/compile_helper.h"

#include <string>
#include <utility>

namespace Carbon::Testing {

auto CompileHelper::GetTokenizedBuffer(llvm::StringRef text,
                                       Diagnostics::Consumer* consumer)
    -> Lex::TokenizedBuffer& {
  auto& source = GetSourceBuffer(text);

  value_store_storage_.emplace_front();
  Lex::LexOptions options;
  options.consumer = consumer ? consumer : &consumer_;
  token_storage_.push_front(
      Lex::Lex(value_store_storage_.front(), source, options));
  return token_storage_.front();
}

auto CompileHelper::GetTokenizedBufferWithSharedValueStore(
    llvm::StringRef text, Diagnostics::Consumer* consumer)
    -> std::pair<Lex::TokenizedBuffer&, SharedValueStores&> {
  auto& tokens = GetTokenizedBuffer(text, consumer);
  return {tokens, value_store_storage_.front()};
}

auto CompileHelper::GetTree(llvm::StringRef text) -> Parse::Tree& {
  auto& tokens = GetTokenizedBuffer(text);
  Parse::ParseOptions options;
  options.consumer = &consumer_;
  tree_storage_.push_front(Parse::Parse(tokens, options));
  return tree_storage_.front();
}

auto CompileHelper::GetTreeAndSubtrees(llvm::StringRef text)
    -> Parse::TreeAndSubtrees& {
  auto& tree = GetTree(text);
  tree_and_subtrees_storage_.push_front(
      Parse::TreeAndSubtrees(token_storage_.front(), tree));
  return tree_and_subtrees_storage_.front();
}

auto CompileHelper::GetTokenizedBufferWithTreeAndSubtrees(llvm::StringRef text)
    -> std::pair<Lex::TokenizedBuffer&, Parse::TreeAndSubtrees&> {
  auto& tree_and_subtrees = GetTreeAndSubtrees(text);
  return {token_storage_.front(), tree_and_subtrees};
}

auto CompileHelper::GetSourceBuffer(llvm::StringRef text) -> SourceBuffer& {
  std::string filename = llvm::formatv("test{0}.carbon", ++file_index_);
  CARBON_CHECK(
      fs_.addFile(filename, /*ModificationTime=*/0,
                  llvm::MemoryBuffer::getMemBuffer(
                      text, filename, /*RequiresNullTerminator=*/false)));
  source_storage_.push_front(
      std::move(*SourceBuffer::MakeFromFile(fs_, filename, consumer_)));
  return source_storage_.front();
}

}  // namespace Carbon::Testing
