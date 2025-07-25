// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/base/canonical_value_store.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <string>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringRef.h"
#include "toolchain/base/value_ids.h"

namespace Carbon::Testing {
namespace {

using ::testing::Eq;
using ::testing::Not;

TEST(CanonicalValueStore, Float) {
  llvm::APFloat float1(1.0);
  llvm::APFloat float2(2.0);

  CanonicalValueStore<FloatId, llvm::APFloat> floats;
  FloatId id1 = floats.Add(float1);
  FloatId id2 = floats.Add(float2);

  ASSERT_TRUE(id1.has_value());
  ASSERT_TRUE(id2.has_value());
  EXPECT_THAT(id1, Not(Eq(id2)));

  EXPECT_THAT(floats.Get(id1).compare(float1), Eq(llvm::APFloatBase::cmpEqual));
  EXPECT_THAT(floats.Get(id2).compare(float2), Eq(llvm::APFloatBase::cmpEqual));
}

TEST(CanonicalValueStore, Identifiers) {
  std::string a = "a";
  std::string b = "b";
  CanonicalValueStore<IdentifierId, llvm::StringRef> identifiers;

  // Make sure reserve works, we use it with identifiers.
  identifiers.Reserve(100);

  auto a_id = identifiers.Add(a);
  auto b_id = identifiers.Add(b);

  ASSERT_TRUE(a_id.has_value());
  ASSERT_TRUE(b_id.has_value());
  EXPECT_THAT(a_id, Not(Eq(b_id)));

  EXPECT_THAT(identifiers.Get(a_id), Eq(a));
  EXPECT_THAT(identifiers.Get(b_id), Eq(b));

  EXPECT_THAT(identifiers.Lookup(a), Eq(a_id));
  EXPECT_THAT(identifiers.Lookup("c"), Eq(IdentifierId::None));
}

TEST(CanonicalValueStore, StringLiterals) {
  std::string a = "a";
  std::string b = "b";
  CanonicalValueStore<StringLiteralValueId, llvm::StringRef> string_literals;

  auto a_id = string_literals.Add(a);
  auto b_id = string_literals.Add(b);

  ASSERT_TRUE(a_id.has_value());
  ASSERT_TRUE(b_id.has_value());
  EXPECT_THAT(a_id, Not(Eq(b_id)));

  EXPECT_THAT(string_literals.Get(a_id), Eq(a));
  EXPECT_THAT(string_literals.Get(b_id), Eq(b));

  EXPECT_THAT(string_literals.Lookup(a), Eq(a_id));
  EXPECT_THAT(string_literals.Lookup("c"), Eq(StringLiteralValueId::None));
}

}  // namespace
}  // namespace Carbon::Testing
