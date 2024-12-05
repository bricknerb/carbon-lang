// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/sem_ir/name_scope.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace Carbon::SemIR {
namespace {

using ::testing::ElementsAre;
using ::testing::Field;
using ::testing::Pair;

MATCHER_P(NameScopeEntryEquals, entry, "") {
  return ExplainMatchResult(
      AllOf(Field("name_id", &NameScope::Entry::name_id, entry.name_id),
            Field("inst_id", &NameScope::Entry::inst_id, entry.inst_id),
            Field("access_kind", &NameScope::Entry::access_kind,
                  entry.access_kind)),
      arg, result_listener);
}

TEST(NameScope, Empty) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id(++id);
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_THAT(name_scope.entries(), ElementsAre());
  EXPECT_THAT(name_scope.extended_scopes(), ElementsAre());
  EXPECT_EQ(name_scope.inst_id(), scope_inst_id);
  EXPECT_EQ(name_scope.name_id(), scope_name_id);
  EXPECT_EQ(name_scope.parent_scope_id(), parent_scope_id);
  EXPECT_FALSE(name_scope.has_error());
  EXPECT_FALSE(name_scope.is_closed_import());
  EXPECT_FALSE(name_scope.is_imported_package());
  EXPECT_THAT(name_scope.import_ir_scopes(), ElementsAre());
}

TEST(NameScope, Lookup) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id(++id);
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  NameScope::Entry entry1 = {.name_id = NameId(++id),
                             .inst_id = InstId(++id),
                             .access_kind = AccessKind::Public};
  name_scope.AddRequired(entry1);

  NameScope::Entry entry2 = {.name_id = NameId(++id),
                             .inst_id = InstId(++id),
                             .access_kind = AccessKind::Protected};
  name_scope.AddRequired(entry2);

  NameScope::Entry entry3 = {.name_id = NameId(++id),
                             .inst_id = InstId(++id),
                             .access_kind = AccessKind::Private};
  name_scope.AddRequired(entry3);

  auto lookup = name_scope.Lookup(entry1.name_id);
  ASSERT_NE(lookup, std::nullopt);
  EXPECT_THAT(static_cast<NameScope&>(name_scope).GetEntry(*lookup),
              NameScopeEntryEquals(entry1));
  EXPECT_THAT(static_cast<const NameScope&>(name_scope).GetEntry(*lookup),
              NameScopeEntryEquals(entry1));

  lookup = name_scope.Lookup(entry2.name_id);
  ASSERT_NE(lookup, std::nullopt);
  EXPECT_THAT(name_scope.GetEntry(*lookup), NameScopeEntryEquals(entry2));

  lookup = name_scope.Lookup(entry3.name_id);
  ASSERT_NE(lookup, std::nullopt);
  EXPECT_THAT(name_scope.GetEntry(*lookup), NameScopeEntryEquals(entry3));

  NameId unknown_name_id(++id);
  lookup = name_scope.Lookup(unknown_name_id);
  EXPECT_EQ(lookup, std::nullopt);
}

TEST(NameScope, LookupOrAdd) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id(++id);
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  NameScope::Entry entry1 = {.name_id = NameId(++id),
                             .inst_id = InstId(++id),
                             .access_kind = AccessKind::Public};
  {
    auto [added, entry_id] = name_scope.LookupOrAdd(
        entry1.name_id, entry1.inst_id, entry1.access_kind);
    EXPECT_TRUE(added);
    EXPECT_THAT(name_scope.GetEntry(entry_id), NameScopeEntryEquals(entry1));
  }

  NameScope::Entry entry2 = {.name_id = NameId(++id),
                             .inst_id = InstId(++id),
                             .access_kind = AccessKind::Protected};
  {
    auto [added, entry_id] = name_scope.LookupOrAdd(
        entry2.name_id, entry2.inst_id, entry2.access_kind);
    EXPECT_TRUE(added);
    EXPECT_THAT(name_scope.GetEntry(entry_id), NameScopeEntryEquals(entry2));
  }

  NameScope::Entry entry3 = {.name_id = NameId(++id),
                             .inst_id = InstId(++id),
                             .access_kind = AccessKind::Private};
  {
    auto [added, entry_id] = name_scope.LookupOrAdd(
        entry3.name_id, entry3.inst_id, entry3.access_kind);
    EXPECT_TRUE(added);
    EXPECT_THAT(name_scope.GetEntry(entry_id), NameScopeEntryEquals(entry3));
  }

  {
    auto [added, entry_id] = name_scope.LookupOrAdd(
        entry1.name_id, entry1.inst_id, entry1.access_kind);
    EXPECT_FALSE(added);
    EXPECT_THAT(name_scope.GetEntry(entry_id), NameScopeEntryEquals(entry1));
  }

  {
    auto [added, entry_id] = name_scope.LookupOrAdd(
        entry2.name_id, entry2.inst_id, entry2.access_kind);
    EXPECT_FALSE(added);
    EXPECT_THAT(name_scope.GetEntry(entry_id), NameScopeEntryEquals(entry2));
  }

  {
    auto [added, entry_id] = name_scope.LookupOrAdd(
        entry3.name_id, entry3.inst_id, entry3.access_kind);
    EXPECT_FALSE(added);
    EXPECT_THAT(name_scope.GetEntry(entry_id), NameScopeEntryEquals(entry3));
  }
}

TEST(NameScope, ExtendedScopes) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id = NameScopeId::Package;
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_THAT(name_scope.extended_scopes(), ElementsAre());

  InstId extended_scope1(++id);
  name_scope.AddExtendedScope(extended_scope1);
  EXPECT_THAT(name_scope.extended_scopes(), ElementsAre(extended_scope1));

  InstId extended_scope2(++id);
  name_scope.AddExtendedScope(extended_scope2);
  EXPECT_THAT(name_scope.extended_scopes(),
              ElementsAre(extended_scope1, extended_scope2));
}

TEST(NameScope, HasError) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id(++id);
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_FALSE(name_scope.has_error());

  name_scope.set_has_error();
  EXPECT_TRUE(name_scope.has_error());

  name_scope.set_has_error();
  EXPECT_TRUE(name_scope.has_error());
}

TEST(NameScope, IsClosedImport) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id(++id);
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_FALSE(name_scope.is_closed_import());

  name_scope.set_is_closed_import(true);
  EXPECT_TRUE(name_scope.is_closed_import());

  name_scope.set_is_closed_import(false);
  EXPECT_FALSE(name_scope.is_closed_import());
}

TEST(NameScope, IsImportedPackageParentNonPackageScope) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id(++id);
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_FALSE(name_scope.is_imported_package());

  name_scope.set_is_closed_import(true);
  EXPECT_FALSE(name_scope.is_imported_package());

  name_scope.set_is_closed_import(false);
  EXPECT_FALSE(name_scope.is_imported_package());
}

TEST(NameScope, IsImportedPackageParentPackageScope) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id = NameScopeId::Package;
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_FALSE(name_scope.is_imported_package());

  name_scope.set_is_closed_import(true);
  EXPECT_TRUE(name_scope.is_imported_package());

  name_scope.set_is_closed_import(false);
  EXPECT_FALSE(name_scope.is_imported_package());
}

TEST(NameScope, ImportIRScopes) {
  int id = 0;

  InstId scope_inst_id(++id);
  NameId scope_name_id(++id);
  NameScopeId parent_scope_id = NameScopeId::Package;
  NameScope name_scope(scope_inst_id, scope_name_id, parent_scope_id);

  EXPECT_THAT(name_scope.import_ir_scopes(), ElementsAre());

  ImportIRId import_ir_id1(++id);
  NameScopeId import_name_scope_id1(++id);
  name_scope.AddImportIRScope({import_ir_id1, import_name_scope_id1});
  EXPECT_THAT(name_scope.import_ir_scopes(),
              ElementsAre(Pair(import_ir_id1, import_name_scope_id1)));

  ImportIRId import_ir_id2(++id);
  NameScopeId import_name_scope_id2(++id);
  name_scope.AddImportIRScope({import_ir_id2, import_name_scope_id2});
  EXPECT_THAT(name_scope.import_ir_scopes(),
              ElementsAre(Pair(import_ir_id1, import_name_scope_id1),
                          Pair(import_ir_id2, import_name_scope_id2)));
}

}  // namespace
}  // namespace Carbon::SemIR
