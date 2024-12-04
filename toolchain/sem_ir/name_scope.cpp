// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/sem_ir/name_scope.h"

namespace Carbon::SemIR {

auto NameScope::AddRequired(Entry name_entry) -> void {
  auto add_name = [&] {
    int index = names_.size();
    names_.push_back(name_entry);
    return index;
  };
  auto result = name_map_.Insert(name_entry.name_id, add_name);
  CARBON_CHECK(result.is_inserted(), "Failed to add required name: {0}",
               name_entry.name_id);
}

auto NameScope::LookupOrAdd(SemIR::NameId name_id, InstId inst_id,
                            AccessKind access_kind)
    -> std::pair<bool, EntryId> {
  auto insert_result = name_map_.Insert(name_id, names_.size());
  if (!insert_result.is_inserted()) {
    return {false, EntryId(insert_result.value())};
  }

  names_.push_back(
      {.name_id = name_id, .inst_id = inst_id, .access_kind = access_kind});
  return {true, EntryId(names_.size() - 1)};
}

}  // namespace Carbon::SemIR
