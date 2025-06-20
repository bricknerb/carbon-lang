// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/sem_ir/absolute_node_id.h"

#include "toolchain/sem_ir/ids.h"

namespace Carbon::SemIR {

// Follows an imported instruction location to find the sequence of import
// locations and the ultimately imported location.
static auto FollowImportRef(
    llvm::SmallVector<AbsoluteNodeId>& absolute_node_ids,
    const File*& cursor_ir, InstId& cursor_inst_id,
    ImportIRInstId import_ir_inst_id) -> bool {
  auto import_ir_inst = cursor_ir->import_ir_insts().Get(import_ir_inst_id);
  if (import_ir_inst.ir_id() == ImportIRId::Cpp) {
    CARBON_CHECK(cursor_ir->import_cpps().size() > 0);
    // TODO: Decompose the Clang source location to determine which C++ import
    // made this location available, and use the location of that import instead
    // of arbitrarily using the first C++ import.
    absolute_node_ids.push_back(
        AbsoluteNodeId(cursor_ir->check_ir_id(),
                       cursor_ir->import_cpps().values().begin()->node_id));
    absolute_node_ids.push_back(
        AbsoluteNodeId(import_ir_inst.clang_source_loc_id()));
    return true;
  }

  const auto& import_ir = cursor_ir->import_irs().Get(import_ir_inst.ir_id());
  CARBON_CHECK(import_ir.decl_id.has_value(),
               "If we get `None` locations here, we may need to more "
               "thoroughly track ImportDecls.");

  auto import_loc_id = cursor_ir->insts().GetCanonicalLocId(import_ir.decl_id);
  switch (import_loc_id.kind()) {
    case LocId::Kind::None:
      break;

    case LocId::Kind::ImportIRInstId: {
      // For implicit imports, we need to unravel the location a little
      // further.
      auto implicit_import_ir_inst =
          cursor_ir->import_ir_insts().Get(import_loc_id.import_ir_inst_id());
      const auto& implicit_ir =
          cursor_ir->import_irs().Get(implicit_import_ir_inst.ir_id());
      auto implicit_loc_id = implicit_ir.sem_ir->insts().GetCanonicalLocId(
          implicit_import_ir_inst.inst_id());
      CARBON_CHECK(implicit_loc_id.kind() == LocId::Kind::NodeId,
                   "Should only be one layer of implicit imports");
      absolute_node_ids.push_back(AbsoluteNodeId(
          implicit_ir.sem_ir->check_ir_id(), implicit_loc_id.node_id()));
      break;
    }

    case LocId::Kind::InstId:
      CARBON_FATAL("Unexpected LocId: {0}", import_loc_id);

    case LocId::Kind::NodeId: {
      // For imports in the current file, the location is simple.
      absolute_node_ids.push_back(
          AbsoluteNodeId(cursor_ir->check_ir_id(), import_loc_id.node_id()));
      break;
    }
  }

  cursor_ir = import_ir.sem_ir;
  cursor_inst_id = import_ir_inst.inst_id();
  return false;
}

// Returns true if this is the final parse node location. If the location is an
// import, follows it and returns false.
static auto HandleLocId(llvm::SmallVector<AbsoluteNodeId>& absolute_node_ids,
                        const File*& cursor_ir, InstId& cursor_inst_id,
                        LocId loc_id) -> bool {
  switch (loc_id.kind()) {
    case LocId::Kind::ImportIRInstId: {
      return FollowImportRef(absolute_node_ids, cursor_ir, cursor_inst_id,
                             loc_id.import_ir_inst_id());
    }

    case LocId::Kind::NodeId: {
      // Parse nodes always refer to the current IR.
      absolute_node_ids.push_back(
          AbsoluteNodeId(cursor_ir->check_ir_id(), loc_id.node_id()));
      return true;
    }

    case LocId::Kind::None:
    case LocId::Kind::InstId:
      CARBON_FATAL("Unexpected LocId: {0}", loc_id);
  }
}

// Loops through imported instructions until the actual instruction is found.
static auto GetAbsoluteNodeIdImpl(
    llvm::SmallVector<AbsoluteNodeId>& absolute_node_ids, const File* cursor_ir,
    InstId cursor_inst_id) -> void {
  while (cursor_inst_id.has_value()) {
    auto cursor_inst = cursor_ir->insts().Get(cursor_inst_id);
    if (auto bind_ref = cursor_inst.TryAs<ExportDecl>();
        bind_ref && bind_ref->value_id.has_value()) {
      cursor_inst_id = bind_ref->value_id;
      continue;
    }

    // If the parse node has a value, use it for the location.
    if (auto loc_id = cursor_ir->insts().GetCanonicalLocId(cursor_inst_id);
        loc_id.has_value()) {
      if (HandleLocId(absolute_node_ids, cursor_ir, cursor_inst_id, loc_id)) {
        return;
      }
      continue;
    }

    // If a namespace has an instruction for an import, switch to looking at it.
    if (auto ns = cursor_inst.TryAs<Namespace>()) {
      if (ns->import_id.has_value()) {
        cursor_inst_id = ns->import_id;
        continue;
      }
    }
    break;
  }

  // `None` parse node but not an import; just nothing to point at.
  absolute_node_ids.push_back(
      AbsoluteNodeId(cursor_ir->check_ir_id(), Parse::NodeId::None));
}

auto GetAbsoluteNodeId(const File* sem_ir, LocId loc_id)
    -> llvm::SmallVector<AbsoluteNodeId> {
  llvm::SmallVector<AbsoluteNodeId> absolute_node_ids;
  switch (loc_id.kind()) {
    case LocId::Kind::None:
      absolute_node_ids.push_back(
          AbsoluteNodeId(sem_ir->check_ir_id(), Parse::NodeId::None));
      break;

    case LocId::Kind::InstId:
      GetAbsoluteNodeIdImpl(absolute_node_ids, sem_ir, loc_id.inst_id());
      break;

    case LocId::Kind::ImportIRInstId:
    case LocId::Kind::NodeId: {
      const File* cursor_ir = sem_ir;
      InstId cursor_inst_id = InstId::None;
      if (HandleLocId(absolute_node_ids, cursor_ir, cursor_inst_id,
                      cursor_ir->insts().GetCanonicalLocId(loc_id))) {
        break;
      }
      CARBON_CHECK(cursor_inst_id.has_value(), "Should be set by HandleLocId");
      GetAbsoluteNodeIdImpl(absolute_node_ids, cursor_ir, cursor_inst_id);
      break;
    }
  }
  return absolute_node_ids;
}

}  // namespace Carbon::SemIR
