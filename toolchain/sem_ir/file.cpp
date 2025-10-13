// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/sem_ir/file.h"

#include <optional>
#include <string>
#include <utility>

#include "clang/AST/Mangle.h"
#include "common/check.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "toolchain/base/kind_switch.h"
#include "toolchain/base/shared_value_stores.h"
#include "toolchain/base/yaml.h"
#include "toolchain/parse/node_ids.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/inst_kind.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::SemIR {

File::File(const Parse::Tree* parse_tree, CheckIRId check_ir_id,
           const std::optional<Parse::Tree::PackagingDecl>& packaging_decl,
           SharedValueStores& value_stores, std::string filename)
    : parse_tree_(parse_tree),
      check_ir_id_(check_ir_id),
      package_id_(packaging_decl ? packaging_decl->names.package_id
                                 : PackageNameId::None),
      library_id_(packaging_decl ? LibraryNameId::ForStringLiteralValueId(
                                       packaging_decl->names.library_id)
                                 : LibraryNameId::Default),
      value_stores_(&value_stores),
      filename_(std::move(filename)),
      classes_(IdTag(check_ir_id.index, 0)),
      associated_constants_(IdTag(check_ir_id.index, 0)),
      impls_(*this),
      // The `+1` prevents adding a tag to the global `NameSpace::PackageInstId`
      // instruction. It's not a "singleton" instruction, but it's a unique
      // instruction id that comes right after the singletons.
      insts_(this, SingletonInstKinds.size() + 1),
      constant_values_(ConstantId::NotConstant, &insts_),
      inst_blocks_(allocator_),
      constants_(this) {
  // `type` and the error type are both complete & concrete types.
  types_.SetComplete(
      TypeType::TypeId,
      {.value_repr = {.kind = ValueRepr::Copy, .type_id = TypeType::TypeId}});
  types_.SetComplete(
      ErrorInst::TypeId,
      {.value_repr = {.kind = ValueRepr::Copy, .type_id = ErrorInst::TypeId}});

  insts_.Reserve(SingletonInstKinds.size());
  for (auto kind : SingletonInstKinds) {
    auto inst_id =
        insts_.AddInNoBlock(LocIdAndInst::NoLoc(Inst::MakeSingleton(kind)));
    constant_values_.Set(inst_id, ConstantId::ForConcreteConstant(inst_id));
  }
}

File::~File() = default;

auto File::Verify() const -> ErrorOr<Success> {
  // Invariants don't necessarily hold for invalid IR.
  if (has_errors_) {
    return Success();
  }

  // Check that every code block has a terminator sequence that appears at the
  // end of the block.
  for (const Function& function : functions_.values()) {
    for (InstBlockId block_id : function.body_block_ids) {
      TerminatorKind prior_kind = TerminatorKind::NotTerminator;
      for (InstId inst_id : inst_blocks().Get(block_id)) {
        TerminatorKind inst_kind =
            insts().Get(inst_id).kind().terminator_kind();
        if (prior_kind == TerminatorKind::Terminator) {
          return Error(llvm::formatv("Inst {0} in block {1} follows terminator",
                                     inst_id, block_id));
        }
        if (prior_kind > inst_kind) {
          return Error(
              llvm::formatv("Non-terminator inst {0} in block {1} follows "
                            "terminator sequence",
                            inst_id, block_id));
        }
        prior_kind = inst_kind;
      }
      if (prior_kind != TerminatorKind::Terminator) {
        return Error(llvm::formatv("No terminator in block {0}", block_id));
      }
    }
  }

  // TODO: Check that an instruction only references other instructions that are
  // either global or that dominate it.
  return Success();
}

auto File::OutputYaml(bool include_singletons) const -> Yaml::OutputMapping {
  return Yaml::OutputMapping([this, include_singletons](
                                 Yaml::OutputMapping::Map map) {
    map.Add("filename", filename_);
    map.Add("sem_ir", Yaml::OutputMapping([&](Yaml::OutputMapping::Map map) {
              map.Add("import_irs", import_irs_.OutputYaml());
              map.Add("import_ir_insts", import_ir_insts_.OutputYaml());
              map.Add("clang_decls", clang_decls_.OutputYaml());
              map.Add("name_scopes", name_scopes_.OutputYaml());
              map.Add("entity_names", entity_names_.OutputYaml());
              map.Add("functions", functions_.OutputYaml());
              map.Add("classes", classes_.OutputYaml());
              map.Add("generics", generics_.OutputYaml());
              map.Add("specifics", specifics_.OutputYaml());
              map.Add("struct_type_fields", struct_type_fields_.OutputYaml());
              map.Add("types", types_.OutputYaml());
              map.Add("insts",
                      Yaml::OutputMapping([&](Yaml::OutputMapping::Map map) {
                        for (auto [id, inst] : insts_.enumerate()) {
                          if (!include_singletons && IsSingletonInstId(id)) {
                            continue;
                          }
                          map.Add(PrintToString(id), Yaml::OutputScalar(inst));
                        }
                      }));
              map.Add("constant_values",
                      constant_values_.OutputYaml(include_singletons));
              map.Add("inst_blocks", inst_blocks_.OutputYaml());
            }));
  });
}

auto File::CollectMemUsage(MemUsage& mem_usage, llvm::StringRef label) const
    -> void {
  mem_usage.Collect(MemUsage::ConcatLabel(label, "allocator_"), allocator_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "entity_names_"),
                    entity_names_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "functions_"), functions_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "classes_"), classes_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "interfaces_"), interfaces_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "impls_"), impls_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "generics_"), generics_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "specifics_"), specifics_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "import_irs_"), import_irs_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "import_ir_insts_"),
                    import_ir_insts_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "clang_decls_"), clang_decls_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "struct_type_fields_"),
                    struct_type_fields_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "insts_"), insts_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "name_scopes_"), name_scopes_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "constant_values_"),
                    constant_values_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "inst_blocks_"), inst_blocks_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "constants_"), constants_);
  mem_usage.Collect(MemUsage::ConcatLabel(label, "types_"), types_);
}

auto File::set_clang_ast_unit(clang::ASTUnit* clang_ast_unit) -> void {
  clang_ast_unit_ = clang_ast_unit;
  clang_mangle_context_.reset(
      clang_ast_unit->getASTContext().createMangleContext());
}

}  // namespace Carbon::SemIR
