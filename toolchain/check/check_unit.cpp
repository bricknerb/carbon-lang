// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/check_unit.h"

#include <iterator>
#include <string>
#include <tuple>
#include <utility>

#include "common/growing_range.h"
#include "common/pretty_stack_trace_function.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "toolchain/base/fixed_size_value_store.h"
#include "toolchain/base/kind_switch.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/check/generic.h"
#include "toolchain/check/handle.h"
#include "toolchain/check/impl.h"
#include "toolchain/check/impl_lookup.h"
#include "toolchain/check/impl_validation.h"
#include "toolchain/check/import.h"
#include "toolchain/check/import_cpp.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/node_id_traversal.h"
#include "toolchain/check/type.h"
#include "toolchain/check/type_structure.h"
#include "toolchain/diagnostics/diagnostic.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/import_ir.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

// Returns the number of imported IRs, to assist in Context construction.
static auto GetImportedIRCount(UnitAndImports* unit_and_imports) -> int {
  int count = 0;
  for (auto& package_imports : unit_and_imports->package_imports) {
    count += package_imports.imports.size();
  }
  if (!unit_and_imports->api_for_impl) {
    // Leave an empty slot for `ImportIRId::ApiForImpl`.
    ++count;
  }
  if (!unit_and_imports->cpp_import_names.empty()) {
    // Leave an empty slot for `ImportIRId::Cpp`.
    ++count;
  }
  return count;
}

CheckUnit::CheckUnit(
    UnitAndImports* unit_and_imports,
    const Parse::GetTreeAndSubtreesStore* tree_and_subtrees_getters,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs,
    std::shared_ptr<clang::CompilerInvocation> clang_invocation,
    llvm::raw_ostream* vlog_stream)
    : unit_and_imports_(unit_and_imports),
      tree_and_subtrees_getter_(tree_and_subtrees_getters->Get(
          unit_and_imports->unit->sem_ir->check_ir_id())),
      total_ir_count_(tree_and_subtrees_getters->size()),
      fs_(std::move(fs)),
      clang_invocation_(std::move(clang_invocation)),
      emitter_(&unit_and_imports_->err_tracker, tree_and_subtrees_getters,
               unit_and_imports_->unit->sem_ir),
      context_(
          &emitter_, tree_and_subtrees_getter_, unit_and_imports_->unit->sem_ir,
          GetImportedIRCount(unit_and_imports), total_ir_count_, vlog_stream) {}

auto CheckUnit::Run() -> void {
  Timings::ScopedTiming timing(unit_and_imports_->unit->timings, "check");

  // We can safely mark this as checked at the start.
  unit_and_imports_->is_checked = true;

  PrettyStackTraceFunction context_dumper(
      [&](llvm::raw_ostream& output) { context_.PrintForStackDump(output); });

  // Add a block for the file.
  context_.inst_block_stack().Push();

  InitPackageScopeAndImports();

  // Eagerly import the impls declared in the api file to prepare to redeclare
  // them.
  ImportImplsFromApiFile(context_);

  if (!ProcessNodeIds()) {
    context_.sem_ir().set_has_errors(true);
    return;
  }

  FinishRun();
}

auto CheckUnit::InitPackageScopeAndImports() -> void {
  // Importing makes many namespaces, so only canonicalize the type once.
  auto namespace_type_id =
      GetSingletonType(context_, SemIR::NamespaceType::TypeInstId);

  // Define the package scope, with an instruction for `package` expressions to
  // reference.
  auto package_scope_id = context_.name_scopes().Add(
      SemIR::Namespace::PackageInstId, SemIR::NameId::PackageNamespace,
      SemIR::NameScopeId::None);
  CARBON_CHECK(package_scope_id == SemIR::NameScopeId::Package);

  auto package_inst_id =
      AddInst<SemIR::Namespace>(context_, Parse::NodeId::None,
                                {.type_id = namespace_type_id,
                                 .name_scope_id = SemIR::NameScopeId::Package,
                                 .import_id = SemIR::InstId::None});
  CARBON_CHECK(package_inst_id == SemIR::Namespace::PackageInstId);

  // Call `SetSpecialImportIRs()` to set `ImportIRId::ApiForImpl` and
  // `ImportIRId::Cpp` first, as required.
  if (unit_and_imports_->api_for_impl) {
    const auto& names = context_.parse_tree().packaging_decl()->names;
    auto import_decl_id = AddInst<SemIR::ImportDecl>(
        context_, names.node_id,
        {.package_id = SemIR::NameId::ForPackageName(names.package_id)});
    SetSpecialImportIRs(
        context_, {.decl_id = import_decl_id,
                   .is_export = false,
                   .sem_ir = unit_and_imports_->api_for_impl->unit->sem_ir});
  } else {
    SetSpecialImportIRs(context_,
                        {.decl_id = SemIR::InstId::None, .sem_ir = nullptr});
  }

  // Add import instructions for everything directly imported. Implicit imports
  // are handled separately.
  for (auto& package_imports : unit_and_imports_->package_imports) {
    CARBON_CHECK(!package_imports.import_decl_id.has_value());
    package_imports.import_decl_id = AddInst<SemIR::ImportDecl>(
        context_, package_imports.node_id,
        {.package_id =
             SemIR::NameId::ForPackageName(package_imports.package_id)});
  }
  // Process the imports.
  if (unit_and_imports_->api_for_impl) {
    ImportApiFile(context_, namespace_type_id,
                  *unit_and_imports_->api_for_impl->unit->sem_ir);
  }
  ImportCurrentPackage(package_inst_id, namespace_type_id);
  CARBON_CHECK(context_.scope_stack().PeekIndex() == ScopeIndex::Package);
  ImportOtherPackages(namespace_type_id);

  const auto& cpp_import_names = unit_and_imports_->cpp_import_names;
  if (!cpp_import_names.empty()) {
    auto* cpp_ast = unit_and_imports_->unit->cpp_ast;
    CARBON_CHECK(cpp_ast);
    CARBON_CHECK(!cpp_ast->get());
    *cpp_ast =
        ImportCppFiles(context_, cpp_import_names, fs_, clang_invocation_);
  }
}

auto CheckUnit::CollectDirectImports(
    llvm::SmallVector<SemIR::ImportIR>& results,
    FixedSizeValueStore<SemIR::CheckIRId, int>& ir_to_result_index,
    SemIR::InstId import_decl_id, const PackageImports& imports, bool is_local)
    -> void {
  for (const auto& import : imports.imports) {
    const auto& direct_ir = *import.unit_info->unit->sem_ir;
    auto& index = ir_to_result_index.Get(direct_ir.check_ir_id());
    if (index != -1) {
      // This should only happen when doing API imports for an implementation
      // file. Don't change the entry; is_export doesn't matter.
      continue;
    }
    index = results.size();
    results.push_back({.decl_id = import_decl_id,
                       // Only tag exports in API files, ignoring the value in
                       // implementation files.
                       .is_export = is_local && import.names.is_export,
                       .sem_ir = &direct_ir});
  }
}

auto CheckUnit::CollectTransitiveImports(SemIR::InstId import_decl_id,
                                         const PackageImports* local_imports,
                                         const PackageImports* api_imports)
    -> llvm::SmallVector<SemIR::ImportIR> {
  llvm::SmallVector<SemIR::ImportIR> results;

  // Track whether an IR was imported in full, including `export import`. This
  // distinguishes from IRs that are indirectly added without all names being
  // exported to this IR.
  auto ir_to_result_index =
      FixedSizeValueStore<SemIR::CheckIRId, int>::MakeWithExplicitSize(
          total_ir_count_, -1);

  // First add direct imports. This means that if an entity is imported both
  // directly and indirectly, the import path will reflect the direct import.
  if (local_imports) {
    CollectDirectImports(results, ir_to_result_index, import_decl_id,
                         *local_imports,
                         /*is_local=*/true);
  }
  if (api_imports) {
    CollectDirectImports(results, ir_to_result_index, import_decl_id,
                         *api_imports,
                         /*is_local=*/false);
  }

  // Loop through direct imports for any indirect exports. The underlying vector
  // is appended during iteration, so take the size first.
  const int direct_imports = results.size();
  for (int direct_index : llvm::seq(direct_imports)) {
    bool is_export = results[direct_index].is_export;

    for (const auto& indirect_ir :
         results[direct_index].sem_ir->import_irs().values()) {
      if (!indirect_ir.is_export) {
        continue;
      }

      auto& indirect_index =
          ir_to_result_index.Get(indirect_ir.sem_ir->check_ir_id());
      if (indirect_index == -1) {
        indirect_index = results.size();
        // TODO: In the case of a recursive `export import`, this only points at
        // the outermost import. May want something that better reflects the
        // recursion.
        results.push_back({.decl_id = results[direct_index].decl_id,
                           .is_export = is_export,
                           .sem_ir = indirect_ir.sem_ir});
      } else if (is_export) {
        results[indirect_index].is_export = true;
      }
    }
  }

  return results;
}

auto CheckUnit::ImportCurrentPackage(SemIR::InstId package_inst_id,
                                     SemIR::TypeId namespace_type_id) -> void {
  // Add imports from the current package.
  auto import_map_lookup =
      unit_and_imports_->package_imports_map.Lookup(PackageNameId::None);
  if (!import_map_lookup) {
    // Push the scope; there are no names to add.
    context_.scope_stack().PushForEntity(
        package_inst_id, SemIR::NameScopeId::Package, SemIR::SpecificId::None,
        /*lexical_lookup_has_load_error=*/false);
    return;
  }
  PackageImports& self_import =
      unit_and_imports_->package_imports[import_map_lookup.value()];

  if (self_import.has_load_error) {
    context_.name_scopes().Get(SemIR::NameScopeId::Package).set_has_error();
  }

  ImportLibrariesFromCurrentPackage(
      context_, namespace_type_id,
      CollectTransitiveImports(self_import.import_decl_id, &self_import,
                               /*api_imports=*/nullptr));

  context_.scope_stack().PushForEntity(
      package_inst_id, SemIR::NameScopeId::Package, SemIR::SpecificId::None,
      context_.name_scopes().Get(SemIR::NameScopeId::Package).has_error());
}

auto CheckUnit::ImportOtherPackages(SemIR::TypeId namespace_type_id) -> void {
  // api_imports_list is initially the size of the current file's imports,
  // including for API files, for simplicity in iteration. It's only really used
  // when processing an implementation file, in order to combine the API file
  // imports.
  //
  // For packages imported by the API file, the PackageNameId is the package
  // name and the index is into the API's import list. Otherwise, the initial
  // {None, -1} state remains.
  llvm::SmallVector<std::pair<PackageNameId, int32_t>> api_imports_list;
  api_imports_list.resize(unit_and_imports_->package_imports.size(),
                          {PackageNameId::None, -1});

  // When there's an API file, add the mapping to api_imports_list.
  if (unit_and_imports_->api_for_impl) {
    const auto& api_identifiers =
        unit_and_imports_->api_for_impl->unit->value_stores->identifiers();
    auto& impl_identifiers =
        unit_and_imports_->unit->value_stores->identifiers();

    for (auto [api_imports_index, api_imports] :
         llvm::enumerate(unit_and_imports_->api_for_impl->package_imports)) {
      // Skip the current package.
      if (!api_imports.package_id.has_value()) {
        continue;
      }

      // Translate the package ID from the API file to the implementation file.
      auto impl_package_id = api_imports.package_id;
      if (auto package_identifier_id = impl_package_id.AsIdentifierId();
          package_identifier_id.has_value()) {
        impl_package_id = PackageNameId::ForIdentifier(
            impl_identifiers.Add(api_identifiers.Get(package_identifier_id)));
      }

      if (auto lookup =
              unit_and_imports_->package_imports_map.Lookup(impl_package_id)) {
        // On a hit, replace the entry to unify the API and implementation
        // imports.
        api_imports_list[lookup.value()] = {impl_package_id, api_imports_index};
      } else {
        // On a miss, add the package as API-only.
        api_imports_list.push_back({impl_package_id, api_imports_index});
      }
    }
  }

  for (auto [i, api_imports_entry] : llvm::enumerate(api_imports_list)) {
    // These variables are updated after figuring out which imports are present.
    auto import_decl_id = SemIR::InstId::None;
    PackageNameId package_id = PackageNameId::None;
    bool has_load_error = false;

    // Identify the local package imports if present.
    PackageImports* local_imports = nullptr;
    if (i < unit_and_imports_->package_imports.size()) {
      local_imports = &unit_and_imports_->package_imports[i];
      if (!local_imports->package_id.has_value()) {
        // Skip the current package.
        continue;
      }
      import_decl_id = local_imports->import_decl_id;

      package_id = local_imports->package_id;
      has_load_error |= local_imports->has_load_error;
    }

    // Identify the API package imports if present.
    PackageImports* api_imports = nullptr;
    if (api_imports_entry.second != -1) {
      api_imports = &unit_and_imports_->api_for_impl
                         ->package_imports[api_imports_entry.second];

      if (local_imports) {
        CARBON_CHECK(package_id == api_imports_entry.first);
      } else {
        auto import_ir_inst_id =
            context_.import_ir_insts().Add(SemIR::ImportIRInst(
                SemIR::ImportIRId::ApiForImpl, api_imports->import_decl_id));
        import_decl_id =
            AddInst(context_, MakeImportedLocIdAndInst<SemIR::ImportDecl>(
                                  context_, import_ir_inst_id,
                                  {.package_id = SemIR::NameId::ForPackageName(
                                       api_imports_entry.first)}));
        package_id = api_imports_entry.first;
      }
      has_load_error |= api_imports->has_load_error;
    }

    // Do the actual import.
    ImportLibrariesFromOtherPackage(
        context_, namespace_type_id, import_decl_id, package_id,
        CollectTransitiveImports(import_decl_id, local_imports, api_imports),
        has_load_error);
  }
}

// Loops over all nodes in the tree. On some errors, this may return early,
// for example if an unrecoverable state is encountered.
// NOLINTNEXTLINE(readability-function-size)
auto CheckUnit::ProcessNodeIds() -> bool {
  NodeIdTraversal traversal(&context_);

  Parse::NodeId node_id = Parse::NodeId::None;

  // On crash, report which token we were handling.
  PrettyStackTraceFunction node_dumper([&](llvm::raw_ostream& output) {
    const auto& tree = tree_and_subtrees_getter_();
    auto converted = tree.NodeToDiagnosticLoc(node_id, /*token_only=*/false);
    converted.loc.FormatLocation(output);
    output << "Checking " << context_.parse_tree().node_kind(node_id) << "\n";
    // Crash output has a tab indent; try to indent slightly past that.
    converted.loc.FormatSnippet(output, /*indent=*/10);
  });

  while (auto maybe_node_id = traversal.Next()) {
    node_id = *maybe_node_id;

    emitter_.AdvanceToken(context_.parse_tree().node_token(node_id));

    if (context_.parse_tree().node_has_error(node_id)) {
      context_.TODO(node_id, "handle invalid parse trees in `check`");
      return false;
    }

    bool result;
    auto parse_kind = context_.parse_tree().node_kind(node_id);
    switch (parse_kind) {
#define CARBON_PARSE_NODE_KIND(Name)                                   \
  case Parse::NodeKind::Name: {                                        \
    result = HandleParseNode(                                          \
        context_, context_.parse_tree().As<Parse::Name##Id>(node_id)); \
    break;                                                             \
  }
#include "toolchain/parse/node_kind.def"
    }

    if (!result) {
      CARBON_CHECK(
          unit_and_imports_->err_tracker.seen_error(),
          "HandleParseNode for `{0}` returned false without diagnosing.",
          parse_kind);
      return false;
    }
    traversal.Handle(parse_kind);
  }
  return true;
}

auto CheckUnit::CheckRequiredDeclarations() -> void {
  for (const auto& function : context_.functions().values()) {
    if (!function.first_owning_decl_id.has_value() &&
        function.extern_library_id == context_.sem_ir().library_id()) {
      auto function_import_id =
          context_.insts().GetImportSource(function.non_owning_decl_id);
      CARBON_CHECK(function_import_id.has_value());
      auto import_ir_id =
          context_.sem_ir().import_ir_insts().Get(function_import_id).ir_id();
      auto& import_ir = context_.import_irs().Get(import_ir_id);
      if (import_ir.sem_ir->package_id().has_value() !=
          context_.sem_ir().package_id().has_value()) {
        continue;
      }

      CARBON_DIAGNOSTIC(
          MissingOwningDeclarationInApi, Error,
          "owning declaration required for non-owning declaration");
      if (!import_ir.sem_ir->package_id().has_value() &&
          !context_.sem_ir().package_id().has_value()) {
        emitter_.Emit(function.non_owning_decl_id,
                      MissingOwningDeclarationInApi);
        continue;
      }

      if (import_ir.sem_ir->identifiers().Get(
              import_ir.sem_ir->package_id().AsIdentifierId()) ==
          context_.sem_ir().identifiers().Get(
              context_.sem_ir().package_id().AsIdentifierId())) {
        emitter_.Emit(function.non_owning_decl_id,
                      MissingOwningDeclarationInApi);
      }
    }
  }
}

auto CheckUnit::CheckRequiredDefinitions() -> void {
  CARBON_DIAGNOSTIC(MissingDefinitionInImpl, Error,
                    "no definition found for declaration in impl file");
  for (SemIR::InstId decl_inst_id : context_.definitions_required_by_decl()) {
    SemIR::Inst decl_inst = context_.insts().Get(decl_inst_id);
    CARBON_KIND_SWITCH(context_.insts().Get(decl_inst_id)) {
      case CARBON_KIND(SemIR::ClassDecl class_decl): {
        if (!context_.classes().Get(class_decl.class_id).is_complete()) {
          emitter_.Emit(decl_inst_id, MissingDefinitionInImpl);
        }
        break;
      }
      case CARBON_KIND(SemIR::FunctionDecl function_decl): {
        if (context_.functions().Get(function_decl.function_id).definition_id ==
            SemIR::InstId::None) {
          emitter_.Emit(decl_inst_id, MissingDefinitionInImpl);
        }
        break;
      }
      case CARBON_KIND(SemIR::ImplDecl impl_decl): {
        auto& impl = context_.impls().Get(impl_decl.impl_id);
        if (!impl.is_complete()) {
          FillImplWitnessWithErrors(context_, impl);
          CARBON_DIAGNOSTIC(ImplMissingDefinition, Error,
                            "impl declared but not defined");
          emitter_.Emit(decl_inst_id, ImplMissingDefinition);
        }
        break;
      }
      case SemIR::InterfaceDecl::Kind: {
        // TODO: Handle `interface` as well, once we can test it without
        // triggering
        // https://github.com/carbon-language/carbon-lang/issues/4071.
        CARBON_FATAL("TODO: Support interfaces in DiagnoseMissingDefinitions");
      }
      default: {
        CARBON_FATAL("Unexpected inst in definitions_required_by_decl: {0}",
                     decl_inst);
      }
    }
  }

  for (auto [loc, specific_id] :
       GrowingRange(context_.definitions_required_by_use())) {
    // This is using the location for the use. We could track the
    // list of enclosing locations if this was used from a generic.
    if (!ResolveSpecificDefinition(context_, loc, specific_id)) {
      CARBON_DIAGNOSTIC(MissingGenericFunctionDefinition, Error,
                        "use of undefined generic function");
      CARBON_DIAGNOSTIC(MissingGenericFunctionDefinitionHere, Note,
                        "generic function declared here");
      auto generic_decl_id =
          context_.generics()
              .Get(context_.specifics().Get(specific_id).generic_id)
              .decl_id;
      emitter_.Build(loc, MissingGenericFunctionDefinition)
          .Note(generic_decl_id, MissingGenericFunctionDefinitionHere)
          .Emit();
    }
  }
}

auto CheckUnit::CheckPoisonedConcreteImplLookupQueries() -> void {
  // Impl lookup can generate instructions (via deduce) which we don't use, as
  // we're only generating diagnostics here, so we catch and discard them.
  context_.inst_block_stack().Push();
  auto poisoned_queries =
      std::exchange(context_.poisoned_concrete_impl_lookup_queries(), {});
  for (const auto& poison : poisoned_queries) {
    auto witness_result =
        EvalLookupSingleImplWitness(context_, poison.loc_id, poison.query,
                                    poison.non_canonical_query_self_inst_id,
                                    /*poison_concrete_results=*/false);
    CARBON_CHECK(witness_result.has_concrete_value());
    auto found_witness_id = witness_result.concrete_witness();
    if (found_witness_id != poison.impl_witness) {
      auto witness_to_impl_id = [&](SemIR::InstId witness_id) {
        auto table_id = context_.insts()
                            .GetAs<SemIR::ImplWitness>(witness_id)
                            .witness_table_id;
        return context_.insts()
            .GetAs<SemIR::ImplWitnessTable>(table_id)
            .impl_id;
      };

      // We can get the `Impl` from the resulting witness here, which is the
      // `Impl` that conflicts with the previous poison query.
      auto bad_impl_id = witness_to_impl_id(found_witness_id);
      const auto& bad_impl = context_.impls().Get(bad_impl_id);

      auto prev_impl_id = witness_to_impl_id(poison.impl_witness);
      const auto& prev_impl = context_.impls().Get(prev_impl_id);

      CARBON_DIAGNOSTIC(
          PoisonedImplLookupConcreteResult, Error,
          "found `impl` that would change the result of an earlier "
          "use of `{0} as {1}`",
          InstIdAsRawType, SpecificInterfaceIdAsRawType);
      auto builder =
          emitter_.Build(poison.loc_id, PoisonedImplLookupConcreteResult,
                         poison.query.query_self_inst_id,
                         poison.query.query_specific_interface_id);
      CARBON_DIAGNOSTIC(
          PoisonedImplLookupConcreteResultNoteBadImpl, Note,
          "the use would select the `impl` here but it was not found yet");
      builder.Note(bad_impl.first_decl_id(),
                   PoisonedImplLookupConcreteResultNoteBadImpl);
      CARBON_DIAGNOSTIC(PoisonedImplLookupConcreteResultNotePreviousImpl, Note,
                        "the use had selected the `impl` here");
      builder.Note(prev_impl.first_decl_id(),
                   PoisonedImplLookupConcreteResultNotePreviousImpl);
      builder.Emit();
    }
  }
  context_.inst_block_stack().PopAndDiscard();
}

auto CheckUnit::CheckImpls() -> void { ValidateImplsInFile(context_); }

auto CheckUnit::FinishRun() -> void {
  CheckRequiredDeclarations();
  CheckRequiredDefinitions();
  CheckPoisonedConcreteImplLookupQueries();
  CheckImpls();

  // Pop information for the file-level scope.
  context_.sem_ir().set_top_inst_block_id(context_.inst_block_stack().Pop());
  context_.scope_stack().Pop();

  // Finalizes the list of exports on the IR.
  context_.inst_blocks().ReplacePlaceholder(SemIR::InstBlockId::Exports,
                                            context_.exports());
  // Finalizes the ImportRef inst block.
  context_.inst_blocks().ReplacePlaceholder(SemIR::InstBlockId::Imports,
                                            context_.imports());
  // Finalizes __global_init.
  context_.global_init().Finalize();

  context_.sem_ir().set_has_errors(unit_and_imports_->err_tracker.seen_error());

  // Verify that Context cleanly finished.
  context_.VerifyOnFinish();
}

}  // namespace Carbon::Check
