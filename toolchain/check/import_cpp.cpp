// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/import_cpp.h"

#include <memory>
#include <optional>
#include <string>

#include "clang/Frontend/TextDiagnostic.h"
#include "clang/Sema/Lookup.h"
#include "clang/Tooling/Tooling.h"
#include "common/raw_string_ostream.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "toolchain/check/context.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/check/import.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/literal.h"
#include "toolchain/check/type.h"
#include "toolchain/diagnostics/diagnostic.h"
#include "toolchain/diagnostics/format_providers.h"
#include "toolchain/parse/node_ids.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/name_scope.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

// Generates C++ file contents to #include all requested imports.
static auto GenerateCppIncludesHeaderCode(
    Context& context, llvm::ArrayRef<Parse::Tree::PackagingNames> imports)
    -> std::string {
  std::string code;
  llvm::raw_string_ostream code_stream(code);
  for (const Parse::Tree::PackagingNames& import : imports) {
    code_stream << "#include \""
                << FormatEscaped(
                       context.string_literal_values().Get(import.library_id))
                << "\"\n";
  }
  return code;
}

namespace {

// Adds a `ClangDiagnostic` instruction which points on the source location
// pointed by `info`.
static auto AddClangDiagnosticInst(Context& context,
                                   const clang::Diagnostic& info) {
  SemIR::ClangSourceLocationId clang_source_location_id =
      context.sem_ir().clang_source_location_ids().Add(info.getLocation());
  return AddInst(context, SemIR::LocIdAndInst::NoLoc<SemIR::ClangDiagnostic>(
                              {.clang_loc_id = clang_source_location_id}));
}

// Used to convert Clang diagnostics to Carbon diagnostics.
class CarbonClangDiagnosticConsumer : public clang::DiagnosticConsumer {
 public:
  // Creates an instance with the location that triggers calling Clang.
  // `context` must not be null.
  explicit CarbonClangDiagnosticConsumer(Context* context, SemIRLoc loc)
      : context_(context), loc_(loc) {}

  // Generates a Carbon warning for each Clang warning and a Carbon error for
  // each Clang error or fatal.
  auto HandleDiagnostic(clang::DiagnosticsEngine::Level diag_level,
                        const clang::Diagnostic& info) -> void override {
    DiagnosticConsumer::HandleDiagnostic(diag_level, info);

    SemIR::InstId clang_diagnostic_inst_id =
        AddClangDiagnosticInst(*context_, info);

    llvm::SmallString<256> message;
    info.FormatDiagnostic(message);

    RawStringOstream diagnostics_stream;
    clang::TextDiagnostic text_diagnostic(
        diagnostics_stream,
        // TODO: Consider allowing setting `LangOptions` or use
        // `ASTContext::getLangOptions()`.
        clang::LangOptions(),
        // TODO: Consider allowing setting `DiagnosticOptions` or use
        // `ASTUnit::getDiagnostics().::getLangOptions().getDiagnosticOptions()`.
        new clang::DiagnosticOptions());
    text_diagnostic.emitDiagnostic(
        clang::FullSourceLoc(info.getLocation(), info.getSourceManager()),
        diag_level, message, info.getRanges(), info.getFixItHints());

    std::string diagnostics_str = diagnostics_stream.TakeStr();

    diagnostics_infos_.push_back(
        {.level = diag_level,
         .diagnostic_inst_id = clang_diagnostic_inst_id,
         .message = diagnostics_str});
  }

  // Outputs Carbon diagnostics based on the collected Clang diagnostics. Must
  // be called after the AST is set in the context.
  auto EmitDiagnostics() -> void {
    for (const ClangDiagnosticInfo& info : diagnostics_infos_) {
      switch (info.level) {
        case clang::DiagnosticsEngine::Ignored:
        case clang::DiagnosticsEngine::Note:
        case clang::DiagnosticsEngine::Remark: {
          context_->TODO(
              info.diagnostic_inst_id,
              llvm::formatv(
                  "Unsupported: C++ diagnostic level for diagnostic\n{0}",
                  info.message));
          break;
        }
        case clang::DiagnosticsEngine::Warning:
        case clang::DiagnosticsEngine::Error:
        case clang::DiagnosticsEngine::Fatal: {
          // TODO: Adjust diagnostics to drop the Carbon file here, and then
          // remove the "C++:\n" prefix.
          CARBON_DIAGNOSTIC(CppInteropParseWarning, Warning, "C++:\n{0}",
                            std::string);
          CARBON_DIAGNOSTIC(CppInteropParseError, Error, "C++:\n{0}",
                            std::string);
          // TODO: This should be part of the location, instead of added as a
          // note here.
          CARBON_DIAGNOSTIC(InCppImport, Note, "in `Cpp` import");
          context_->emitter()
              .Build(info.diagnostic_inst_id,
                     info.level == clang::DiagnosticsEngine::Warning
                         ? CppInteropParseWarning
                         : CppInteropParseError,
                     info.message)
              .Note(loc_, InCppImport)
              .Emit();
          break;
        }
      }
    }
  }

 private:
  // The type-checking context in which we're running Clang.
  Context* context_;

  // The location that triggered calling Clang.
  SemIRLoc loc_;

  struct ClangDiagnosticInfo {
    clang::DiagnosticsEngine::Level level;
    SemIR::InstId diagnostic_inst_id;
    std::string message;
  };

  llvm::SmallVector<ClangDiagnosticInfo> diagnostics_infos_;
};

}  // namespace

// Returns an AST for the C++ imports and a bool that represents whether
// compilation errors where encountered or the generated AST is null due to an
// error. Sets the AST in the context's `sem_ir`.
// TODO: Consider to always have a (non-null) AST.
static auto GenerateAst(Context& context, llvm::StringRef importing_file_path,
                        llvm::ArrayRef<Parse::Tree::PackagingNames> imports,
                        llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs)
    -> std::pair<std::unique_ptr<clang::ASTUnit>, bool> {
  // TODO: Use all import locations by referring each Clang diagnostic to the
  // relevant import.
  SemIRLoc loc = imports.back().node_id;

  CarbonClangDiagnosticConsumer diagnostics_consumer(&context, loc);

  // TODO: Share compilation flags with ClangRunner.
  auto ast = clang::tooling::buildASTFromCodeWithArgs(
      GenerateCppIncludesHeaderCode(context, imports),
      // Parse C++ (and not C)
      {"-x", "c++"}, (importing_file_path + ".generated.cpp_imports.h").str(),
      "clang-tool", std::make_shared<clang::PCHContainerOperations>(),
      clang::tooling::getClangStripDependencyFileAdjuster(),
      clang::tooling::FileContentMappings(), &diagnostics_consumer, fs);
  // Remove link to the diagnostics consumer before its deletion.
  ast->getDiagnostics().setClient(nullptr);

  // In order to emit diagnostics, we need the AST.
  context.sem_ir().set_cpp_ast(ast.get());
  diagnostics_consumer.EmitDiagnostics();

  return {std::move(ast), !ast || diagnostics_consumer.getNumErrors() > 0};
}

// Adds a namespace for the `Cpp` import and returns its `NameScopeId`.
static auto AddNamespace(Context& context, PackageNameId cpp_package_id,
                         llvm::ArrayRef<Parse::Tree::PackagingNames> imports)
    -> SemIR::NameScopeId {
  auto& import_cpps = context.sem_ir().import_cpps();
  import_cpps.Reserve(imports.size());
  for (const Parse::Tree::PackagingNames& import : imports) {
    import_cpps.Add({.node_id = context.parse_tree().As<Parse::ImportDeclId>(
                         import.node_id),
                     .library_id = import.library_id});
  }

  return AddImportNamespaceToScope(
             context,
             GetSingletonType(context, SemIR::NamespaceType::SingletonInstId),
             SemIR::NameId::ForPackageName(cpp_package_id),
             SemIR::NameScopeId::Package,
             /*diagnose_duplicate_namespace=*/false,
             [&]() {
               return AddInst<SemIR::ImportCppDecl>(
                   context,
                   context.parse_tree().As<Parse::ImportDeclId>(
                       imports.front().node_id),
                   {});
             })
      .add_result.name_scope_id;
}

auto ImportCppFiles(Context& context, llvm::StringRef importing_file_path,
                    llvm::ArrayRef<Parse::Tree::PackagingNames> imports,
                    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs)
    -> std::unique_ptr<clang::ASTUnit> {
  if (imports.empty()) {
    return nullptr;
  }

  CARBON_CHECK(!context.sem_ir().cpp_ast());

  auto [generated_ast, ast_has_error] =
      GenerateAst(context, importing_file_path, imports, fs);

  PackageNameId package_id = imports.front().package_id;
  CARBON_CHECK(
      llvm::all_of(imports, [&](const Parse::Tree::PackagingNames& import) {
        return import.package_id == package_id;
      }));
  auto name_scope_id = AddNamespace(context, package_id, imports);
  SemIR::NameScope& name_scope = context.name_scopes().Get(name_scope_id);
  name_scope.set_is_closed_import(true);
  name_scope.set_cpp_decl_context(
      generated_ast->getASTContext().getTranslationUnitDecl());

  if (ast_has_error) {
    name_scope.set_has_error();
  }

  return std::move(generated_ast);
}

// Look ups the given name in the Clang AST in a specific scope. Returns the
// lookup result if lookup was successful.
static auto ClangLookup(Context& context, SemIR::LocId loc_id,
                        SemIR::NameScopeId scope_id, SemIR::NameId name_id)
    -> std::optional<clang::LookupResult> {
  std::optional<llvm::StringRef> name =
      context.names().GetAsStringIfIdentifier(name_id);
  if (!name) {
    // Special names never exist in C++ code.
    return std::nullopt;
  }

  clang::ASTUnit* ast = context.sem_ir().cpp_ast();
  CARBON_CHECK(ast);
  clang::Sema& sema = ast->getSema();

  clang::LookupResult lookup(
      sema,
      clang::DeclarationNameInfo(
          clang::DeclarationName(
              sema.getPreprocessor().getIdentifierInfo(*name)),
          clang::SourceLocation()),
      clang::Sema::LookupNameKind::LookupOrdinaryName);

  bool found = sema.LookupQualifiedName(
      lookup, context.name_scopes().Get(scope_id).cpp_decl_context());

  if (lookup.isClassLookup()) {
    // TODO: To support class lookup, also return the AccessKind for storage.
    context.TODO(loc_id, "Unsupported: Lookup in Class");
    return std::nullopt;
  }

  if (!found) {
    return std::nullopt;
  }

  return lookup;
}

// Returns the return type of the given function declaration.
// Currently only void and 32-bit int are supported.
// TODO: Support more return types.
static auto GetReturnType(Context& context, SemIRLoc loc_id,
                          const clang::FunctionDecl* clang_decl)
    -> SemIR::InstId {
  clang::QualType ret_type = clang_decl->getReturnType().getCanonicalType();
  if (ret_type->isVoidType()) {
    return SemIR::InstId::None;
  }
  if (const auto* builtin_type = dyn_cast<clang::BuiltinType>(ret_type);
      builtin_type && builtin_type->getKind() == clang::BuiltinType::Int) {
    constexpr int SupportedIntWidth = 32;
    uint64_t int_size = context.ast_context().getTypeSize(ret_type);
    if (int_size != SupportedIntWidth) {
      // TODO: Add tests for this case.
      context.TODO(loc_id,
                   llvm::formatv("Unsupported: return type: {0}, size: {1}",
                                 ret_type.getAsString(), int_size));
      return SemIR::ErrorInst::SingletonInstId;
    }
    IntId size_id = context.ints().Add(int_size);
    // TODO: Fill in a location for the type once available.
    SemIR::TypeId type_id = MakeIntType(context, Parse::NodeId::None,
                                        SemIR::IntKind::Signed, size_id);
    // TODO: Fill in a location for the type once available.
    SemIR::InstId type_inst_id = MakeIntTypeLiteral(
        context, Parse::NodeId::None, SemIR::IntKind::Signed, size_id);

    SemIR::InstId return_slot_pattern_id = AddInstInNoBlock(
        // TODO: Fill in a location for the return type once available.
        context, SemIR::LocIdAndInst::NoLoc(SemIR::ReturnSlotPattern(
                     {.type_id = type_id, .type_inst_id = type_inst_id})));
    SemIR::InstId param_pattern_id = AddInstInNoBlock(
        // TODO: Fill in a location for the return type once available.
        context, SemIR::LocIdAndInst::NoLoc(SemIR::OutParamPattern(
                     {.type_id = type_id,
                      .subpattern_id = return_slot_pattern_id,
                      .index = SemIR::CallParamIndex::None})));
    return param_pattern_id;
  }
  context.TODO(loc_id, llvm::formatv("Unsupported: return type: {0}",
                                     ret_type.getAsString()));
  return SemIR::ErrorInst::SingletonInstId;
}

// Imports a function declaration from Clang to Carbon. If successful, returns
// the new Carbon function declaration `InstId`.
static auto ImportFunctionDecl(Context& context, SemIR::LocId loc_id,
                               SemIR::NameScopeId scope_id,
                               SemIR::NameId name_id,
                               const clang::FunctionDecl* clang_decl)
    -> SemIR::InstId {
  if (clang_decl->isVariadic()) {
    context.TODO(loc_id, "Unsupported: Variadic function");
    return SemIR::ErrorInst::SingletonInstId;
  }
  if (!clang_decl->isGlobal()) {
    context.TODO(loc_id, "Unsupported: Non-global function");
    return SemIR::ErrorInst::SingletonInstId;
  }
  if (clang_decl->getTemplatedKind() != clang::FunctionDecl::TK_NonTemplate) {
    context.TODO(loc_id, "Unsupported: Template function");
    return SemIR::ErrorInst::SingletonInstId;
  }
  if (!clang_decl->param_empty()) {
    context.TODO(loc_id, "Unsupported: Function with parameters");
    return SemIR::ErrorInst::SingletonInstId;
  }

  auto return_slot_pattern_id = GetReturnType(context, loc_id, clang_decl);
  if (SemIR::ErrorInst::SingletonInstId == return_slot_pattern_id) {
    return SemIR::ErrorInst::SingletonInstId;
  }

  auto function_decl = SemIR::FunctionDecl{
      SemIR::TypeId::None, SemIR::FunctionId::None, SemIR::InstBlockId::Empty};
  auto decl_id =
      AddPlaceholderInst(context, Parse::NodeId::None, function_decl);

  auto function_info = SemIR::Function{
      {.name_id = name_id,
       .parent_scope_id = scope_id,
       .generic_id = SemIR::GenericId::None,
       .first_param_node_id = Parse::NodeId::None,
       .last_param_node_id = Parse::NodeId::None,
       .pattern_block_id = SemIR::InstBlockId::Empty,
       .implicit_param_patterns_id = SemIR::InstBlockId::Empty,
       .param_patterns_id = SemIR::InstBlockId::Empty,
       .is_extern = false,
       .extern_library_id = SemIR::LibraryNameId::None,
       .non_owning_decl_id = SemIR::InstId::None,
       .first_owning_decl_id = decl_id,
       .definition_id = SemIR::InstId::None},
      {.call_params_id = SemIR::InstBlockId::Empty,
       .return_slot_pattern_id = return_slot_pattern_id,
       .virtual_modifier = SemIR::FunctionFields::VirtualModifier::None,
       .self_param_id = SemIR::InstId::None,
       .cpp_decl = clang_decl}};

  function_decl.function_id = context.functions().Add(function_info);

  function_decl.type_id = GetFunctionType(context, function_decl.function_id,
                                          SemIR::SpecificId::None);

  ReplaceInstBeforeConstantUse(context, decl_id, function_decl);

  return decl_id;
}

// Imports a namespace declaration from Clang to Carbon. If successful, returns
// the new Carbon namespace declaration `InstId`.
static auto ImportNamespaceDecl(Context& context,
                                SemIR::NameScopeId parent_scope_id,
                                SemIR::NameId name_id,
                                clang::NamespaceDecl* clang_decl)
    -> SemIR::InstId {
  auto result = AddImportNamespace(
      context, GetSingletonType(context, SemIR::NamespaceType::SingletonInstId),
      name_id, parent_scope_id, /*import_id=*/SemIR::InstId::None);
  context.name_scopes()
      .Get(result.name_scope_id)
      .set_cpp_decl_context(clang_decl);
  return result.inst_id;
}

// Imports a declaration from Clang to Carbon. If successful, returns the
// instruction for the new Carbon declaration.
static auto ImportNameDecl(Context& context, SemIR::LocId loc_id,
                           SemIR::NameScopeId scope_id, SemIR::NameId name_id,
                           clang::NamedDecl* clang_decl) -> SemIR::InstId {
  if (const auto* clang_function_decl =
          clang::dyn_cast<clang::FunctionDecl>(clang_decl)) {
    return ImportFunctionDecl(context, loc_id, scope_id, name_id,
                              clang_function_decl);
  }
  if (auto* clang_namespace_decl =
          clang::dyn_cast<clang::NamespaceDecl>(clang_decl)) {
    return ImportNamespaceDecl(context, scope_id, name_id,
                               clang_namespace_decl);
  }

  context.TODO(loc_id, llvm::formatv("Unsupported: Declaration type {0}",
                                     clang_decl->getDeclKindName())
                           .str());
  return SemIR::InstId::None;
}

auto ImportNameFromCpp(Context& context, SemIR::LocId loc_id,
                       SemIR::NameScopeId scope_id, SemIR::NameId name_id)
    -> SemIR::InstId {
  Diagnostics::AnnotationScope annotate_diagnostics(
      &context.emitter(), [&](auto& builder) {
        CARBON_DIAGNOSTIC(InCppNameLookup, Note,
                          "in `Cpp` name lookup for `{0}`", SemIR::NameId);
        builder.Note(loc_id, InCppNameLookup, name_id);
      });

  auto lookup = ClangLookup(context, loc_id, scope_id, name_id);
  if (!lookup) {
    return SemIR::InstId::None;
  }

  if (!lookup->isSingleResult()) {
    context.TODO(loc_id,
                 llvm::formatv("Unsupported: Lookup succeeded but couldn't "
                               "find a single result; LookupResultKind: {0}",
                               lookup->getResultKind())
                     .str());
    return SemIR::ErrorInst::SingletonInstId;
  }

  return ImportNameDecl(context, loc_id, scope_id, name_id,
                        lookup->getFoundDecl());
}

}  // namespace Carbon::Check
