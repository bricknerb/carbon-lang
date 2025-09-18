// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/cpp/overload_resolution.h"

#include "clang/Sema/Overload.h"
#include "clang/Sema/Sema.h"
#include "toolchain/check/cpp/import.h"
#include "toolchain/check/cpp/type_mapping.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

// Adds the given overload candidates to the candidate set.
static auto AddOverloadCandidataes(clang::Sema& sema,
                                   clang::OverloadCandidateSet& candidate_set,
                                   const clang::UnresolvedSetImpl& functions,
                                   clang::Expr* self_arg,
                                   llvm::ArrayRef<clang::Expr*> args) -> void {
  constexpr bool SuppressUserConversions = false;
  constexpr bool PartialOverloading = false;
  constexpr clang::TemplateArgumentListInfo* ExplicitTemplateArgs = nullptr;

  for (auto found_decl : functions.pairs()) {
    auto* decl = found_decl.getDecl()->getUnderlyingDecl();
    auto* template_decl = dyn_cast<clang::FunctionTemplateDecl>(decl);
    auto* fn_decl = template_decl ? template_decl->getTemplatedDecl()
                                  : cast<clang::FunctionDecl>(decl);
    auto* method_decl = dyn_cast<clang::CXXMethodDecl>(fn_decl);
    if (method_decl && !method_decl->isStatic() &&
        !isa<clang::CXXConstructorDecl>(fn_decl)) {
      clang::QualType self_type;
      clang::Expr::Classification self_classification;
      if (self_arg) {
        self_type = self_arg->getType();
        self_classification = self_arg->Classify(sema.Context);
      }
      if (template_decl) {
        sema.AddMethodTemplateCandidate(
            template_decl, found_decl,
            cast<clang::CXXRecordDecl>(template_decl->getDeclContext()),
            ExplicitTemplateArgs, self_type, self_classification, args,
            candidate_set, SuppressUserConversions, PartialOverloading);
      } else {
        sema.AddMethodCandidate(method_decl, found_decl,
                                method_decl->getParent(), self_type,
                                self_classification, args, candidate_set,
                                SuppressUserConversions, PartialOverloading);
      }
    } else {
      if (template_decl) {
        sema.AddTemplateOverloadCandidate(
            template_decl, found_decl, ExplicitTemplateArgs, args,
            candidate_set, SuppressUserConversions, PartialOverloading);
      } else {
        sema.AddOverloadCandidate(fn_decl, found_decl, args, candidate_set,
                                  SuppressUserConversions, PartialOverloading);
      }
    }
  }
}

auto PerformCppOverloadResolution(Context& context, SemIR::LocId loc_id,
                                  SemIR::CppOverloadSetId overload_set_id,
                                  SemIR::InstId self_id,
                                  llvm::ArrayRef<SemIR::InstId> arg_ids)
    -> SemIR::InstId {
  Diagnostics::AnnotationScope annotate_diagnostics(
      &context.emitter(), [&](auto& builder) {
        CARBON_DIAGNOSTIC(InCallToCppFunction, Note,
                          "in call to Cpp function here");
        builder.Note(loc_id, InCallToCppFunction);
      });

  // Map Carbon call argument types to C++ types.
  clang::Expr* self_expr = nullptr;
  if (self_id.has_value()) {
    self_expr = InventClangArg(context, self_id);
    if (!self_expr) {
      return SemIR::ErrorInst::InstId;
    }
  }
  auto arg_exprs = InventClangArgs(context, arg_ids);
  if (!arg_exprs.has_value()) {
    return SemIR::ErrorInst::InstId;
  }

  const SemIR::CppOverloadSet& overload_set =
      context.cpp_overload_sets().Get(overload_set_id);

  // Add candidate functions from the name lookup.
  clang::OverloadCandidateSet candidate_set(
      // TODO: Add location accordingly.
      clang::SourceLocation(),
      clang::OverloadCandidateSet::CandidateSetKind::CSK_Normal);

  clang::ASTUnit* ast = context.sem_ir().clang_ast_unit();
  CARBON_CHECK(ast);
  clang::Sema& sema = ast->getSema();

  AddOverloadCandidataes(sema, candidate_set, overload_set.candidate_functions,
                         self_expr, *arg_exprs);

  // Find best viable function among the candidates.
  clang::OverloadCandidateSet::iterator best_viable_fn;
  clang::OverloadingResult overloading_result =
      // TODO: Add location accordingly.
      candidate_set.BestViableFunction(sema, clang::SourceLocation(),
                                       best_viable_fn);

  switch (overloading_result) {
    case clang::OverloadingResult::OR_Success: {
      // TODO: Handle the cases when Function is null.
      CARBON_CHECK(best_viable_fn->Function);
      sema.MarkFunctionReferenced(clang::SourceLocation(),
                                  best_viable_fn->Function);
      SemIR::InstId result =
          ImportCppFunctionDecl(context, loc_id, best_viable_fn->Function);
      return result;
    }
    case clang::OverloadingResult::OR_No_Viable_Function: {
      // TODO: Add notes with the candidates.
      CARBON_DIAGNOSTIC(CppOverloadingNoViableFunctionFound, Error,
                        "no matching function for call to `{0}`",
                        SemIR::NameId);
      context.emitter().Emit(loc_id, CppOverloadingNoViableFunctionFound,
                             overload_set.name_id);
      return SemIR::ErrorInst::InstId;
    }
    case clang::OverloadingResult::OR_Ambiguous: {
      // TODO: Add notes with the candidates.
      CARBON_DIAGNOSTIC(CppOverloadingAmbiguousCandidatesFound, Error,
                        "call to `{0}` is ambiguous", SemIR::NameId);
      context.emitter().Emit(loc_id, CppOverloadingAmbiguousCandidatesFound,
                             overload_set.name_id);
      return SemIR::ErrorInst::InstId;
    }
    case clang::OverloadingResult::OR_Deleted: {
      // TODO: Add notes with the candidates.
      CARBON_DIAGNOSTIC(CppOverloadingDeletedFunctionFound, Error,
                        "call to deleted function `{0}`", SemIR::NameId);
      context.emitter().Emit(loc_id, CppOverloadingDeletedFunctionFound,
                             overload_set.name_id);
      return SemIR::ErrorInst::InstId;
    }
  }
}

}  // namespace Carbon::Check
