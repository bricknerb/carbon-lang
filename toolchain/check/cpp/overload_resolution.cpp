// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/cpp/overload_resolution.h"

#include "clang/Sema/Overload.h"
#include "clang/Sema/Sema.h"
#include "toolchain/check/cpp/import.h"
#include "toolchain/check/cpp/type_mapping.h"
#include "toolchain/sem_ir/expr_info.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

// Invents a Clang argument expression to use in overload resolution to
// represent the given Carbon argument instruction.
static auto InventClangArg(Context& context, SemIR::InstId arg_id)
    -> clang::Expr* {
  clang::ExprValueKind value_kind;
  switch (SemIR::GetExprCategory(context.sem_ir(), arg_id)) {
    case SemIR::ExprCategory::NotExpr:
      CARBON_FATAL("Should not see these here");

    case SemIR::ExprCategory::Error:
      return nullptr;

    case SemIR::ExprCategory::DurableRef:
      value_kind = clang::ExprValueKind::VK_LValue;
      break;

    case SemIR::ExprCategory::EphemeralRef:
      value_kind = clang::ExprValueKind::VK_XValue;
      break;

    case SemIR::ExprCategory::Value:
    case SemIR::ExprCategory::Initializing:
      value_kind = clang::ExprValueKind::VK_PRValue;
      break;

    case SemIR::ExprCategory::Mixed:
      // TODO: Handle this by creating an InitListExpr.
      value_kind = clang::ExprValueKind::VK_PRValue;
      break;
  }

  if (context.insts().Get(arg_id).type_id() == SemIR::ErrorInst::TypeId) {
    // The argument error has already been diagnosed.
    return nullptr;
  }

  clang::QualType arg_cpp_type = MapToCppType(context, arg_id);
  if (arg_cpp_type.isNull()) {
    CARBON_DIAGNOSTIC(CppCallArgTypeNotSupported, Error,
                      "call argument of type {0} is not supported",
                      TypeOfInstId);
    context.emitter().Emit(arg_id, CppCallArgTypeNotSupported, arg_id);
    return nullptr;
  }

  // TODO: Avoid heap allocating more of these on every call. Either cache them
  // somewhere or put them on the stack.
  return new (context.ast_context()) clang::OpaqueValueExpr(
      // TODO: Add location accordingly.
      clang::SourceLocation(), arg_cpp_type.getNonReferenceType(), value_kind);
}

// For each arg, invents a Clang argument expression to use in overload
// resolution to represent the given Carbon argument instructions. Returns
// std::nullopt if any arg failed.
static auto InventClangArgs(Context& context,
                            llvm::ArrayRef<SemIR::InstId> arg_ids)
    -> std::optional<llvm::SmallVector<clang::Expr*>> {
  llvm::SmallVector<clang::Expr*> arg_exprs;
  arg_exprs.reserve(arg_ids.size());
  for (SemIR::InstId arg_id : arg_ids) {
    auto* arg_expr = InventClangArg(context, arg_id);
    if (!arg_expr) {
      return std::nullopt;
    }
    arg_exprs.push_back(arg_expr);
  }
  return arg_exprs;
}

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

// Performs overloading resolution over a set of candidates. Returns the
// resolved function, or an error instruction if overload resolution failed.
static auto PerformCppOverloadResolutionImpl(
    Context& context, SemIR::LocId loc_id, SemIR::NameId name_id,
    const clang::UnresolvedSet<4>& candidate_functions, clang::Expr* self_expr,
    llvm::ArrayRef<clang::Expr*> arg_exprs) -> SemIR::InstId {
  // Add candidate functions from the name lookup.
  clang::OverloadCandidateSet candidate_set(
      // TODO: Add location accordingly.
      clang::SourceLocation(),
      clang::OverloadCandidateSet::CandidateSetKind::CSK_Normal);

  clang::ASTUnit* ast = context.sem_ir().clang_ast_unit();
  CARBON_CHECK(ast);
  clang::Sema& sema = ast->getSema();

  AddOverloadCandidataes(sema, candidate_set, candidate_functions, self_expr,
                         arg_exprs);

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
                             name_id);
      return SemIR::ErrorInst::InstId;
    }
    case clang::OverloadingResult::OR_Ambiguous: {
      // TODO: Add notes with the candidates.
      CARBON_DIAGNOSTIC(CppOverloadingAmbiguousCandidatesFound, Error,
                        "call to `{0}` is ambiguous", SemIR::NameId);
      context.emitter().Emit(loc_id, CppOverloadingAmbiguousCandidatesFound,
                             name_id);
      return SemIR::ErrorInst::InstId;
    }
    case clang::OverloadingResult::OR_Deleted: {
      // TODO: Add notes with the candidates.
      CARBON_DIAGNOSTIC(CppOverloadingDeletedFunctionFound, Error,
                        "call to deleted function `{0}`", SemIR::NameId);
      context.emitter().Emit(loc_id, CppOverloadingDeletedFunctionFound,
                             name_id);
      return SemIR::ErrorInst::InstId;
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

  return PerformCppOverloadResolutionImpl(
      context, loc_id, context.cpp_overload_sets().Get(overload_set_id).name_id,
      context.cpp_overload_sets().Get(overload_set_id).candidate_functions,
      self_expr, *arg_exprs);
}

// Maps Carbon operator interface and operator names to Clang operator kinds.
static auto GetClangOperatorKind(Context& context, SemIR::LocId loc_id,
                                 llvm::StringLiteral interface_name,
                                 llvm::StringLiteral op_name)
    -> std::optional<clang::OverloadedOperatorKind> {
  // Unary operators.
  if (interface_name == "Destroy" || interface_name == "As" ||
      interface_name == "ImplicitAs") {
    // TODO: Support destructors and conversions.
    return std::nullopt;
  }

  // Increment and Decrement.
  if (interface_name == "Inc") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_PlusPlus;
  }
  if (interface_name == "Dec") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_MinusMinus;
  }

  // Arithmetic.
  if (interface_name == "Negate") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Minus;
  }

  // Binary operators.

  // Arithmetic Operators.
  if (interface_name == "AddWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Plus;
  }
  if (interface_name == "SubWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Minus;
  }
  if (interface_name == "MulWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Star;
  }
  if (interface_name == "DivWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Slash;
  }
  if (interface_name == "ModWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Percent;
  }

  // Bitwise Operators.
  if (interface_name == "BitAndWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Amp;
  }
  if (interface_name == "BitOrWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Pipe;
  }
  if (interface_name == "BitXorWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_Caret;
  }
  if (interface_name == "LeftShiftWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_LessLess;
  }
  if (interface_name == "RightShiftWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_GreaterGreater;
  }

  // Compound Assignment Arithmetic Operators.
  if (interface_name == "AddAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_PlusEqual;
  }
  if (interface_name == "SubAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_MinusEqual;
  }
  if (interface_name == "MulAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_StarEqual;
  }
  if (interface_name == "DivAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_SlashEqual;
  }
  if (interface_name == "ModAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_PercentEqual;
  }

  // Compound Assignment Bitwise Operators.
  if (interface_name == "BitAndAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_AmpEqual;
  }
  if (interface_name == "BitOrAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_PipeEqual;
  }
  if (interface_name == "BitXorAssignWith") {
    CARBON_CHECK(op_name == "Op");
    return clang::OO_CaretEqual;
  }
  // TODO: Add support for `LeftShiftAssignWith` (`OO_LessLessEqual`) and
  // `RightShiftAssignWith` (`OO_GreaterGreaterEqual`) when references are
  // supported.

  // Relational Operators.
  if (interface_name == "EqWith") {
    if (op_name == "Equal") {
      return clang::OO_EqualEqual;
    }
    CARBON_CHECK(op_name == "NotEqual");
    return clang::OO_ExclaimEqual;
  }
  if (interface_name == "OrderedWith") {
    if (op_name == "Less") {
      return clang::OO_Less;
    }
    if (op_name == "Greater") {
      return clang::OO_Greater;
    }
    if (op_name == "LessOrEquivalent") {
      return clang::OO_LessEqual;
    }
    CARBON_CHECK(op_name == "GreaterOrEquivalent");
    return clang::OO_GreaterEqual;
  }

  context.TODO(loc_id, llvm::formatv("Unsupported operator interface `{0}`",
                                     interface_name));
  return std::nullopt;
}

auto LookupAndResolveCppOperator(Context& context, SemIR::LocId loc_id,
                                 Operator op,
                                 llvm::ArrayRef<SemIR::InstId> arg_ids)
    -> SemIR::InstId {
  Diagnostics::AnnotationScope annotate_diagnostics(
      &context.emitter(), [&](auto& builder) {
        CARBON_DIAGNOSTIC(InCppOperatorLookup, Note,
                          "in `Cpp` operator `{0}` lookup", std::string);
        builder.Note(loc_id, InCppOperatorLookup, op.interface_name.str());
      });

  auto op_kind =
      GetClangOperatorKind(context, loc_id, op.interface_name, op.op_name);
  if (!op_kind) {
    return SemIR::InstId::None;
  }

  auto arg_exprs = InventClangArgs(context, arg_ids);
  if (!arg_exprs.has_value()) {
    return SemIR::ErrorInst::InstId;
  }

  clang::Sema& sema = context.sem_ir().clang_ast_unit()->getSema();

  clang::UnresolvedSet<4> functions;
  // TODO: Add location accordingly.
  clang::OverloadCandidateSet candidate_set(
      clang::SourceLocation(), clang::OverloadCandidateSet::CSK_Operator);
  sema.LookupOverloadedBinOp(candidate_set, *op_kind, functions, *arg_exprs);

  for (auto& it : candidate_set) {
    if (!it.Function) {
      continue;
    }
    functions.addDecl(it.Function, it.FoundDecl.getAccess());
  }

  return PerformCppOverloadResolutionImpl(context, loc_id,
                                          SemIR::NameId::CppOperator, functions,
                                          nullptr, *arg_exprs);
}

}  // namespace Carbon::Check
