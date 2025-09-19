// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/cpp/operators.h"

#include "clang/Sema/Overload.h"
#include "clang/Sema/Sema.h"
#include "toolchain/check/cpp/import.h"
#include "toolchain/check/cpp/type_mapping.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/type.h"
#include "toolchain/sem_ir/ids.h"

namespace Carbon::Check {

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

auto LookupCppOperator(Context& context, SemIR::LocId loc_id, Operator op,
                       llvm::ArrayRef<SemIR::InstId> arg_ids) -> SemIR::InstId {
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
  // This works for both unary and binary operators.
  sema.LookupOverloadedBinOp(candidate_set, *op_kind, functions, *arg_exprs);

  for (auto& it : candidate_set) {
    if (!it.Function) {
      continue;
    }
    functions.addDecl(it.Function, it.FoundDecl.getAccess());
  }

  return ImportCppOverloadSet(context, SemIR::NameScopeId::None,
                              SemIR::NameId::CppOperator, functions);
}

}  // namespace Carbon::Check
