// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_CHECK_FUNCTION_H_
#define CARBON_TOOLCHAIN_CHECK_FUNCTION_H_

#include "toolchain/check/context.h"
#include "toolchain/check/decl_name_stack.h"
#include "toolchain/check/subst.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/ids.h"

namespace Carbon::Check {

// Returns the ID of the self parameter pattern, or None.
// TODO: Do this during initial traversal of implicit params.
auto FindSelfPattern(Context& context,
                     SemIR::InstBlockId implicit_param_patterns_id)
    -> SemIR::InstId;

// Checks that `new_function` has the same return type as `prev_function`, or if
// `prev_function_id` is specified, a specific version of `prev_function`.
// Prints a suitable diagnostic and returns false if not. Never checks for a
// syntactic match.
auto CheckFunctionReturnTypeMatches(Context& context,
                                    const SemIR::Function& new_function,
                                    const SemIR::Function& prev_function,
                                    SemIR::SpecificId prev_specific_id,
                                    bool diagnose = true) -> bool;

// Checks that `new_function` has the same parameter types and return type as
// `prev_function`, or if `prev_function_id` is specified, a specific version of
// `prev_function`. Prints a suitable diagnostic and returns false if not.
//
// `check_syntax` is false if the redeclaration can be called via a thunk with
// implicit conversions from the original declaration.
// `check_self` is false if the self declaration does not have to match (for
// instance in impls of virtual functions).
auto CheckFunctionTypeMatches(Context& context,
                              const SemIR::Function& new_function,
                              const SemIR::Function& prev_function,
                              SemIR::SpecificId prev_specific_id,
                              bool check_syntax, bool check_self,
                              bool diagnose = true) -> bool;

inline auto CheckFunctionTypeMatches(Context& context,
                                     const SemIR::Function& new_function,
                                     const SemIR::Function& prev_function)
    -> bool {
  return CheckFunctionTypeMatches(context, new_function, prev_function,
                                  SemIR::SpecificId::None,
                                  /*check_syntax=*/true, /*check_self=*/true);
}

// Checks that the return type of the specified function is complete, issuing an
// error if not. This computes the return slot usage for the function if
// necessary, and returns information about how the function returns its return
// value.
auto CheckFunctionReturnType(Context& context, SemIR::LocId loc_id,
                             const SemIR::Function& function,
                             SemIR::SpecificId specific_id)
    -> SemIR::ReturnTypeInfo;

// Checks that a function declaration's signature is suitable to support a
// function definition. This requires the parameter types to be complete and the
// return type to be concrete.
auto CheckFunctionDefinitionSignature(Context& context,
                                      SemIR::FunctionId function_id) -> void;

}  // namespace Carbon::Check

#endif  // CARBON_TOOLCHAIN_CHECK_FUNCTION_H_
