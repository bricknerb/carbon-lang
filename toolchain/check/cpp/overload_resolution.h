// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_CHECK_CPP_OVERLOAD_RESOLUTION_H_
#define CARBON_TOOLCHAIN_CHECK_CPP_OVERLOAD_RESOLUTION_H_

#include "toolchain/check/context.h"
#include "toolchain/sem_ir/function.h"
#include "toolchain/sem_ir/ids.h"

namespace Carbon::Check {

// Checks and builds SemIR for a call to `callee_id` with self `self_id` and
// arguments `args_id`, where the callee is a C++ (potentially overloaded)
// function.
//
// Performs overloading resolution over the overload set. A set with a single
// non-templated function goes through the same rules for overloading
// resolution. Uses Clang to find the best viable function for the call.
//
// Preserves the given self, if set. If not set, and the function is a a C++
// member operator, self will be set to the first argument, which in turn will
// be removed from the given args.
//
// Note on non-overloaded functions: In C++, a single non-templated function is
// also treated as an overloaded set and goes through the overload resolution to
// ensure that the function is viable for the call. This is to make sure that
// calls that have no viable implicit conversion sequence are rejected even when
// an implicit conversion is possible. Keeping the same behavior here for
// consistency and supporting migrations so that the migrated callers from C++
// remain valid.
auto PerformCallToCppOverloadFunction(Context& context, SemIR::LocId loc_id,
                                      SemIR::CppOverloadSetId overload_set_id,
                                      SemIR::InstId self_id,
                                      llvm::ArrayRef<SemIR::InstId> arg_ids)
    -> SemIR::InstId;

}  // namespace Carbon::Check

#endif  // CARBON_TOOLCHAIN_CHECK_CPP_OVERLOAD_RESOLUTION_H_
