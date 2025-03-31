// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_CHECK_CLASS_H_
#define CARBON_TOOLCHAIN_CHECK_CLASS_H_

#include "toolchain/check/context.h"

namespace Carbon::Check {

// Builds the `Self` type using the resulting type constant.
auto SetNewClassSelfTypeId(Context& context, SemIR::ClassId class_id) -> void;

// Tracks that this declaration is the definition and introduce `Self`.
auto TrackClassDefinition(Context& context, SemIR::ClassId class_id,
                          SemIR::InstId class_decl_id) -> SemIR::Class&;

}  // namespace Carbon::Check

#endif  // CARBON_TOOLCHAIN_CHECK_CLASS_H_
