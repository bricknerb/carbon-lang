// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/class.h"

#include "toolchain/check/context.h"
#include "toolchain/check/eval.h"

namespace Carbon::Check {

auto SetNewClassSelfTypeId(Context& context, SemIR::ClassId class_id) -> void {
  auto& class_info = context.classes().Get(class_id);
  auto specific_id = context.generics().GetSelfSpecific(class_info.generic_id);
  class_info.self_type_id = context.types().GetTypeIdForTypeConstantId(
      TryEvalInst(context, SemIR::InstId::None,
                  SemIR::ClassType{.type_id = SemIR::TypeType::SingletonTypeId,
                                   .class_id = class_id,
                                   .specific_id = specific_id}));
}

auto TrackClassDefinition(Context& context, SemIR::ClassId class_id,
                          SemIR::InstId class_decl_id) -> SemIR::Class& {
  auto& class_info = context.classes().Get(class_id);

  // Track that this declaration is the definition.
  CARBON_CHECK(!class_info.has_definition_started());
  class_info.definition_id = class_decl_id;
  class_info.scope_id = context.name_scopes().Add(
      class_decl_id, SemIR::NameId::None, class_info.parent_scope_id);

  // Introduce `Self`.
  context.name_scopes().AddRequiredName(
      class_info.scope_id, SemIR::NameId::SelfType,
      context.types().GetInstId(class_info.self_type_id));

  return class_info;
}

}  // namespace Carbon::Check
