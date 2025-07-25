// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEM_IR_INST_KIND_H_
#define CARBON_TOOLCHAIN_SEM_IR_INST_KIND_H_

#include <cstdint>

#include "common/enum_base.h"
namespace Carbon::SemIR {

// Whether an instruction defines a type.
enum class InstIsType : int8_t {
  // Always of type `type`, and might define a type constant.
  Always,
  // Sometimes of type `type`, and might define a type constant.
  Maybe,
  // Never defines a type constant. Note that such instructions can still have
  // type `type`, but are not the canonical definition of any type.
  Never,
};

// Whether an instruction can have a constant value, and whether it can be a
// constant inst (i.e. an inst whose canonical ID defines a constant value; see
// constant.h).
//
// This specifies whether an instruction of this kind can have a corresponding
// constant value in the `constant_values()` list, and whether an instruction of
// this kind can be added to the `constants()` list.
enum class InstConstantKind : int8_t {
  // This instruction never has a constant value, and is never a constant inst.
  // This is also used for instructions that don't produce a value at all and
  // aren't used as constants.
  Never,
  // This instruction is never a constant inst, but can reduce to a
  // constant value of a different kind. For example, `UnaryOperatorNot` is
  // never a constant inst; if its operand is a concrete constant, its
  // constant value will instead be a `BoolLiteral`, and if its operand is not a
  // concrete constant, it is non-constant. This is the default.
  Indirect,
  // This instruction can be a symbolic constant inst, depending on its
  // operands, but never a concrete constant inst. For example, a `Call`
  // instruction can be a symbolic constant inst but never a concrete constant
  // inst. The instruction may have a concrete constant value of a different
  // kind.
  SymbolicOnly,
  // This instruction may be a symbolic constant inst if it has symbolic
  // operands, and may be a concrete constant inst if it is a reference
  // expression, but it is never a concrete constant if it is a value or
  // initializing expression. For example, a `TupleAccess` instruction can be a
  // symbolic constant inst when applied to a symbolic constant, and can be a
  // concrete reference constant inst when applied to a reference constant.
  SymbolicOrReference,
  // This instruction is a metaprogramming or template instantiation action that
  // generates an instruction. Like `SymbolicOnly`, it may be a symbolic
  // constant inst depending on its operands, but never a concrete constant
  // inst. The instruction may have a concrete constant value that is a
  // generated instruction. Constant evaluation support for types with this
  // constant kind is provided automatically, by calling `PerformDelayedAction`.
  InstAction,
  // This instruction's operands determine whether it has a constant value,
  // whether it is a constant inst, and/or whether it results in a compile-time
  // error, in ways not expressed by the other InstConstantKinds. For example,
  // `ArrayType` is a compile-time constant if its operands are constant and its
  // array bound is within a valid range, and `ConstType` is a constant inst if
  // its operand is the canonical ID of a constant inst that isn't a
  // `ConstType`.
  Conditional,
  // This instruction is a constant inst if and only if its operands are all the
  // canonical IDs of constant insts, it has a constant value if and only if its
  // operands all have constant values, and that constant value is the result of
  // substituting the operands with their canonical IDs. For example, a
  // `TupleValue` has all these properties. Constant evaluation support for
  // types with this constant kind is provided automatically.
  WheneverPossible,
  // The same as `WheneverPossible`, except that the operands are known in
  // advance to always have a constant value. For example, `IntValue`.
  Always,
  // The instruction may be a unique constant, as described below for
  // `AlwaysUnique`. Otherwise the instruction is not constant. This is used for
  // `VarStorage`, where global variables are `AlwaysUnique` and other variables
  // are non-constant.
  ConditionalUnique,
  // This instruction is itself a unique constant, and its ID is always
  // canonical. This is used for declarations whose constant identity is simply
  // themselves. The `ConstantId` for this instruction will always be a concrete
  // constant whose `InstId` refers directly back to the instruction, rather
  // than to a separate instruction in the constants block.
  // TODO: Decide if this is the model we want for these cases.
  AlwaysUnique,
};

// Whether constant evaluation of an instruction needs the instruction to have
// been created and allocated an InstId, or only needs the instruction operands.
enum class InstConstantNeedsInstIdKind : int8_t {
  // This instruction kind doesn't need an InstId to be evaluated.
  No,
  // This instruction needs an InstId during evaluation, but doesn't need the
  // instruction to persist after evaluation.
  DuringEvaluation,
  // This instruction needs a permanent instruction ID, for example because that
  // instruction ID can appear in the constant result of evaluation.
  Permanent,
};

// Whether an instruction is a terminator or part of the terminator sequence.
// The instructions in a block appear in the order NotTerminator, then
// TerminatorSequence, then Terminator, which is also the numerical order of
// these values.
enum class TerminatorKind : int8_t {
  // This instruction is not a terminator.
  NotTerminator,
  // This instruction is not itself a terminator, but forms part of a terminator
  // sequence.
  TerminatorSequence,
  // This instruction is a terminator.
  Terminator,
};

CARBON_DEFINE_RAW_ENUM_CLASS(InstKind, uint8_t) {
#define CARBON_SEM_IR_INST_KIND(Name) CARBON_RAW_ENUM_ENUMERATOR(Name)
#include "toolchain/sem_ir/inst_kind.def"
};

class InstKind : public CARBON_ENUM_BASE(InstKind) {
 public:
#define CARBON_SEM_IR_INST_KIND(Name) CARBON_ENUM_CONSTANT_DECL(Name)
#include "toolchain/sem_ir/inst_kind.def"

  // Returns the `InstKind` for an instruction, for `CARBON_KIND_SWITCH`.
  template <typename InstT>
  static constexpr auto& For = InstT::Kind;

  template <typename TypedNodeId>
  class Definition;

  // Information about a definition. See associated accessors below for
  // comments.
  struct DefinitionInfo {
    llvm::StringLiteral ir_name;
    InstIsType is_type = InstIsType::Never;
    InstConstantKind constant_kind = InstConstantKind::Indirect;
    InstConstantNeedsInstIdKind constant_needs_inst_id =
        constant_kind == InstConstantKind::AlwaysUnique
            ? InstConstantNeedsInstIdKind::Permanent
            : InstConstantNeedsInstIdKind::No;
    TerminatorKind terminator_kind = TerminatorKind::NotTerminator;
    bool is_lowered = true;
    bool deduce_through = false;
    bool has_cleanup = false;
  };

  // Provides a definition for this instruction kind. Should only be called
  // once, to construct the kind as part of defining it in `typed_insts.h`.
  template <typename TypedNodeId>
  constexpr auto Define(DefinitionInfo info) const -> Definition<TypedNodeId>;

  using EnumBase::AsInt;
  using EnumBase::FromInt;
  using EnumBase::Make;

  // Returns true if the kind matches any of the provided instructions' kinds.
  template <typename... InstT>
  constexpr auto IsAnyOf() const -> bool {
    return ((*this == InstT::Kind) || ...);
  }

  // Returns the name to use for this instruction kind in Semantics IR.
  auto ir_name() const -> llvm::StringLiteral {
    return definition_info(*this).ir_name;
  }

  // Returns whether this instruction kind defines a type.
  auto is_type() const -> InstIsType { return definition_info(*this).is_type; }

  // Returns whether this instruction kind is expected to produce a typed value.
  auto has_type() const -> bool;

  // Returns this instruction kind's category of allowed constants.
  auto constant_kind() const -> InstConstantKind {
    return definition_info(*this).constant_kind;
  }

  // Returns whether we need an `InstId` referring to the instruction to
  // constant evaluate this instruction. If this is set to `true`, then:
  //
  //  - `Check::TryEvalInst` will not allow this instruction to be directly
  //    evaluated without an `InstId`.
  //  - `Check::EvalConstantInst` will be passed an `InstId` for the original
  //    instruction being evaluated.
  //
  // This is set to true for instructions whose evaluation either might need a
  // location, for example for diagnostics or for newly-created instructions,
  // and for instructions whose evaluation needs to inspect the original form of
  // its operands.
  auto constant_needs_inst_id() const -> InstConstantNeedsInstIdKind {
    return definition_info(*this).constant_needs_inst_id;
  }

  // Returns whether this instruction kind is a code block terminator, such as
  // an unconditional branch instruction, or part of the termination sequence,
  // such as a conditional branch instruction. The termination sequence of a
  // code block appears after all other instructions, and ends with a
  // terminator instruction.
  auto terminator_kind() const -> TerminatorKind {
    return definition_info(*this).terminator_kind;
  }

  // Returns true if `Instruction(A)` == `Instruction(B)` allows deduction to
  // conclude `A` == `B`.
  auto deduce_through() const -> bool {
    return definition_info(*this).deduce_through;
  }

  // Returns true if this instruction has scoped cleanup associated, typically a
  // destructor.
  constexpr auto has_cleanup() const -> bool {
    return definition_info(*this).has_cleanup;
  }

 private:
  // Returns the DefinitionInfo for the kind.
  static auto definition_info(InstKind kind) -> const DefinitionInfo&;
};

#define CARBON_SEM_IR_INST_KIND(Name) \
  CARBON_ENUM_CONSTANT_DEFINITION(InstKind, Name)
#include "toolchain/sem_ir/inst_kind.def"

// We expect the instruction kind to fit compactly into 8 bits.
static_assert(sizeof(InstKind) == 1, "Kind objects include padding!");

// A definition of an instruction kind. This is an InstKind value, plus
// ancillary data such as the name to use for the node kind in LLVM IR. These
// are not copyable, and only one instance of this type is expected to exist
// per instruction kind, specifically `TypedInst::Kind`. Use `InstKind`
// instead as a thin wrapper around an instruction kind index.
template <typename TypedNodeIdArg>
class InstKind::Definition : public InstKind {
 public:
  using TypedNodeId = TypedNodeIdArg;

  // Not copyable.
  Definition(const Definition&) = delete;
  auto operator=(const Definition&) -> Definition& = delete;

  // Returns the name to use for this instruction kind in Semantics IR.
  constexpr auto ir_name() const -> llvm::StringLiteral {
    return info_.ir_name;
  }

  // Returns whether this instruction kind defines a type.
  constexpr auto is_type() const -> InstIsType { return info_.is_type; }

  // Returns whether instructions of this kind are always symbolic whenever they
  // are types. For convenience, also returns false if the instruction cannot be
  // a type, because this is typically used in requires expressions where that
  // case is handled by a separate overload.
  constexpr auto is_symbolic_when_type() const -> bool {
    // Types are values (not references) of type `type`, so if the instruction
    // kind is always symbolic when it's a value, then it's always symbolic when
    // it's a type.
    return is_type() != InstIsType::Never &&
           (constant_kind() == InstConstantKind::SymbolicOnly ||
            constant_kind() == InstConstantKind::SymbolicOrReference);
  }

  // Returns this instruction kind's category of allowed constants.
  constexpr auto constant_kind() const -> InstConstantKind {
    return info_.constant_kind;
  }

  // Returns whether constant evaluation of this instruction needs an InstId.
  constexpr auto constant_needs_inst_id() const -> InstConstantNeedsInstIdKind {
    return info_.constant_needs_inst_id;
  }

  // Returns whether this instruction kind is a code block terminator. See
  // InstKind::terminator_kind().
  constexpr auto terminator_kind() const -> TerminatorKind {
    return info_.terminator_kind;
  }

  // Returns true if the instruction is lowered.
  constexpr auto is_lowered() const -> bool { return info_.is_lowered; }

  // Returns true if `Instruction(A)` == `Instruction(B)` allows deduction to
  // conclude `A` == `B`.
  constexpr auto deduce_through() const -> bool { return info_.deduce_through; }

  // Returns true if this instruction has scoped cleanup associated, typically a
  // destructor.
  constexpr auto has_cleanup() const -> bool { return info_.has_cleanup; }

 private:
  friend class InstKind;

  constexpr Definition(InstKind kind, InstKind::DefinitionInfo info)
      : InstKind(kind), info_(info) {}

  InstKind::DefinitionInfo info_;
};

template <typename TypedNodeId>
constexpr auto InstKind::Define(DefinitionInfo info) const
    -> Definition<TypedNodeId> {
  return Definition<TypedNodeId>(*this, info);
}

}  // namespace Carbon::SemIR

#endif  // CARBON_TOOLCHAIN_SEM_IR_INST_KIND_H_
