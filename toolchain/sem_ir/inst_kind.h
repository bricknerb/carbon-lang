// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEM_IR_INST_KIND_H_
#define CARBON_TOOLCHAIN_SEM_IR_INST_KIND_H_

#include <cstdint>

#include "common/check.h"
#include "common/enum_base.h"
#include "llvm/ADT/FoldingSet.h"

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

// Whether an instruction produces or represents a value, and if so, what kind
// of value.
enum class InstValueKind : int8_t {
  // This instruction doesn't produce a value, and shouldn't be referenced by
  // other instructions.
  None,
  // This instruction represents an expression or expression-like construct that
  // produces a value of the type indicated by its `type_id` field.
  Typed,
};

// Whether an instruction can be used to define a constant value. This specifies
// whether the instruction can be added to the `constants()` list. Note that
// even instructions that cannot define a constant value can still have an
// associated `constant_value()`, but the constant value will be a different
// kind of instruction.
enum class InstConstantKind : int8_t {
  // This instruction never defines a constant value. For example,
  // `UnaryOperatorNot` never defines a constant value; if its operand is a
  // concrete constant, its constant value will instead be a `BoolLiteral`. This
  // is also used for instructions that don't produce a value at all.
  Never,
  // This instruction may be a symbolic constant, depending on its operands, but
  // is never a concrete constant. For example, a `Call` instruction can be a
  // symbolic constant but never a concrete constant.
  SymbolicOnly,
  // This instruction can define a symbolic or concrete constant, but might not
  // have a constant value, depending on its operands. For example, a
  // `TupleValue` can define a constant if its operands are constants.
  Conditional,
  // This instruction always has a constant value of the same kind. For example,
  // `IntValue`.
  Always,
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

  template <typename TypedNodeId>
  class Definition;

  // Information about a definition. See associated accessors below for
  // comments.
  struct DefinitionInfo {
    llvm::StringLiteral ir_name;
    InstIsType is_type = InstIsType::Never;
    InstConstantKind constant_kind = InstConstantKind::Never;
    TerminatorKind terminator_kind = TerminatorKind::NotTerminator;
    bool is_lowered = true;
    bool deduce_through = false;
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

  // Returns whether this instruction kind is expected to produce a value.
  auto value_kind() const -> InstValueKind;

  // Returns this instruction kind's category of allowed constants.
  auto constant_kind() const -> InstConstantKind {
    return definition_info(*this).constant_kind;
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

  // Compute a fingerprint for this instruction kind, allowing its use as part
  // of the key in a `FoldingSet`.
  auto Profile(llvm::FoldingSetNodeID& id) -> void { id.AddInteger(AsInt()); }

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

  // Returns this instruction kind's category of allowed constants.
  constexpr auto constant_kind() const -> InstConstantKind {
    return info_.constant_kind;
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
