// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_LOWER_FUNCTION_CONTEXT_H_
#define CARBON_TOOLCHAIN_LOWER_FUNCTION_CONTEXT_H_

#include "common/map.h"
#include "common/raw_string_ostream.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "toolchain/lower/file_context.h"
#include "toolchain/sem_ir/file.h"
#include "toolchain/sem_ir/ids.h"

namespace Carbon::Lower {

// Context and shared functionality for lowering handlers that produce an
// `llvm::Function` definition.
class FunctionContext {
 public:
  // `function` must not be null. `function_fingerprint` and `di_subprogram` may
  // be null (see members).
  explicit FunctionContext(
      FileContext& file_context, llvm::Function* function,
      FileContext& specific_file_context, SemIR::SpecificId specific_id,
      FileContext::SpecificFunctionFingerprint* function_fingerprint,
      llvm::DISubprogram* di_subprogram, llvm::raw_ostream* vlog_stream);

  // Describes a function's body fingerprint while creating the function body.
  // The final fingerprint is stored in the `FileContext` as a
  // `SpecificFunctionFingerprint`.
  //
  // Create two function fingerprints, where both fingerprints include data
  // that's evaluated (and hence lowered) differently based on the
  // `SpecificId`. `common_fingerprint` includes global values, types
  // and `FunctionId` for functions called inside the function body.
  // `specific_fingerprint` includes `SpecificId`s for functions called.
  //
  // For two specifics of the same generic:
  // - If `common_fingerprint` is different, the specifics cannot be coalesced.
  // - If `common_fingerprint` and `specific_fingerprint` are the
  //   same, the specifics can be coalesced without additional checks.
  // - If `common_fingerprint` is the same but `specific_fingerprint` is
  //   different, additional checks are needed, i.e. inspecting the non-hashed
  //   `SpecificId`s.
  //
  // TODO: Consider optimizations for repeated entries in both fingerprints.
  struct LoweringFunctionFingerprint {
    llvm::BLAKE3 common_fingerprint;
    llvm::BLAKE3 specific_fingerprint;
  };

  // Returns a basic block corresponding to the start of the given semantics
  // block, and enqueues it for emission.
  auto GetBlock(SemIR::InstBlockId block_id) -> llvm::BasicBlock*;

  // If we have not yet allocated a `BasicBlock` for this `block_id`, set it to
  // `block`, and enqueue `block_id` for emission. Returns whether we set the
  // block.
  auto TryToReuseBlock(SemIR::InstBlockId block_id, llvm::BasicBlock* block)
      -> bool;

  // Builds LLVM IR for the sequence of instructions in `block_id`.
  auto LowerBlockContents(SemIR::InstBlockId block_id) -> void;

  // Builds LLVM IR for the specified instruction.
  auto LowerInst(SemIR::InstId inst_id) -> void;

  // Returns a phi node corresponding to the block argument of the given basic
  // block.
  auto GetBlockArg(SemIR::InstBlockId block_id, SemIR::TypeId type_id)
      -> llvm::PHINode*;

  // Returns a value for the given instruction.
  auto GetValue(SemIR::InstId inst_id) -> llvm::Value*;

  // Sets the value for the given instruction.
  auto SetLocal(SemIR::InstId inst_id, llvm::Value* value) -> void {
    bool added = locals_.Insert(inst_id, value).is_inserted();
    CARBON_CHECK(added, "Duplicate local insert: {0} {1}", inst_id,
                 sem_ir().insts().Get(inst_id));
  }

  // Gets a callable's function.
  auto GetFunction(SemIR::FunctionId function_id) -> llvm::Function* {
    return file_context_->GetFunction(function_id);
  }

  // Gets or creates a callable's function.
  auto GetOrCreateFunction(SemIR::FunctionId function_id,
                           SemIR::SpecificId specific_id) -> llvm::Function* {
    return file_context_->GetOrCreateFunction(function_id, specific_id);
  }

  // Builds LLVM function type information for the specified function.
  auto BuildFunctionTypeInfo(const SemIR::Function& function,
                             SemIR::SpecificId specific_id)
      -> FileContext::FunctionTypeInfo {
    return file_context_->BuildFunctionTypeInfo(function, specific_id);
  }

  // Returns a lowered type for the given type_id.
  auto GetType(SemIR::TypeId type_id) -> llvm::Type* {
    return file_context_->GetType(type_id);
  }

  // Returns the type of the given instruction in the current specific.
  auto GetTypeOfInstInSpecific(SemIR::InstId inst_id) -> llvm::Type* {
    auto [type_file, type_id] = GetTypeIdOfInstInSpecific(inst_id);
    auto* type = GetFileContext(type_file).GetType(type_id);
    AddTypeToCurrentFingerprint(type);
    return type;
  }

  // Returns the type of the given instruction in the current specific.
  // TODO: Each caller of this should add information to the fingerprint
  // indicating what information they used from the type.
  auto GetTypeIdOfInstInSpecific(SemIR::InstId inst_id)
      -> std::pair<const SemIR::File*, SemIR::TypeId>;

  // Returns a lowered value to use for a value of type `type`.
  auto GetTypeAsValue() -> llvm::Value* {
    return file_context_->GetTypeAsValue();
  }

  // Returns a lowered value to use for a value of int literal type.
  auto GetIntLiteralAsValue() -> llvm::Constant* {
    return file_context_->GetIntLiteralAsValue();
  }

  // Returns the instruction immediately after all the existing static allocas.
  // This is the insert point for future static allocas.
  auto GetInstructionAfterAllocas() const -> llvm::Instruction* {
    return after_allocas_;
  }

  // Sets the instruction after static allocas. This should be called once,
  // after the first alloca is created.
  auto SetInstructionAfterAllocas(llvm::Instruction* after_allocas) -> void {
    CARBON_CHECK(!after_allocas_);
    after_allocas_ = after_allocas;
  }

  // Create a synthetic block that corresponds to no SemIR::InstBlockId. Such
  // a block should only ever have a single predecessor, and is used when we
  // need multiple `llvm::BasicBlock`s to model the linear control flow in a
  // single SemIR::File block.
  auto MakeSyntheticBlock() -> llvm::BasicBlock*;

  // Determine whether block is the most recently created synthetic block.
  auto IsCurrentSyntheticBlock(llvm::BasicBlock* block) -> bool {
    return synthetic_block_ == block;
  }

  // Returns the debug location to associate with the specified instruction.
  auto GetDebugLoc(SemIR::InstId inst_id) -> llvm::DebugLoc;

  // After emitting an initializer `init_id`, finishes performing the
  // initialization of `dest_id` from that initializer. This is a no-op if the
  // initialization was performed in-place, and otherwise performs a store or a
  // copy.
  auto FinishInit(SemIR::TypeId type_id, SemIR::InstId dest_id,
                  SemIR::InstId source_id) -> void;

  // When fingerprinting for a specific, adds the call, found in the function
  // body, to <function_id, specific_id>. `function_id` and `specific_id` are
  // IDs within the file identified by `function_file_id`.
  auto AddCallToCurrentFingerprint(SemIR::CheckIRId file_id,
                                   SemIR::FunctionId function_id,
                                   SemIR::SpecificId specific_id) -> void;

  // When fingerprinting for a specific, adds the type.
  auto AddTypeToCurrentFingerprint(llvm::Type* type) -> void;

  // Emits the final function fingerprints. Only called when function lowering
  // is complete.
  auto EmitFinalFingerprint() -> void;

  // Returns the FileContext to use for lowering in the given file.
  auto GetFileContext(const SemIR::File* file) -> FileContext& {
    // Avoid hash table lookup for the expected files.
    if (file == &sem_ir()) {
      return *file_context_;
    }
    if (file == &specific_sem_ir()) {
      return *specific_file_context_;
    }
    return file_context_->context().GetFileContext(file);
  }

  auto llvm_context() -> llvm::LLVMContext& {
    return file_context_->llvm_context();
  }
  auto llvm_module() -> llvm::Module& { return file_context_->llvm_module(); }
  auto llvm_function() -> llvm::Function& { return *function_; }
  auto builder() -> llvm::IRBuilderBase& { return builder_; }
  auto sem_ir() -> const SemIR::File& { return file_context_->sem_ir(); }

  // The file context for the file that `specific_id()` is within.
  auto specific_file_context() -> FileContext& {
    return *specific_file_context_;
  }
  // The file that `specific_id()` is within.
  auto specific_sem_ir() -> const SemIR::File& {
    return specific_file_context_->sem_ir();
  }
  // The specific ID for the function that is being lowered. Note that this is
  // an ID from `specific_sem_ir()`, not from `sem_ir()`.
  auto specific_id() -> SemIR::SpecificId { return specific_id_; }

  // TODO: could template on BuiltinFunctionKind if more format
  // globals are eventually needed.
  auto printf_int_format_string() -> llvm::Value* {
    auto* format_string = file_context_->printf_int_format_string();
    if (!format_string) {
      format_string = builder().CreateGlobalString("%d\n", "printf.int.format");
      file_context_->SetPrintfIntFormatString(format_string);
    }
    return format_string;
  }

 private:
  // Custom instruction inserter for our IR builder. Automatically names
  // instructions.
  class Inserter : public llvm::IRBuilderDefaultInserter {
   public:
    explicit Inserter(const SemIR::InstNamer* inst_namer)
        : inst_namer_(inst_namer) {}

    // Sets the instruction we are currently emitting.
    auto SetCurrentInstId(SemIR::InstId inst_id) -> void { inst_id_ = inst_id; }

   private:
    auto InsertHelper(llvm::Instruction* inst, const llvm::Twine& name,
                      llvm::BasicBlock::iterator insert_pt) const
        -> void override;

    // The instruction namer.
    const SemIR::InstNamer* inst_namer_;

    // The current instruction ID.
    SemIR::InstId inst_id_ = SemIR::InstId::None;
  };

  // Emits a value copy for type `type_id` from `source_id` to `dest_id`.
  // `source_id` must produce a value representation for `type_id`, and
  // `dest_id` must be a pointer to a `type_id` object.
  auto CopyValue(SemIR::TypeId type_id, SemIR::InstId source_id,
                 SemIR::InstId dest_id) -> void;

  // Emits an object representation copy for type `type_id` from `source_id` to
  // `dest_id`. `source_id` and `dest_id` must produce pointers to `type_id`
  // objects.
  auto CopyObject(SemIR::TypeId type_id, SemIR::InstId source_id,
                  SemIR::InstId dest_id) -> void;

  // When fingerprinting for a specific, adds the global.
  auto AddGlobalToCurrentFingerprint(llvm::Value* global) -> void;

  // Context for lowering in the file that contains this function's
  // instructions.
  FileContext* file_context_;

  // The IR function we're generating.
  llvm::Function* function_;

  // Context for lowering in the file that contains our `specific_id_`. Note
  // that this is a different file than the one referred to by `file_context_`
  // if we are lowering a specific that was generated for a generic function
  // defined in a different file.
  FileContext* specific_file_context_;

  // The specific id, if the function is a specific.
  SemIR::SpecificId specific_id_;

  // Builder for creating code in this function. The insertion point is held at
  // the location of the current SemIR instruction.
  llvm::IRBuilder<llvm::ConstantFolder, Inserter> builder_;

  // The instruction after all allocas. This is used as the insert point for new
  // allocas.
  llvm::Instruction* after_allocas_ = nullptr;

  llvm::DISubprogram* di_subprogram_;

  // The optional vlog stream.
  llvm::raw_ostream* vlog_stream_;

  // This is initialized and populated while lowering a specific function.
  // When complete, this is used to complete the function_fingerprint_.
  LoweringFunctionFingerprint current_fingerprint_;

  // The accumulated fingerprint is owned by the FileContext and passed into
  // the FunctionContext. The function fingerprint is currently only built for
  // specific functions, otherwise, this will be nullptr.
  FileContext::SpecificFunctionFingerprint* function_fingerprint_;

  // Maps a function's SemIR::File blocks to lowered blocks.
  Map<SemIR::InstBlockId, llvm::BasicBlock*> blocks_;

  // The synthetic block we most recently created. May be null if there is no
  // such block.
  llvm::BasicBlock* synthetic_block_ = nullptr;

  // Maps a function's SemIR::File instructions to lowered values.
  Map<SemIR::InstId, llvm::Value*> locals_;
};

// Provides handlers for instructions that occur in a FunctionContext. Although
// this is declared for all instructions, it should only be defined for
// instructions which are non-constant and not always typed. See
// `FunctionContext::LowerInst` for how this is used.
#define CARBON_SEM_IR_INST_KIND(Name)                              \
  auto HandleInst(FunctionContext& context, SemIR::InstId inst_id, \
                  SemIR::Name inst) -> void;
#include "toolchain/sem_ir/inst_kind.def"

}  // namespace Carbon::Lower

#endif  // CARBON_TOOLCHAIN_LOWER_FUNCTION_CONTEXT_H_
