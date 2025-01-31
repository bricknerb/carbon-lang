// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_DIAGNOSTICS_DIAGNOSTIC_EMITTER_H_
#define CARBON_TOOLCHAIN_DIAGNOSTICS_DIAGNOSTIC_EMITTER_H_

#include <cstdint>
#include <string>
#include <type_traits>
#include <utility>

#include "common/check.h"
#include "llvm/ADT/Any.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FormatVariadic.h"
#include "toolchain/diagnostics/diagnostic.h"
#include "toolchain/diagnostics/diagnostic_consumer.h"
#include "toolchain/diagnostics/diagnostic_converter.h"
#include "toolchain/diagnostics/diagnostic_kind.h"

namespace Carbon {

namespace Internal {

// Disable type deduction based on `args`; the type of `diagnostic_base`
// determines the diagnostic's parameter types.
template <typename Arg>
using NoTypeDeduction = std::type_identity_t<Arg>;

}  // namespace Internal

template <typename LocT, typename AnnotateFn>
class DiagnosticAnnotationScope;

// Manages the creation of reports, the testing if diagnostics are enabled, and
// the collection of reports.
//
// This class is parameterized by a location type, allowing different
// diagnostic clients to provide location information in whatever form is most
// convenient for them, such as a position within a buffer when lexing, a token
// when parsing, or a parse tree node when type-checking, and to allow unit
// tests to be decoupled from any concrete location representation.
template <typename LocT>
class DiagnosticEmitter {
 public:
  // A builder-pattern type to provide a fluent interface for constructing
  // a more complex diagnostic. See `DiagnosticEmitter::Build` for the
  // expected usage.
  // This is nodiscard to protect against accidentally building a diagnostic
  // without emitting it.
  class [[nodiscard]] DiagnosticBuilder {
   public:
    // DiagnosticBuilder is move-only and cannot be copied.
    DiagnosticBuilder(DiagnosticBuilder&&) noexcept = default;
    auto operator=(DiagnosticBuilder&&) noexcept
        -> DiagnosticBuilder& = default;

    // Adds a note diagnostic attached to the main diagnostic being built.
    // The API mirrors the main emission API: `DiagnosticEmitter::Emit`.
    // For the expected usage see the builder API: `DiagnosticEmitter::Build`.
    template <typename... Args>
    auto Note(LocT loc, const DiagnosticBase<Args...>& diagnostic_base,
              Internal::NoTypeDeduction<Args>... args) -> DiagnosticBuilder& {
      if (!emitter_) {
        return *this;
      }
      CARBON_CHECK(diagnostic_base.Level == DiagnosticLevel::Note ||
                       diagnostic_base.Level == DiagnosticLevel::LocationInfo,
                   "{0}", static_cast<int>(diagnostic_base.Level));
      AddMessage(loc, diagnostic_base, {emitter_->MakeAny<Args>(args)...});
      return *this;
    }

    // Emits the built diagnostic and its attached notes.
    // For the expected usage see the builder API: `DiagnosticEmitter::Build`.
    template <typename... Args>
    auto Emit() -> void {
      if (!emitter_) {
        return;
      }
      for (auto annotate_fn : llvm::reverse(emitter_->annotate_fns_)) {
        annotate_fn(*this);
      }
      emitter_->consumer_->HandleDiagnostic(std::move(diagnostic_));
    }

    // Returns true if this DiagnosticBuilder may emit a diagnostic. Can be used
    // to avoid excess work computing notes, etc, if no diagnostic is going to
    // be emitted anyway.
    explicit operator bool() { return emitter_; }

   private:
    friend class DiagnosticEmitter<LocT>;

    template <typename... Args>
    explicit DiagnosticBuilder(DiagnosticEmitter<LocT>* emitter, LocT loc,
                               const DiagnosticBase<Args...>& diagnostic_base,
                               llvm::SmallVector<llvm::Any> args)
        : emitter_(emitter), diagnostic_({.level = diagnostic_base.Level}) {
      AddMessage(loc, diagnostic_base, std::move(args));
      CARBON_CHECK(diagnostic_base.Level != DiagnosticLevel::Note);
    }

    // Create a null `DiagnosticBuilder` that will not emit anything. Notes will
    // be silently ignored.
    DiagnosticBuilder() : emitter_(nullptr) {}

    // Adds a message to the diagnostic, handling conversion of the location and
    // arguments.
    template <typename... Args>
    auto AddMessage(LocT loc, const DiagnosticBase<Args...>& diagnostic_base,
                    llvm::SmallVector<llvm::Any> args) -> void {
      if (!emitter_) {
        return;
      }
      auto converted = emitter_->converter_->ConvertLoc(
          loc, [&](DiagnosticLoc context_loc,
                   const DiagnosticBase<>& context_diagnostic_base) {
            AddMessageWithDiagnosticLoc(context_loc, context_diagnostic_base,
                                        {});
          });
      // Use the last byte offset from the first message.
      if (diagnostic_.messages.empty()) {
        diagnostic_.last_byte_offset = converted.last_byte_offset;
      }
      AddMessageWithDiagnosticLoc(converted.loc, diagnostic_base, args);
    }

    // Adds a message to the diagnostic, handling conversion of the arguments. A
    // DiagnosticLoc must be provided instead of a LocT in order to
    // avoid potential recursion.
    template <typename... Args>
    auto AddMessageWithDiagnosticLoc(
        DiagnosticLoc loc, const DiagnosticBase<Args...>& diagnostic_base,
        llvm::SmallVector<llvm::Any> args) -> void {
      if (!emitter_) {
        return;
      }
      diagnostic_.messages.emplace_back(DiagnosticMessage{
          .kind = diagnostic_base.Kind,
          .level = diagnostic_base.Level,
          .loc = loc,
          .format = diagnostic_base.Format,
          .format_args = std::move(args),
          .format_fn = [](const DiagnosticMessage& message) -> std::string {
            return FormatFn<Args...>(
                message, std::make_index_sequence<sizeof...(Args)>());
          }});
    }

    // Handles the cast of llvm::Any to Args types for formatv.
    // TODO: Custom formatting can be provided with an format_provider, but that
    // affects all formatv calls. Consider replacing formatv with a custom call
    // that allows diagnostic-specific formatting.
    template <typename... Args, size_t... N>
    static auto FormatFn(const DiagnosticMessage& message,
                         std::index_sequence<N...> /*indices*/) -> std::string {
      static_assert(sizeof...(Args) == sizeof...(N), "Invalid template args");
      CARBON_CHECK(message.format_args.size() == sizeof...(Args),
                   "Argument count mismatch on {0}: {1} != {2}", message.kind,
                   message.format_args.size(), sizeof...(Args));
      return llvm::formatv(
          message.format.data(),
          llvm::any_cast<
              typename Internal::DiagnosticTypeForArg<Args>::StorageType>(
              message.format_args[N])...);
    }

    DiagnosticEmitter<LocT>* emitter_;
    Diagnostic diagnostic_;
  };

  // The `converter` and `consumer` are required to outlive the diagnostic
  // emitter.
  // TODO: Adjust construction to take stored arguments as pointers, and
  // consider taking `converter` by value (move/copy) instead of reference.
  explicit DiagnosticEmitter(DiagnosticConverter<LocT>& converter,
                             DiagnosticConsumer& consumer)
      : converter_(&converter), consumer_(&consumer) {}

  ~DiagnosticEmitter() = default;

  // Emits an error.
  //
  // When passing arguments, they may be buffered. As a consequence, lifetimes
  // may outlive the `Emit` call.
  template <typename... Args>
  auto Emit(LocT loc, const DiagnosticBase<Args...>& diagnostic_base,
            Internal::NoTypeDeduction<Args>... args) -> void {
    DiagnosticBuilder(this, loc, diagnostic_base, {MakeAny<Args>(args)...})
        .Emit();
  }

  // A fluent interface for building a diagnostic and attaching notes for added
  // context or information. For example:
  //
  //   emitter_.Build(loc1, MyDiagnostic)
  //     .Note(loc2, MyDiagnosticNote)
  //     .Emit();
  template <typename... Args>
  auto Build(LocT loc, const DiagnosticBase<Args...>& diagnostic_base,
             Internal::NoTypeDeduction<Args>... args) -> DiagnosticBuilder {
    return DiagnosticBuilder(this, loc, diagnostic_base,
                             {MakeAny<Args>(args)...});
  }

  // Create a null `DiagnosticBuilder` that will not emit anything. Notes will
  // be silently ignored.
  auto BuildSuppressed() -> DiagnosticBuilder { return DiagnosticBuilder(); }

 private:
  // Converts an argument to llvm::Any for storage, handling input to storage
  // type conversion when needed.
  template <typename Arg>
  auto MakeAny(Arg arg) -> llvm::Any {
    llvm::Any converted = converter_->ConvertArg(arg);
    using Storage = Internal::DiagnosticTypeForArg<Arg>::StorageType;
    CARBON_CHECK(
        llvm::any_cast<Storage>(&converted),
        "Failed to convert argument of type {0} to its storage type {1}",
        typeid(Arg).name(), typeid(Storage).name());
    return converted;
  }

  template <typename OtherLocT, typename AnnotateFn>
  friend class DiagnosticAnnotationScope;

  DiagnosticConverter<LocT>* converter_;
  DiagnosticConsumer* consumer_;
  llvm::SmallVector<llvm::function_ref<auto(DiagnosticBuilder& builder)->void>>
      annotate_fns_;
};

// This relies on `void*` location handling on `DiagnosticEmitter`.
//
// TODO: Based on how this ends up used or if we get more distinct emitters, it
// might be worth considering having diagnostics specify that they don't apply
// to source-location carrying emitters. For example, this might look like a
// `CARBON_NO_LOC_DIAGNOSTIC` macro, or some other factoring. But it might end
// up being more noise than it is worth.
class NoLocDiagnosticEmitter : public DiagnosticEmitter<void*> {
 public:
  // This constructor only applies to NoLocDiagnosticEmitter.
  explicit NoLocDiagnosticEmitter(DiagnosticConsumer* consumer)
      : DiagnosticEmitter(converter_, *consumer) {}

  // Emits an error. This specialization only applies to
  // `NoLocDiagnosticEmitter`.
  template <typename... Args>
  auto Emit(const DiagnosticBase<Args...>& diagnostic_base,
            Internal::NoTypeDeduction<Args>... args) -> void {
    DiagnosticEmitter::Emit(nullptr, diagnostic_base, args...);
  }

 private:
  struct Converter : DiagnosticConverter<void*> {
    auto ConvertLoc(void* /*loc*/, ContextFnT /*context_fn*/) const
        -> ConvertedDiagnosticLoc override {
      return {.loc = {.filename = ""}, .last_byte_offset = -1};
    }
  };
  Converter converter_;
};

// An RAII object that denotes a scope in which any diagnostic produced should
// be annotated in some way.
//
// This object is given a function `annotate` that will be called with a
// `DiagnosticBuilder& builder` for any diagnostic that is emitted through the
// given emitter. That function can annotate the diagnostic by calling
// `builder.Note` to add notes.
template <typename LocT, typename AnnotateFn>
class DiagnosticAnnotationScope {
 public:
  DiagnosticAnnotationScope(DiagnosticEmitter<LocT>* emitter,
                            AnnotateFn annotate)
      : emitter_(emitter), annotate_(std::move(annotate)) {
    emitter_->annotate_fns_.push_back(annotate_);
  }
  ~DiagnosticAnnotationScope() { emitter_->annotate_fns_.pop_back(); }

 private:
  DiagnosticEmitter<LocT>* emitter_;
  // Make a copy of the annotation function to ensure that it lives long enough.
  AnnotateFn annotate_;
};

template <typename LocT, typename AnnotateFn>
DiagnosticAnnotationScope(DiagnosticEmitter<LocT>* emitter, AnnotateFn annotate)
    -> DiagnosticAnnotationScope<LocT, AnnotateFn>;

}  // namespace Carbon

#endif  // CARBON_TOOLCHAIN_DIAGNOSTICS_DIAGNOSTIC_EMITTER_H_
