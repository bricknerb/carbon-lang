// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_COMMON_TYPE_ENUM_H_
#define CARBON_COMMON_TYPE_ENUM_H_

#include <algorithm>

#include "common/ostream.h"

namespace Carbon {

// An enum whose values are the specified types.
template <typename... Types>
class TypeEnum : public Printable<TypeEnum<Types...>> {
 public:
  using TypeTuple = std::tuple<Types...>;

  static constexpr size_t NumTypes = sizeof...(Types);
  static constexpr size_t NumValues = NumTypes + 2;

  static_assert(NumValues <= 256, "Too many types for raw enum.");

  // The underlying raw enumeration type.
  //
  // The enum_extensibility attribute indicates that this enum is intended to
  // take values that do not correspond to its declared enumerators.
  enum class [[clang::enum_extensibility(open)]] RawEnumType : uint8_t {
    // The first sizeof...(Types) values correspond to the types.

    // An explicitly invalid value.
    Invalid = NumTypes,

    // Indicates that no type should be used.
    // TODO: This doesn't really fit the model of this type, but it's convenient
    // for all of its users.
    None,
  };

  // Accesses the type given an enum value.
  template <RawEnumType K>
    requires(K != RawEnumType::Invalid)
  using TypeFor = __type_pack_element<static_cast<size_t>(K), Types...>;

  // Workaround for Clang bug https://github.com/llvm/llvm-project/issues/85461
  template <RawEnumType Value>
  static constexpr auto FromRaw = TypeEnum(Value);

  // Names for the `Invalid` and `None` enumeration values.
  static constexpr const TypeEnum& Invalid = FromRaw<RawEnumType::Invalid>;
  static constexpr const TypeEnum& None = FromRaw<RawEnumType::None>;

  // Accesses the enumeration value for the type `Type`. If `AllowInvalid` is
  // set, any unexpected type is mapped to `Invalid`, otherwise an invalid type
  // results in a compile error.
  //
  // The `Self` parameter is an implementation detail to allow `ForImpl` to be
  // defined after this template, and should not be specified.
  template <typename Type, bool AllowInvalid = false, typename Self = TypeEnum>
  static constexpr auto For = Self::template ForImpl<Type, AllowInvalid>();

  // This bool indicates whether the specified type corresponds to a value in
  // this enum.
  template <typename Type>
  static constexpr bool Contains = For<Type, true>.is_valid();

  // Explicitly convert from the raw enum type.
  explicit constexpr TypeEnum(RawEnumType value) : value_(value) {}

  // Implicitly convert to the raw enum type, for use in `switch`.
  //
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr operator RawEnumType() const { return value_; }

  // Conversion to bool is deleted to prevent direct use in an `if` condition
  // instead of comparing with another value.
  explicit operator bool() const = delete;

  // Returns the raw enum value.
  constexpr auto ToRaw() const -> RawEnumType { return value_; }

  // Returns a value that can be used as an array index. Returned value will be
  // < NumValues.
  constexpr auto ToIndex() const -> size_t {
    return static_cast<size_t>(value_);
  }

  // Returns whether this is a valid value, not `Invalid`.
  constexpr auto is_valid() const -> bool {
    return value_ != RawEnumType::Invalid;
  }

  auto Print(llvm::raw_ostream& out) const -> void {
    out << "TypeEnum(";
    if (value_ == RawEnumType::None) {
      out << "None";
    } else {
      static constexpr std::array<llvm::StringLiteral, sizeof...(Types)> Names =
          {
              Types::Label...,
          };
      out << Names[static_cast<int>(value_)];
    }
    out << ")";
  }

 private:
  // Translates a type to its enum value, or `Invalid`.
  template <typename IdT, bool AllowInvalid>
  static constexpr auto ForImpl() -> TypeEnum {
    // A bool for each type saying whether it matches. The result is the index
    // of the first `true` in this list. If none matches, then the result is the
    // length of the list, which is mapped to `Invalid`.
    constexpr bool TypeMatches[] = {std::same_as<IdT, Types>...};
    constexpr int Index =
        std::find(TypeMatches, TypeMatches + NumTypes, true) - TypeMatches;
    static_assert(Index != NumTypes || AllowInvalid,
                  "Unexpected type passed to TypeEnum::For<...>");
    return TypeEnum(static_cast<RawEnumType>(Index));
  }

  RawEnumType value_;
};

}  // namespace Carbon

#endif  // CARBON_COMMON_TYPE_ENUM_H_
