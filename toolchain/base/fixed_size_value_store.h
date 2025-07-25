// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_BASE_FIXED_SIZE_VALUE_STORE_H_
#define CARBON_TOOLCHAIN_BASE_FIXED_SIZE_VALUE_STORE_H_

#include "common/check.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "toolchain/base/mem_usage.h"
#include "toolchain/base/value_store_types.h"

namespace Carbon {

// A value store with a predetermined size.
template <typename IdT, typename ValueT>
class FixedSizeValueStore {
 public:
  using IdType = IdT;
  using ValueType = ValueStoreTypes<ValueT>::ValueType;
  using RefType = ValueStoreTypes<ValueT>::RefType;
  using ConstRefType = ValueStoreTypes<ValueT>::ConstRefType;

  // Makes a ValueStore of the specified size, but without initializing values.
  // Entries must be set before reading.
  static auto MakeForOverwriteWithExplicitSize(size_t size)
      -> FixedSizeValueStore {
    FixedSizeValueStore store;
    store.values_.resize_for_overwrite(size);
    return store;
  }

  // Makes a ValueStore of the same size as a source `ValueStoreT`, but without
  // initializing values. Entries must be set before reading.
  template <typename ValueStoreT>
    requires std::same_as<IdT, typename ValueStoreT::IdType>
  static auto MakeForOverwrite(const ValueStoreT& size_source)
      -> FixedSizeValueStore {
    FixedSizeValueStore store;
    store.values_.resize_for_overwrite(size_source.size());
    return store;
  }

  // Makes a ValueStore of the specified size, initialized to a default.
  static auto MakeWithExplicitSize(size_t size, ConstRefType default_value)
      -> FixedSizeValueStore {
    FixedSizeValueStore store;
    store.values_.resize(size, default_value);
    return store;
  }

  // Makes a ValueStore of the same size as a source `ValueStoreT`. This is
  // the safest constructor to use, since it ensures everything's initialized to
  // a default, and verifies a matching `IdT` for the size.
  template <typename ValueStoreT>
    requires std::same_as<IdT, typename ValueStoreT::IdType>
  explicit FixedSizeValueStore(const ValueStoreT& size_source,
                               ConstRefType default_value) {
    values_.resize(size_source.size(), default_value);
  }

  // Makes a ValueStore using a mapped range of `source`. The `factory_fn`
  // receives each enumerated entry for construction of `ValueType`.
  template <typename ValueStoreT>
    requires std::same_as<IdT, typename ValueStoreT::IdType>
  explicit FixedSizeValueStore(
      const ValueStoreT& source,
      llvm::function_ref<
          auto(IdT, typename ValueStoreT::ConstRefType)->ValueType>
          factory_fn)
      : values_(llvm::map_range(source.enumerate(), factory_fn)) {}

  // Move-only.
  FixedSizeValueStore(FixedSizeValueStore&&) noexcept = default;
  auto operator=(FixedSizeValueStore&&) noexcept
      -> FixedSizeValueStore& = default;

  // Sets the value for an ID.
  auto Set(IdT id, ValueType value) -> void {
    CARBON_DCHECK(id.index >= 0, "{0}", id);
    values_[id.index] = value;
  }

  // Returns a mutable value for an ID.
  auto Get(IdT id) -> RefType {
    CARBON_DCHECK(id.index >= 0, "{0}", id);
    return values_[id.index];
  }

  // Returns the value for an ID.
  auto Get(IdT id) const -> ConstRefType {
    CARBON_DCHECK(id.index >= 0, "{0}", id);
    return values_[id.index];
  }

  // Collects memory usage of the values.
  auto CollectMemUsage(MemUsage& mem_usage, llvm::StringRef label) const
      -> void {
    mem_usage.Collect(label.str(), values_);
  }

  auto size() const -> size_t { return values_.size(); }

  auto values()
      -> llvm::iterator_range<typename llvm::SmallVector<ValueT, 0>::iterator> {
    return llvm::make_range(values_.begin(), values_.end());
  }

  auto values() const -> llvm::iterator_range<
      typename llvm::SmallVector<ValueT, 0>::const_iterator> {
    return llvm::make_range(values_.begin(), values_.end());
  }

 private:
  // Allow default construction for `Make` functions.
  FixedSizeValueStore() = default;

  // Storage for the `ValueT` objects, indexed by the id.
  llvm::SmallVector<ValueT, 0> values_;
};

}  // namespace Carbon

#endif  // CARBON_TOOLCHAIN_BASE_FIXED_SIZE_VALUE_STORE_H_
