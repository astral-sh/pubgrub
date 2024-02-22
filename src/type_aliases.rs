// SPDX-License-Identifier: MPL-2.0

//! Publicly exported type aliases.

/// Map implementation used by the library.
pub type Map<K, V> = rustc_hash::FxHashMap<K, V>;

/// Set implementation used by the library.
pub type Set<V> = rustc_hash::FxHashSet<V>;

/// Concrete dependencies picked by the library during [resolve](crate::solver::resolve).
pub type SelectedDependencies<P, V> = Map<P, V>;
