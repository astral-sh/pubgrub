// SPDX-License-Identifier: MPL-2.0

use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::{Ranges, SetRelation};

/// A set of versions.
///
/// See [`Ranges`] for an implementation.
///
/// The methods with default implementations can be overwritten for better performance, but their
/// output must be equal to the default implementation.
///
/// # Equality and hashing
///
/// It is important that the `Eq` and `Hash` traits are implemented so that if two sets contain the
/// same versions, they are equal under `Eq` and produce the same hash. In particular, you can only
/// derive these traits if equality is strictly equivalent to structural equality, i.e. if version
/// sets are always stored in canonical representations. Such problems may arise if your
/// implementations of `complement()` and `intersection()` do not return canonical representations.
///
/// For example, `>=1,<4 || >=2,<5` and `>=1,<4 || >=3,<5` are equal, because they can both be
/// normalized to `>=1,<5`.
///
/// Note that pubgrub does not know which versions actually exist for a package, the contract
/// is about upholding the mathematical properties of set operations, assuming all versions are
/// possible. This is required for the solver to determine the relationship of version sets to each
/// other.
pub trait VersionSet: Debug + Display + Clone + Eq + Hash {
    /// Version type associated with the sets manipulated.
    type V: Debug + Display + Clone + Ord;

    // Constructors

    /// An empty set containing no version.
    fn empty() -> Self;

    /// A set containing only the given version.
    fn singleton(v: Self::V) -> Self;

    // Operations

    /// The set of all version that are not in this set.
    fn complement(&self) -> Self;

    /// The set of all versions that are in both sets.
    fn intersection(&self, other: &Self) -> Self;

    /// The set of all versions that are in `self` but not in `other`.
    ///
    /// The default implementation intersects the complement of `other` with `self`. The operand
    /// order is intentional because intersection may propagate candidate-selection metadata
    /// directionally. Implementations can override this method to avoid constructing the
    /// complement, but the result must be selection-equivalent to the default expression under
    /// [`VersionSet::selection_eq`].
    fn difference(&self, other: &Self) -> Self {
        other.complement().intersection(self)
    }

    /// Whether the version is part of this set.
    fn contains(&self, v: &Self::V) -> bool;

    /// Whether two sets have the same version membership and candidate-selection behavior.
    ///
    /// A version set may carry metadata that changes which contained version
    /// [`DependencyProvider::choose_version`](crate::DependencyProvider::choose_version) returns
    /// without changing the set's version membership. Dependency incompatibilities carrying
    /// different selection metadata must remain distinct even though the sets compare equal.
    /// Selection metadata must not change whether a candidate exists: for the same package and any
    /// two sets where `self == other`, a dependency provider must return `Some` from
    /// `choose_version` for both or `None` for both. PubGrub records a membership-based
    /// incompatibility after `choose_version` returns `None`, so that result must remain valid
    /// across selection-metadata refinements.
    ///
    /// This method must be an equivalence relation, and returning `true` requires `self == other`.
    /// It may return `false` for sets that compare equal when their selection metadata differs.
    /// It must also be a congruence for every [`VersionSet`] operation: applying the same operation
    /// to selection-equivalent operands must produce selection-equivalent results. Dependency
    /// providers must treat selection-equivalent ranges as interchangeable in
    /// [`DependencyProvider::prioritize`][crate::DependencyProvider::prioritize] and
    /// [`DependencyProvider::choose_version`][crate::DependencyProvider::choose_version].
    fn selection_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Whether this set may refine candidate selection without narrowing version membership.
    ///
    /// PubGrub uses this as a fast path before consulting [`VersionSet::selection_refinement`].
    /// Implementations that override that method must also override this one and return `true` for
    /// every set that could contribute a refinement.
    fn may_refine_selection(&self) -> bool {
        false
    }

    /// Refine candidate-selection metadata with a logically redundant constraint.
    ///
    /// PubGrub calls this method when an active dependency requirement already contains the
    /// package's current version set, but may still contribute metadata that affects candidate
    /// selection. Implementations that carry such metadata should return the result of combining
    /// `self` with `requirement`, or `None` when candidate selection would not change. The returned
    /// set must compare equal to `self` and must not be selection-equivalent to it. PubGrub stores
    /// the returned value directly; it does not need to be reproducible by [`Self::intersection`].
    ///
    /// For a fixed version membership, refinements must compose associatively, commutatively, and
    /// idempotently up to [`Self::selection_eq`]. Applying a collection of logically redundant
    /// requirements in any order and any number of times must produce selection-equivalent
    /// results. In particular, reapplying a requirement to the value it already refined must
    /// return `None`.
    ///
    /// The default assumes candidate selection depends only on version membership. Implementations
    /// whose logically redundant constraints can add candidate-selection metadata must override
    /// this method and [`VersionSet::may_refine_selection`].
    fn selection_refinement(&self, _requirement: &Self) -> Option<Self> {
        None
    }

    // Automatically implemented functions

    /// The set containing all versions.
    ///
    /// The default implementation is the complement of the empty set.
    fn full() -> Self {
        Self::empty().complement()
    }

    /// The set of all versions that are either (or both) of the sets.
    ///
    /// The default implementation is complement of the intersection of the complements of both sets
    /// (De Morgan's law).
    fn union(&self, other: &Self) -> Self {
        self.complement()
            .intersection(&other.complement())
            .complement()
    }

    /// Whether the ranges have no overlapping segments.
    fn is_disjoint(&self, other: &Self) -> bool {
        self.intersection(other) == Self::empty()
    }

    /// Whether all ranges of `self` are contained in `other`.
    fn subset_of(&self, other: &Self) -> bool {
        self == &self.intersection(other)
    }

    /// Classifies `self` as a subset of, disjoint from, or partially overlapping with `other`.
    ///
    /// Implementations can override this to avoid traversing both sets once for [`Self::subset_of`]
    /// and again for [`Self::is_disjoint`].
    /// An empty `self` must be classified as [`SetRelation::Subset`].
    fn relation(&self, other: &Self) -> SetRelation {
        if self.subset_of(other) {
            SetRelation::Subset
        } else if self.is_disjoint(other) {
            SetRelation::Disjoint
        } else {
            SetRelation::Overlapping
        }
    }
}

/// [`Ranges`] contains optimized implementations of all operations.
impl<T: Debug + Display + Clone + Eq + Ord + Hash> VersionSet for Ranges<T> {
    type V = T;

    fn empty() -> Self {
        Ranges::empty()
    }

    fn singleton(v: Self::V) -> Self {
        Ranges::singleton(v)
    }

    fn complement(&self) -> Self {
        Ranges::complement(self)
    }

    fn intersection(&self, other: &Self) -> Self {
        Ranges::intersection(self, other)
    }

    fn difference(&self, other: &Self) -> Self {
        Ranges::difference(self, other)
    }

    fn contains(&self, v: &Self::V) -> bool {
        Ranges::contains(self, v)
    }

    fn full() -> Self {
        Ranges::full()
    }

    fn union(&self, other: &Self) -> Self {
        Ranges::union(self, other)
    }

    fn is_disjoint(&self, other: &Self) -> bool {
        Ranges::is_disjoint(self, other)
    }

    fn subset_of(&self, other: &Self) -> bool {
        Ranges::subset_of(self, other)
    }

    fn relation(&self, other: &Self) -> SetRelation {
        Ranges::relation(self, other)
    }
}
