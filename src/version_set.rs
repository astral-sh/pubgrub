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
///
/// # Candidate-selection metadata
///
/// A version set may carry metadata that affects how a
/// [`DependencyProvider`][crate::DependencyProvider] chooses between versions without affecting
/// which versions the set contains. This metadata must not participate in [`Eq`] or [`Hash`], which
/// remain defined by version membership. Instead, implementations must override
/// [`VersionSet::selection_eq`] when candidate selection depends on such metadata.
/// PubGrub treats the metadata as opaque: implementations are responsible for propagating it
/// through set operations into the values passed to the dependency provider. PubGrub uses the
/// metadata only to avoid compacting dependencies with different selection behavior; it does not
/// propagate metadata from a constraint that is already satisfied by version membership.
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

    /// Whether the version is part of this set.
    fn contains(&self, v: &Self::V) -> bool;

    /// Whether two sets have the same version membership and candidate-selection behavior.
    ///
    /// PubGrub uses this method when deciding whether dependency constraints can be merged without
    /// changing the candidates chosen by the dependency provider. The default assumes candidate
    /// selection depends only on version membership. Implementations that carry additional
    /// selection metadata must override it.
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

    /// The set of versions that are in `self` but not in `other`.
    ///
    /// Implementations can override this to avoid materializing the complement.
    fn difference(&self, other: &Self) -> Self {
        self.intersection(&other.complement())
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

    fn contains(&self, v: &Self::V) -> bool {
        Ranges::contains(self, v)
    }

    fn full() -> Self {
        Ranges::full()
    }

    fn union(&self, other: &Self) -> Self {
        Ranges::union(self, other)
    }

    fn difference(&self, other: &Self) -> Self {
        Ranges::difference(self, other)
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
