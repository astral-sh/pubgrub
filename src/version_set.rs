// SPDX-License-Identifier: MPL-2.0

//! As its name suggests, the [VersionSet] trait describes sets of versions.
//!
//! One needs to define
//! - the associate type for versions,
//! - two constructors for the empty set and a singleton set,
//! - the complement and intersection set operations,
//! - and a function to evaluate membership of versions.
//!
//! Two functions are automatically derived, thanks to the mathematical properties of sets.
//! You can overwrite those implementations, but we highly recommend that you don't,
//! except if you are confident in a correct implementation that brings much performance gains.
//!
//! It is also extremely important that the `Eq` trait is correctly implemented.
//! In particular, you can only use `#[derive(Eq, PartialEq)]` if `Eq` is strictly equivalent to the
//! structural equality, i.e. if version sets have canonical representations.
//! Such problems may arise if your implementations of `complement()` and `intersection()` do not
//! return canonical representations so be careful there.

use std::fmt::{Debug, Display};

use crate::Ranges;

/// A set of versions.
///
/// See [`Ranges`] for an implementation.
///
/// Two version sets that contain the same versions must be equal.
///
/// The methods with default implementations can be overwritten for better performance, but their
/// output must be equal to the default implementation.
pub trait VersionSet: Debug + Display + Clone + Eq {
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

    /// Whether the range have no overlapping segments.
    fn is_disjoint(&self, other: &Self) -> bool {
        self.intersection(other) == Self::empty()
    }

    /// Whether all range of `self` are contained in `other`.
    fn subset_of(&self, other: &Self) -> bool {
        self == &self.intersection(other)
    }
}

/// [`Ranges`] contains optimized implementations of all operations.
impl<T: Debug + Display + Clone + Eq + Ord> VersionSet for Ranges<T> {
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

    fn is_disjoint(&self, other: &Self) -> bool {
        Ranges::is_disjoint(self, other)
    }

    fn subset_of(&self, other: &Self) -> bool {
        Ranges::subset_of(self, other)
    }
}
