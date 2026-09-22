// SPDX-License-Identifier: MPL-2.0

//! A term is the fundamental unit of operation of the PubGrub algorithm.
//! It is a positive or negative expression regarding a set of versions.

use std::fmt::{self, Display};

use crate::{SetRelation, VersionSet};

/// A positive or negative expression regarding a set of versions.
///
/// `positive(r)` and `negative(r.complement())` are not equivalent:
/// * the term `positive(r)` is satisfied if the package is selected AND the selected version is in `r`.
/// * the term `negative(r.complement())` is satisfied if the package is not selected OR the selected version is in `r`.
///
/// A positive term in the partial solution requires a version to be selected, but a negative term
/// allows for a solution that does not have that package selected.
/// Specifically, `positive(VS::empty())` means that there was a conflict (we need to select a version for the package
/// but can't pick any), while `negative(VS::full())` would mean it is fine as long as we don't select the package.
///
/// Equivalently, a term describes a set of `Option<Version>` values: a positive term contains
/// `Some(v)` for versions in its set, while a negative term contains `None` and `Some(v)` for
/// versions outside its set. Negating a term therefore only needs to flip its polarity.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Term<VS: VersionSet> {
    /// Whether the term excludes the set and allows the package to be unselected.
    pub negative: bool,
    /// The versions included by a positive term or excluded by a negative term.
    pub set: VS,
}

/// Base methods.
impl<VS: VersionSet> Term<VS> {
    /// A term requiring the package to be selected with a version in the set.
    pub fn positive(set: VS) -> Self {
        Self {
            set,
            negative: false,
        }
    }

    /// A term allowing the package to be unselected or have a version outside the set.
    pub fn negative(set: VS) -> Self {
        Self {
            set,
            negative: true,
        }
    }

    /// A term that is always true.
    pub(crate) fn any() -> Self {
        Self::negative(VS::empty())
    }

    /// A term that is never true.
    #[cfg(test)]
    pub(crate) fn empty() -> Self {
        Self::positive(VS::empty())
    }

    /// A positive term containing exactly that version.
    pub(crate) fn exact(version: VS::V) -> Self {
        Self::positive(VS::singleton(version))
    }

    /// Simply check if a term is positive.
    pub(crate) fn is_positive(&self) -> bool {
        !self.negative
    }

    /// Negate a term.
    /// Evaluation of a negated term always returns
    /// the opposite of the evaluation of the original one.
    pub(crate) fn negate(&self) -> Self {
        Self {
            negative: !self.negative,
            set: self.set.clone(),
        }
    }

    /// Evaluate a term regarding a given choice of version.
    pub(crate) fn contains(&self, v: &VS::V) -> bool {
        self.set.contains(v) ^ self.negative
    }

    /// Unwrap the set contained in a positive term.
    ///
    /// Panics if used on a negative set.
    pub(crate) fn unwrap_positive(&self) -> &VS {
        assert!(
            !self.negative,
            "Negative term cannot unwrap positive set: {:?}",
            self.set
        );
        &self.set
    }

    /// Unwrap the set contained in a negative term.
    ///
    /// Panics if used on a positive set.
    pub(crate) fn unwrap_negative(&self) -> &VS {
        assert!(
            self.negative,
            "Positive term cannot unwrap negative set: {:?}",
            self.set
        );
        &self.set
    }
}

/// Set operations with terms.
impl<VS: VersionSet> Term<VS> {
    /// Compute the intersection of two terms.
    ///
    /// The intersection is negative (unselected package is allowed)
    /// if all terms are negative.
    pub(crate) fn intersection(&self, other: &Self) -> Self {
        Self {
            set: match (self.negative, other.negative) {
                (false, false) => self.set.intersection(&other.set),
                (false, true) => self.set.difference(&other.set),
                (true, false) => other.set.difference(&self.set),
                (true, true) => self.set.union(&other.set),
            },
            negative: self.negative & other.negative,
        }
    }

    /// Intersect with the negation of another term without cloning its version set.
    pub(crate) fn difference(&self, other: &Self) -> Self {
        Self {
            set: match (self.negative, other.negative) {
                (false, false) => self.set.difference(&other.set),
                (false, true) => self.set.intersection(&other.set),
                (true, false) => self.set.union(&other.set),
                (true, true) => other.set.difference(&self.set),
            },
            negative: self.negative & !other.negative,
        }
    }

    /// Compute the union of two terms.
    /// If at least one term is negative, the union is also negative (unselected package is allowed).
    pub(crate) fn union(&self, other: &Self) -> Self {
        Self {
            set: match (self.negative, other.negative) {
                (false, false) => self.set.union(&other.set),
                (false, true) => other.set.difference(&self.set),
                (true, false) => self.set.difference(&other.set),
                (true, true) => self.set.intersection(&other.set),
            },
            negative: self.negative | other.negative,
        }
    }

    /// Indicate if this term is a subset of another term.
    /// Just like for sets, we say that t1 is a subset of t2
    /// if and only if t1 ∩ t2 = t1.
    pub(crate) fn subset_of(&self, other: &Self) -> bool {
        match (self.negative, other.negative) {
            (false, false) => self.set.subset_of(&other.set),
            (false, true) => self.set.is_disjoint(&other.set),
            // Only a negative term allows the unselected package,
            // so it can never be a subset of a positive term.
            (true, false) => false,
            (true, true) => other.set.subset_of(&self.set),
        }
    }
}

/// Describe a relation between a set of terms S and another term t.
///
/// As a shorthand, we say that a term v
/// satisfies or contradicts a term t if {v} satisfies or contradicts it.
pub(crate) enum Relation {
    /// We say that a set of terms S "satisfies" a term t
    /// if t must be true whenever every term in S is true.
    Satisfied,
    /// Conversely, S "contradicts" t if t must be false
    /// whenever every term in S is true.
    Contradicted,
    /// If neither of these is true we say that S is "inconclusive" for t.
    Inconclusive,
}

/// Relation between terms.
impl<VS: VersionSet> Term<VS> {
    /// Check if a set of terms satisfies this term.
    ///
    /// We say that a set of terms S "satisfies" a term t
    /// if t must be true whenever every term in S is true.
    ///
    /// It turns out that this can also be expressed with set operations:
    ///    S satisfies t if and only if  ⋂ S ⊆ t
    #[cfg(test)]
    fn satisfied_by(&self, terms_intersection: &Self) -> bool {
        terms_intersection.subset_of(self)
    }

    /// Check if a set of terms contradicts this term.
    ///
    /// We say that a set of terms S "contradicts" a term t
    /// if t must be false whenever every term in S is true.
    ///
    /// It turns out that this can also be expressed with set operations:
    ///    S contradicts t if and only if ⋂ S is disjoint with t
    ///    S contradicts t if and only if  (⋂ S) ⋂ t = ∅
    #[cfg(test)]
    fn contradicted_by(&self, terms_intersection: &Self) -> bool {
        terms_intersection.intersection(self) == Self::empty()
    }

    /// Check if a set of terms satisfies or contradicts a given term.
    /// Otherwise the relation is inconclusive.
    /// Satisfaction takes precedence when an empty positive intersection both satisfies and
    /// contradicts the term.
    pub(crate) fn relation_with(&self, other_terms_intersection: &Self) -> Relation {
        let range = &self.set;
        let other = &other_terms_intersection.set;
        let satisfied = if other_terms_intersection.negative {
            if !range.subset_of(other) {
                return Relation::Inconclusive;
            }
            self.negative
        } else {
            match other.relation(range) {
                // An empty positive intersection satisfies every term.
                SetRelation::Subset => !self.negative || other == &VS::empty(),
                SetRelation::Disjoint => self.negative,
                SetRelation::Overlapping => return Relation::Inconclusive,
            }
        };
        if satisfied {
            Relation::Satisfied
        } else {
            Relation::Contradicted
        }
    }
}

impl<VS: VersionSet> AsRef<Self> for Term<VS> {
    fn as_ref(&self) -> &Self {
        self
    }
}

// REPORT ######################################################################

impl<VS: VersionSet> Display for Term<VS> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.negative {
            write!(f, "Not ( {} )", self.set)
        } else {
            Display::fmt(&self.set, f)
        }
    }
}

// TESTS #######################################################################

#[cfg(test)]
pub mod tests {
    use super::*;
    use proptest::prelude::*;
    use version_ranges::Ranges;

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    struct NoDisjointRanges(Ranges<u32>);

    impl Display for NoDisjointRanges {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }

    impl VersionSet for NoDisjointRanges {
        type V = u32;

        fn empty() -> Self {
            Self(Ranges::empty())
        }

        fn singleton(version: Self::V) -> Self {
            Self(Ranges::singleton(version))
        }

        fn complement(&self) -> Self {
            Self(self.0.complement())
        }

        fn intersection(&self, other: &Self) -> Self {
            Self(self.0.intersection(&other.0))
        }

        fn contains(&self, version: &Self::V) -> bool {
            self.0.contains(version)
        }

        fn is_disjoint(&self, _other: &Self) -> bool {
            panic!("subset-only term relations must not check disjointness")
        }
    }

    pub fn strategy() -> impl Strategy<Value = Term<Ranges<u32>>> {
        prop_oneof![
            version_ranges::proptest_strategy().prop_map(Term::negative),
            version_ranges::proptest_strategy().prop_map(Term::positive),
        ]
    }

    #[test]
    fn empty_positive_intersection_satisfies_negative_term() {
        let term = Term::negative(Ranges::<u32>::singleton(1u32));

        assert!(matches!(
            term.relation_with(&Term::empty()),
            Relation::Satisfied
        ));
    }

    #[test]
    fn subset_only_relations_do_not_check_disjointness() {
        let one = NoDisjointRanges::singleton(1);
        let two = NoDisjointRanges::singleton(2);

        assert!(matches!(
            Term::positive(one.clone()).relation_with(&Term::negative(two.clone())),
            Relation::Inconclusive
        ));
        assert!(matches!(
            Term::negative(one).relation_with(&Term::negative(two)),
            Relation::Inconclusive
        ));
    }

    proptest! {

        // Testing relation --------------------------------

        #[test]
        fn relation_with(term1 in strategy(), term2 in strategy()) {
            match term1.relation_with(&term2) {
                Relation::Satisfied => assert!(term1.satisfied_by(&term2)),
                Relation::Contradicted => assert!(term1.contradicted_by(&term2)),
                Relation::Inconclusive => {
                    assert!(!term1.satisfied_by(&term2));
                    assert!(!term1.contradicted_by(&term2));
                }
            }
        }

        /// Ensure that we don't wrongly convert between positive and negative ranges
        #[test]
        fn positive_negative(term1 in strategy(), term2 in strategy()) {
            let intersection_positive = term1.is_positive() || term2.is_positive();
            let union_positive = term1.is_positive() && term2.is_positive();
            assert_eq!(term1.intersection(&term2).is_positive(), intersection_positive);
            assert_eq!(term1.union(&term2).is_positive(), union_positive);
        }

        #[test]
        fn difference_through_intersection(r1 in strategy(), r2 in strategy()) {
            assert_eq!(r1.difference(&r2), r1.intersection(&r2.negate()));
        }

        #[test]
        fn subset_of_through_intersection(r1 in strategy(), r2 in strategy()) {
            let disjoint_def = r1.intersection(&r2) == r1;
            assert_eq!(r1.subset_of(&r2), disjoint_def);
        }

        #[test]
        fn union_through_intersection(r1 in strategy(), r2 in strategy()) {
            let union_def = r1
                .negate()
                .intersection(&r2.negate())
                .negate();
            assert_eq!(r1.union(&r2), union_def);
        }
    }
}
