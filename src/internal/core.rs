// SPDX-License-Identifier: MPL-2.0

//! Core model and functions
//! to write a functional PubGrub algorithm.

use std::cmp::Reverse;
use std::collections::HashSet as Set;
use std::fmt::{Debug, Display};
use std::hash::{BuildHasher, BuildHasherDefault, Hash};
use std::num::NonZeroU32;
use std::sync::Arc;

use priority_queue::PriorityQueue;
use rustc_hash::FxHasher;

use crate::internal::{
    Arena, HashArena, Id, IncompDpId, IncompId, Incompatibility, Relation, SmallMap, SmallVec,
};
use crate::{DependencyProvider, DerivationTree, Map, NoSolutionError, Package, Term, VersionSet};

type FnvIndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<FxHasher>>;
type FnvIndexSet<T> = indexmap::IndexSet<T, BuildHasherDefault<FxHasher>>;

/// The number of decisions that have been made at some point in solving.
///
/// The logical level is zero-based, but it is stored as `level + 1` in a
/// `NonZeroU32` so that `Option<DecisionLevel>` fits in the same 4 bytes as a
/// bare `u32`, with `None` occupying the all-zero niche.
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct DecisionLevel(NonZeroU32);

impl DecisionLevel {
    /// Decision level zero: no decisions have been made yet.
    pub(crate) const ZERO: DecisionLevel = DecisionLevel(NonZeroU32::MIN);
    /// Sentinel greater than any decision level the solver can reach.
    pub(crate) const MAX: DecisionLevel = DecisionLevel(NonZeroU32::MAX);

    /// Build a `DecisionLevel` from its zero-based logical value.
    pub(crate) fn new(level: u32) -> Self {
        // `level + 1` is always >= 1; the only overflow would be `u32::MAX`
        // decisions, which the solver cannot reach.
        Self(NonZeroU32::new(level + 1).expect("decision level overflow"))
    }

    /// The zero-based logical decision level.
    pub(crate) fn get(self) -> u32 {
        self.0.get() - 1
    }

    /// Advances to the next decision level, panicking if the representation is exhausted.
    pub(crate) fn increment(self) -> Self {
        Self(self.0.checked_add(1).expect("decision level overflow"))
    }
}

impl Debug for DecisionLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DecisionLevel({})", self.get())
    }
}

// Preserve the niche so `Option<DecisionLevel>` stays as small as a bare `u32`.
const _: () = assert!(std::mem::size_of::<Option<DecisionLevel>>() == std::mem::size_of::<u32>());

/// An opaque handle to a conflict recorded by solver state.
#[derive(Copy, Clone, Debug)]
pub struct ConflictId<DP: DependencyProvider>(IncompDpId<DP>);

/// A dependency constraint from one package (dependent) onto another (dependency).
///
/// This may merge overlapping constraints from multiple dependent versions, hence the version
/// range for dependent.
#[derive(Debug)]
pub struct Dependency<'a, P, VS> {
    /// The package declaring the dependency.
    pub dependent: Id<P>,
    /// The versions of the dependent package sharing this dependency.
    pub dependent_versions: &'a VS,
    /// The required package.
    pub dependency: Id<P>,
    /// The required versions, or `None` if the dependency range is empty.
    pub dependency_versions: Option<&'a VS>,
}

#[derive(Clone)]
struct MergedDependencies<P: Package, I> {
    buckets: Map<DependencyKey<P>, SmallVec<I>>,
}

impl<P: Package, I> Default for MergedDependencies<P, I> {
    fn default() -> Self {
        Self {
            buckets: Map::default(),
        }
    }
}

impl<P: Package, I> MergedDependencies<P, I> {
    fn bucket(
        &mut self,
        dependent: Id<P>,
        dependency: Id<P>,
        range: &impl Hash,
    ) -> &mut SmallVec<I> {
        let range_hash = self.buckets.hasher().hash_one(range);
        self.buckets
            .entry(DependencyKey {
                dependent,
                dependency,
                range_hash,
            })
            .or_default()
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct DependencyKey<P: Package> {
    dependent: Id<P>,
    dependency: Id<P>,
    range_hash: u64,
}

/// Current state of the PubGrub algorithm.
#[derive(Clone)]
pub struct State<DP: DependencyProvider> {
    /// The root package and version.
    pub root_package: Id<DP::P>,
    root_version: DP::V,

    /// All incompatibilities indexed by package.
    #[allow(clippy::type_complexity)]
    incompatibilities: Map<Id<DP::P>, Vec<IncompDpId<DP>>>,

    /// All incompatibilities expressing dependencies, with common dependents merged.
    merged_dependencies: MergedDependencies<DP::P, IncompDpId<DP>>,

    next_global_index: u32,
    /// The number of decisions that have been made, equal to the number of packages with decisions.
    current_decision_level: DecisionLevel,
    /// Store for all known package decisions and package derivations.
    ///
    /// "assignment" refers to both packages with decisions and package with only derivations and
    /// no decision yet. We combine this in a single index map with a decisions section in front and
    /// a derivations sections in the back, where making a decision moves a package from the
    /// derivations sections to the decisions section.
    ///
    /// `[..current_decision_level]`: Packages that have had a decision made, sorted by the
    /// `decision_level`. The section is can be seen as the partial solution, it contains a
    /// mapping from package name to decided version. The sorting makes it very efficient to
    /// extract the solution, and to backtrack to a particular decision level. The
    /// `AssignmentsIntersection` is always a `Decision`.
    ///
    /// `[current_decision_level..]`: Packages that are dependencies of some other package,
    /// but have not yet been decided. The `AssignmentsIntersection` is always a `Derivations`, the
    /// derivations store the obligations from the decided packages.
    #[allow(clippy::type_complexity)]
    package_assignments: FnvIndexMap<Id<DP::P>, PackageAssignments<DP::P, DP::VS, DP::M>>,
    /// The undecided packages order by their `Priority`.
    ///
    /// The max heap allows quickly `pop`ing the highest priority package.
    ///
    /// The `Reverse<u32>` is the discovery order of packages used as tiebreaker. Its order is that
    /// of a breadth-first search.
    #[allow(clippy::type_complexity)]
    prioritized_potential_packages:
        PriorityQueue<Id<DP::P>, (DP::Priority, Reverse<u32>), BuildHasherDefault<FxHasher>>,
    /// Packages whose derivations changed since the last time `prioritize` was called and need
    /// their priorities to be updated.
    outdated_priorities: FnvIndexSet<Id<DP::P>>,
    /// For each completed backtrack generation, the highest decision level that remains valid.
    /// The active generation is represented by the missing entry after the end of this vector.
    last_valid_decision_levels: Vec<DecisionLevel>,

    /// The store is the reference storage for all incompatibilities.
    incompatibility_store: Arena<Incompatibility<DP::P, DP::VS, DP::M>>,

    /// The store is the reference storage for all packages.
    pub package_store: HashArena<DP::P>,

    /// This is a stack of work to be done in `unit_propagation`.
    /// It can definitely be a local variable to that method, but
    /// this way we can reuse the same allocation for better performance.
    unit_propagation_buffer: SmallVec<Id<DP::P>>,
}

impl<DP: DependencyProvider> State<DP> {
    /// Initialization of PubGrub state.
    pub fn init(root_package: DP::P, root_version: DP::V) -> Self {
        let mut incompatibility_store = Arena::new();
        let mut package_store = HashArena::new();
        let root_package = package_store.alloc(root_package);
        let not_root_id = incompatibility_store.alloc(Incompatibility::not_root(
            root_package,
            root_version.clone(),
        ));
        let mut incompatibilities = Map::default();
        incompatibilities.insert(root_package, vec![not_root_id]);
        Self {
            root_package,
            root_version,
            incompatibilities,
            next_global_index: 0,
            current_decision_level: DecisionLevel::ZERO,
            package_assignments: FnvIndexMap::default(),
            prioritized_potential_packages: PriorityQueue::default(),
            outdated_priorities: FnvIndexSet::default(),
            last_valid_decision_levels: vec![DecisionLevel::ZERO],
            incompatibility_store,
            package_store,
            unit_propagation_buffer: SmallVec::Empty,
            merged_dependencies: MergedDependencies::default(),
        }
    }

    /// Add the dependencies for the version being decided of the current package as
    /// incompatibilities.
    ///
    /// `versions` is the set of versions that share these dependencies, in the simplest case the
    /// version being decided: `VS::singleton(version)`. A caller that knows which versions exist
    /// can widen the version being decided to a set that contains no other existing version,
    /// e.g. with `Ranges::widen_versions`: If `2` is the only existing version between `1` and
    /// `3`, the dependency incompatibilities for `{2}` can instead be created for `>1, <3`. When
    /// resolution then rejects the version, the version sets derived from these
    /// incompatibilities exclude a contiguous set instead of accumulating one hole per rejected
    /// version, and the incompatibilities of adjacent versions merge into contiguous sets,
    /// keeping the version sets and the operations on them minimal.
    ///
    /// `versions` must contain `version`, every existing version in `versions` must have exactly
    /// the given dependencies, and the set of existing versions must not grow while resolution
    /// is running.
    pub fn add_package_version_dependencies(
        &mut self,
        package: Id<DP::P>,
        version: DP::V,
        versions: DP::VS,
        dependencies: impl IntoIterator<Item = (DP::P, DP::VS)>,
    ) -> Option<ConflictId<DP>> {
        debug_assert!(
            versions.contains(&version),
            "the version being decided must be in the version set sharing its dependencies",
        );
        let dep_incompats =
            self.add_incompatibility_from_dependencies(package, versions, dependencies);
        self.add_package_version_incompatibilities(package, version, dep_incompats)
            .map(ConflictId)
    }

    /// Record that no available version satisfies a version range.
    pub fn add_no_versions(&mut self, package: Id<DP::P>, versions: DP::VS) {
        self.add_incompatibility(Incompatibility::no_versions(package, versions));
    }

    /// Record that a version range is unavailable for a reason outside the solver.
    pub fn add_unavailable(&mut self, package: Id<DP::P>, versions: DP::VS, reason: DP::M) {
        self.add_incompatibility(Incompatibility::custom(package, versions, reason));
    }

    /// Record a dependency constraint without deciding a package version.
    #[cfg(feature = "unstable-state-api")]
    pub fn add_dependency(
        &mut self,
        package: Id<DP::P>,
        versions: DP::VS,
        dependency: (Id<DP::P>, DP::VS),
    ) {
        self.add_incompatibility(Incompatibility::from_dependency(
            package, versions, dependency,
        ));
    }

    /// Iterate over the known incoming and outgoing dependencies of a package.
    ///
    /// Dependent versions with the same dependency range may be merged. Learned
    /// incompatibilities and unavailable-version reasons are excluded.
    #[cfg(feature = "unstable-state-api")]
    pub fn dependencies(
        &self,
        package: Id<DP::P>,
    ) -> impl Iterator<Item = Dependency<'_, DP::P, DP::VS>> {
        self.incompatibilities
            .get(&package)
            .map_or(&[][..], Vec::as_slice)
            .iter()
            .filter_map(|&id| self.incompatibility_store[id].dependency())
    }

    /// Iterate over the packages participating in a conflict.
    pub fn conflict_packages(&self, conflict: &ConflictId<DP>) -> impl Iterator<Item = Id<DP::P>> {
        self.incompatibility_store[conflict.0]
            .iter()
            .map(|(package, _)| package)
    }

    /// Add a newly constructed incompatibility to the state.
    fn add_incompatibility(&mut self, incompat: Incompatibility<DP::P, DP::VS, DP::M>) {
        let id = self.incompatibility_store.alloc(incompat);
        self.merge_incompatibility(id);
    }

    /// Add an incompatibility to the state.
    #[cold]
    fn add_incompatibility_from_dependencies(
        &mut self,
        package: Id<DP::P>,
        versions: DP::VS,
        deps: impl IntoIterator<Item = (DP::P, DP::VS)>,
    ) -> std::ops::Range<IncompDpId<DP>> {
        // Create incompatibilities and allocate them in the store.
        let new_incompats_id_range =
            self.incompatibility_store
                .alloc_iter(deps.into_iter().map(|(dep_p, dep_vs)| {
                    let dep_pid = self.package_store.alloc(dep_p);
                    Incompatibility::from_dependency(package, versions.clone(), (dep_pid, dep_vs))
                }));
        // Merge the newly created incompatibilities with the older ones.
        for id in IncompDpId::<DP>::range_to_iter(new_incompats_id_range.clone()) {
            self.merge_incompatibility(id);
        }
        new_incompats_id_range
    }

    /// Unit propagation is the core mechanism of the solving algorithm.
    /// CF <https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation>
    ///
    /// For each package with a satisfied incompatibility, returns the package and the root cause
    /// conflict.
    #[cold]
    #[allow(clippy::type_complexity)] // Type definitions don't support impl trait.
    pub fn unit_propagation(
        &mut self,
        package: Id<DP::P>,
    ) -> Result<impl IntoIterator<Item = (Id<DP::P>, ConflictId<DP>)> + use<DP>, NoSolutionError<DP>>
    {
        let mut satisfier_causes = SmallVec::default();
        self.unit_propagation_buffer.clear();
        self.unit_propagation_buffer.push(package);
        while let Some(current_package) = self.unit_propagation_buffer.pop() {
            // Iterate over incompatibilities in reverse order
            // to evaluate first the newest incompatibilities.
            let mut conflict_id = None;
            // We only care about incompatibilities if it contains the current package.
            for index in (0..self.incompatibilities[&current_package].len()).rev() {
                let incompat_id = self.incompatibilities[&current_package][index];
                if self.is_contradicted(incompat_id) {
                    continue;
                }
                let current_incompat = &self.incompatibility_store[incompat_id];
                match self.relation(current_incompat) {
                    // If the partial solution satisfies the incompatibility
                    // we must perform conflict resolution.
                    Relation::Satisfied => {
                        log::info!(
                            "Start conflict resolution because incompat satisfied:\n   {}",
                            current_incompat.display(&self.package_store)
                        );
                        conflict_id = Some(incompat_id);
                        break;
                    }
                    Relation::AlmostSatisfied(package_almost) => {
                        // Add `package_almost` to the `unit_propagation_buffer` set.
                        // Putting items in `unit_propagation_buffer` more than once waste cycles,
                        // but so does allocating a hash map and hashing each item.
                        // In practice `unit_propagation_buffer` is small enough that we can just do a linear scan.
                        if !self.unit_propagation_buffer.contains(&package_almost) {
                            self.unit_propagation_buffer.push(package_almost);
                        }
                        // Add (not term) to the partial solution with incompat as cause.
                        self.add_derivation(package_almost, incompat_id);
                        // With the partial solution updated, the incompatibility is now contradicted.
                        self.mark_contradicted(incompat_id);
                    }
                    Relation::Contradicted(_) => {
                        self.mark_contradicted(incompat_id);
                    }
                    _ => {}
                }
            }
            if let Some(incompat_id) = conflict_id {
                let (package_almost, root_cause) = self
                    .conflict_resolution(incompat_id, &mut satisfier_causes)
                    .map_err(|terminal_incompat_id| {
                        self.build_derivation_tree(terminal_incompat_id)
                    })?;
                self.unit_propagation_buffer.clear();
                self.unit_propagation_buffer.push(package_almost);
                // Add to the partial solution with incompat as cause.
                self.add_derivation(package_almost, root_cause);
                // After conflict resolution and the partial solution update,
                // the root cause incompatibility is now contradicted.
                self.mark_contradicted(root_cause);
            }
        }
        // If there are no more changed packages, unit propagation is done.
        Ok(satisfier_causes
            .into_iter()
            .map(|(package, id)| (package, ConflictId(id))))
    }

    /// Return the root cause or the terminal incompatibility. CF
    /// <https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation>
    ///
    /// When we found a conflict, we want to learn as much as possible from it, to avoid making (or
    /// keeping) decisions that will be rejected. Say we found that the dependency requirements on X and the
    /// dependency requirements on Y are incompatible. We may find that the decisions on earlier packages B and C
    /// require us to make incompatible requirements on X and Y, so we backtrack until either B or C
    /// can be revisited. To make it practical, we really only need one of the terms to be a
    /// decision. We may as well leave the other terms general. Something like "the dependency on
    /// the package X is incompatible with the decision on C" tends to work out pretty well. Then if
    /// A turns out to also have a dependency on X the resulting root cause is still useful.
    /// (`unit_propagation` will ensure we don't try that version of C.)
    /// Of course, this is more heuristics than science. If the output is too general, then
    /// `unit_propagation` will handle the confusion by calling us again with the next most specific
    /// conflict it comes across. If the output is too specific, then the outer `solver` loop will
    /// eventually end up calling us again until all possibilities are enumerated.
    ///
    /// To end up with a more useful incompatibility, this function combines incompatibilities into
    /// derivations. Fulfilling this derivation implies the later conflict. By banning it, we
    /// prevent the intermediate steps from occurring again, at least in the exact same way.
    /// However, the statistics collected for `prioritize` may want to analyze those intermediate
    /// steps. For example we might start with "there is no version 1 of Z", and
    /// `conflict_resolution` may be able to determine that "that was inevitable when we picked
    /// version 1 of X" which was inevitable when we picked W and so on, until version 1 of B, which
    /// was depended on by version 1 of A. Therefore the root cause may simplify all the way down to
    /// "we cannot pick version 1 of A". This will prevent us going down this path again. However
    /// when we start looking at version 2 of A, and discover that it depends on version 2 of B, we
    /// will want to prioritize the chain of intermediate steps to check if it has a problem with
    /// the same shape. The `satisfier_causes` argument keeps track of these intermediate steps so
    /// that the caller can use them for prioritization.
    #[allow(clippy::type_complexity)]
    #[cold]
    fn conflict_resolution(
        &mut self,
        incompatibility: IncompDpId<DP>,
        satisfier_causes: &mut SmallVec<(Id<DP::P>, IncompDpId<DP>)>,
    ) -> Result<(Id<DP::P>, IncompDpId<DP>), IncompDpId<DP>> {
        let mut current_incompat_id = incompatibility;
        let mut current_incompat_changed = false;
        loop {
            if self.incompatibility_store[current_incompat_id]
                .is_terminal(self.root_package, &self.root_version)
            {
                return Err(current_incompat_id);
            } else {
                let (package, satisfier_search_result) =
                    self.satisfier_search(&self.incompatibility_store[current_incompat_id]);
                match satisfier_search_result {
                    SatisfierSearch::DifferentDecisionLevels {
                        previous_satisfier_level,
                    } => {
                        self.backtrack(previous_satisfier_level);
                        if current_incompat_changed {
                            self.merge_incompatibility(current_incompat_id);
                        }
                        log::info!("backtrack to {previous_satisfier_level:?}");
                        satisfier_causes.push((package, current_incompat_id));
                        return Ok((package, current_incompat_id));
                    }
                    SatisfierSearch::SameDecisionLevels { satisfier_cause } => {
                        let prior_cause = Incompatibility::prior_cause(
                            current_incompat_id,
                            satisfier_cause,
                            package,
                            &self.incompatibility_store,
                        );
                        log::info!("prior cause: {}", prior_cause.display(&self.package_store));
                        current_incompat_id = self.incompatibility_store.alloc(prior_cause);
                        satisfier_causes.push((package, current_incompat_id));
                        current_incompat_changed = true;
                    }
                }
            }
        }
    }

    /// Manually backtrack before the given package was selected.
    ///
    /// This can be used to switch the order of packages if the previous prioritization was bad.
    ///
    /// Returns the number of the decisions that were backtracked, or `None` if the package was not
    /// decided on yet.
    #[cfg(feature = "unstable-state-api")]
    pub fn backtrack_package(&mut self, package: Id<DP::P>) -> Option<u32> {
        let base_decision_level = self.current_decision_level;
        let decision_level = self.package_assignments.get_index_of(&package)?;
        let decision_level = DecisionLevel::new(decision_level as u32);
        // Decisions occupy indices strictly below the current decision level.
        if decision_level >= self.current_decision_level {
            return None;
        }
        log::debug!(
            "Package backtracking to decision level {}",
            decision_level.get()
        );
        self.backtrack(decision_level);
        Some(base_decision_level.get() - decision_level.get())
    }

    /// Add this incompatibility into the set of all incompatibilities.
    ///
    /// PubGrub collapses identical dependencies from adjacent package versions
    /// into individual incompatibilities.
    /// This substantially reduces the total number of incompatibilities
    /// and makes it much easier for PubGrub to reason about multiple versions of packages at once.
    ///
    /// For example, rather than representing
    /// foo 1.0.0 depends on bar ^1.0.0 and
    /// foo 1.1.0 depends on bar ^1.0.0
    /// as two separate incompatibilities,
    /// they are collapsed together into the single incompatibility {foo ^1.0.0, not bar ^1.0.0}
    /// (provided that no other version of foo exists between 1.0.0 and 2.0.0).
    /// We could collapse them into { foo (1.0.0 ∪ 1.1.0), not bar ^1.0.0 }
    /// without having to check the existence of other versions though.
    fn merge_incompatibility(&mut self, mut id: IncompDpId<DP>) {
        if let Some((p1, p2)) = self.incompatibility_store[id].as_dependency() {
            // Self-dependencies cannot be merged.
            if p1 != p2 {
                let deps_lookup = self.merged_dependencies.bucket(
                    p1,
                    p2,
                    &self.incompatibility_store[id]
                        .get(p2)
                        .map(|term| term.unwrap_negative()),
                );
                if let Some((past, merged)) =
                    deps_lookup.as_mut_slice().iter_mut().find_map(|past| {
                        self.incompatibility_store[id]
                            .merge_dependents(&self.incompatibility_store[*past])
                            .map(|merged| (past, merged))
                    })
                {
                    let new = self.incompatibility_store.alloc(merged);
                    for (pkg, _) in self.incompatibility_store[new].iter() {
                        self.incompatibilities
                            .entry(pkg)
                            .or_default()
                            .retain(|id| id != past);
                    }
                    *past = new;
                    id = new;
                } else {
                    deps_lookup.push(id);
                }
            }
        }
        for (pkg, term) in self.incompatibility_store[id].iter() {
            if cfg!(debug_assertions) {
                assert_ne!(term, &crate::term::Term::any());
            }
            self.incompatibilities.entry(pkg).or_default().push(id);
        }
    }

    // Error reporting #########################################################

    fn build_derivation_tree(
        &self,
        incompat: IncompDpId<DP>,
    ) -> DerivationTree<DP::P, DP::VS, DP::M> {
        let mut all_ids: Set<IncompDpId<DP>> = Set::default();
        let mut shared_ids = Set::default();
        let mut stack = vec![incompat];
        while let Some(i) = stack.pop() {
            if let Some((id1, id2)) = self.incompatibility_store[i].causes() {
                if all_ids.contains(&i) {
                    shared_ids.insert(i);
                } else {
                    stack.push(id1);
                    stack.push(id2);
                }
            }
            all_ids.insert(i);
        }
        // To avoid recursion we need to generate trees in topological order.
        // That is to say we need to ensure that the causes are processed before the incompatibility they effect.
        // It happens to be that sorting by their ID maintains this property.
        let mut sorted_ids = all_ids.into_iter().collect::<Vec<_>>();
        sorted_ids.sort_unstable_by_key(|id| id.into_raw());
        let mut precomputed = Map::default();
        for id in sorted_ids {
            let tree = Incompatibility::build_derivation_tree(
                id,
                &shared_ids,
                &self.incompatibility_store,
                &self.package_store,
                &precomputed,
            );
            precomputed.insert(id, Arc::new(tree));
        }
        // Now the user can refer to the entire tree from its root.
        Arc::into_inner(precomputed.remove(&incompat).unwrap()).unwrap()
    }

    fn is_contradicted(&self, incompatibility: IncompDpId<DP>) -> bool {
        self.incompatibility_store[incompatibility]
            .is_contradicted(&self.last_valid_decision_levels)
    }

    fn mark_contradicted(&mut self, incompatibility: IncompDpId<DP>) {
        self.incompatibility_store[incompatibility].mark_contradicted(
            self.current_decision_level,
            self.last_valid_decision_levels.len() as u32,
        );
    }

    /// Returns whether at least one backtrack generation has completed.
    pub(crate) fn has_backtracked(&self) -> bool {
        self.last_valid_decision_levels.len() > 1
    }

    pub(crate) fn display_partial_solution(&self) -> impl Display + '_ {
        struct PartialSolutionDisplay<'a, DP: DependencyProvider>(&'a State<DP>);

        impl<DP: DependencyProvider> Display for PartialSolutionDisplay<'_, DP> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut assignments: Vec<_> = self
                    .0
                    .package_assignments
                    .iter()
                    .map(|(p, pa)| format!("{:?} = '{}': {}", p, self.0.package_store[*p], pa))
                    .collect();
                assignments.sort();
                write!(
                    f,
                    "next_global_index: {}\ncurrent_decision_level: {:?}\npackage_assignments:\n{}",
                    self.0.next_global_index,
                    self.0.current_decision_level,
                    assignments.join("\t\n")
                )
            }
        }

        PartialSolutionDisplay(self)
    }

    /// Add a decision.
    pub fn add_decision(&mut self, package: Id<DP::P>, version: DP::V) {
        // Check that add_decision is never used in the wrong context.
        if cfg!(debug_assertions) {
            match self.package_assignments.get_mut(&package) {
                None => panic!("Derivations must already exist"),
                Some(pa) => match &pa.assignments_intersection {
                    // Cannot be called when a decision has already been taken.
                    AssignmentsIntersection::Decision { .. } => {
                        panic!("Already existing decision")
                    }
                    // Cannot be called if the versions is not contained in the terms' intersection.
                    AssignmentsIntersection::Derivations(term) => {
                        debug_assert!(
                            term.contains(&version),
                            "{package:?}: {version} was expected to be contained in {term}",
                        )
                    }
                },
            }
        }
        let new_idx = self.current_decision_level.get() as usize;
        self.current_decision_level = self.current_decision_level.increment();
        let (old_idx, _, pa) = self
            .package_assignments
            .get_full_mut(&package)
            .expect("Derivations must already exist");
        pa.highest_decision_level = self.current_decision_level;
        pa.assignments_intersection = AssignmentsIntersection::Decision {
            decision_level: self.next_global_index,
            version: version.clone(),
            term: Term::exact(version),
        };
        // Maintain that the beginning of the `package_assignments` Have all decisions in sorted order.
        if new_idx != old_idx {
            self.package_assignments.swap_indices(new_idx, old_idx);
        }
        self.next_global_index += 1;
    }

    /// The list of package that have not been selected after the last prioritization.
    ///
    /// This list gets updated by [`Self::pick_highest_priority_pkg`] and cleared by backtracking.
    #[cfg(feature = "unstable-state-api")]
    #[allow(clippy::type_complexity)]
    pub fn undecided_packages(
        &self,
    ) -> impl Iterator<
        Item = (
            &Id<DP::P>,
            &(<DP as DependencyProvider>::Priority, Reverse<u32>),
        ),
    > {
        self.prioritized_potential_packages.iter()
    }

    /// Add a derivation.
    fn add_derivation(&mut self, package: Id<DP::P>, cause: IncompDpId<DP>) {
        use indexmap::map::Entry;
        let mut dated_derivation = DatedDerivation {
            global_index: self.next_global_index,
            decision_level: self.current_decision_level,
            cause,
            accumulated_intersection: self.incompatibility_store[cause]
                .get(package)
                .unwrap()
                .negate(),
        };
        self.next_global_index += 1;
        match self.package_assignments.entry(package) {
            Entry::Occupied(mut occupied) => {
                let pa = occupied.get_mut();
                pa.highest_decision_level = self.current_decision_level;
                match &mut pa.assignments_intersection {
                    // Check that add_derivation is never called in the wrong context.
                    AssignmentsIntersection::Decision { .. } => {
                        panic!("add_derivation should not be called after a decision")
                    }
                    AssignmentsIntersection::Derivations(t) => {
                        *t = t.intersection(&dated_derivation.accumulated_intersection);
                        dated_derivation.accumulated_intersection = t.clone();
                        if t.is_positive() {
                            self.outdated_priorities.insert(package);
                        }
                    }
                }
                pa.dated_derivations.push(dated_derivation);
            }
            Entry::Vacant(v) => {
                let term = dated_derivation.accumulated_intersection.clone();
                if term.is_positive() {
                    self.outdated_priorities.insert(package);
                }
                v.insert(PackageAssignments {
                    smallest_decision_level: self.current_decision_level,
                    highest_decision_level: self.current_decision_level,
                    dated_derivations: SmallVec::One([dated_derivation]),
                    assignments_intersection: AssignmentsIntersection::Derivations(term),
                });
            }
        }
    }

    /// Iterate over undecided packages with positive terms updated at the current decision level.
    #[cfg(feature = "unstable-state-api")]
    #[cold]
    pub fn prioritized_packages(&self) -> impl Iterator<Item = (Id<DP::P>, &DP::VS)> {
        // TODO(konsti): Should we use `self.outdated_priorities` instead?
        let current_decision_level = self.current_decision_level;
        self.package_assignments
            .get_range(self.current_decision_level.get() as usize..)
            .unwrap()
            .iter()
            .filter(move |(_, pa)| pa.highest_decision_level == current_decision_level)
            .filter_map(|(&p, pa)| {
                Some((p, pa.assignments_intersection.potential_package_filter()?))
            })
    }

    /// Update pending priorities and select the highest priority undecided package.
    ///
    /// The callback receives the package ID, name, and compatible version set. The selected
    /// package always has a positive term available through [`Self::term_intersection_for_package`].
    #[cold]
    pub fn pick_highest_priority_pkg(
        &mut self,
        mut prioritizer: impl FnMut(Id<DP::P>, &DP::P, &DP::VS) -> DP::Priority,
    ) -> Option<Id<DP::P>> {
        let prioritized_potential_packages = &mut self.prioritized_potential_packages;
        while let Some(p) = self.outdated_priorities.pop() {
            let Some(pa) = self.package_assignments.get(&p) else {
                continue;
            };
            let Some(r) = pa.assignments_intersection.potential_package_filter() else {
                continue;
            };
            let priority = prioritizer(p, &self.package_store[p], r);
            prioritized_potential_packages.push(p, (priority, Reverse(p.into_raw() as u32)));
        }
        while let Some(p) = self.prioritized_potential_packages.pop().map(|(p, _)| p) {
            let Some(pa) = self.package_assignments.get(&p) else {
                continue;
            };
            if pa
                .assignments_intersection
                .potential_package_filter()
                .is_some()
            {
                return Some(p);
            }
        }
        None
    }

    /// If a partial solution has, for every positive derivation,
    /// a corresponding decision that satisfies that assignment,
    /// it's a total solution and version solving has succeeded.
    pub fn extract_solution(&self) -> impl Iterator<Item = (Id<DP::P>, DP::V)> + '_ {
        self.package_assignments
            .iter()
            .take(self.current_decision_level.get() as usize)
            .map(|(&p, pa)| match &pa.assignments_intersection {
                AssignmentsIntersection::Decision {
                    decision_level: _,
                    version: v,
                    term: _,
                } => (p, v.clone()),
                AssignmentsIntersection::Derivations(_) => {
                    // The invariant on the order in `self.package_assignments` was broken.
                    let mut context = String::new();
                    for (id, assignment) in self
                        .package_assignments
                        .iter()
                        .take(self.current_decision_level.get() as usize)
                    {
                        context.push_str(&format!(
                            " * {:?} {:?}\n",
                            id, assignment.assignments_intersection
                        ));
                    }
                    panic!(
                        "Derivations in the Decision part. Decision level {}\n{}",
                        self.current_decision_level.get(),
                        context
                    )
                }
            })
    }

    /// Backtrack the partial solution to a given decision level.
    fn backtrack(&mut self, decision_level: DecisionLevel) {
        self.current_decision_level = decision_level;
        self.package_assignments.retain(|p, pa| {
            if pa.smallest_decision_level > decision_level {
                // Remove all entries that have a smallest decision level higher than the backtrack target.
                false
            } else if pa.highest_decision_level <= decision_level {
                // Do not change entries older than the backtrack decision level target.
                if pa
                    .assignments_intersection
                    .potential_package_filter()
                    .is_some()
                    && self.prioritized_potential_packages.get(p).is_none()
                {
                    self.outdated_priorities.insert(*p);
                }
                true
            } else {
                // smallest_decision_level <= decision_level < highest_decision_level
                //
                // Since decision_level < highest_decision_level,
                // We can be certain that there will be no decision in this package assignments
                // after backtracking, because such decision would have been the last
                // assignment and it would have the "highest_decision_level".

                // Truncate the history.
                while pa.dated_derivations.last().map(|dd| dd.decision_level) > Some(decision_level)
                {
                    pa.dated_derivations.pop();
                }
                debug_assert!(!pa.dated_derivations.is_empty());

                let last = pa.dated_derivations.last().unwrap();

                // Update highest_decision_level.
                pa.highest_decision_level = last.decision_level;

                // Reset the assignments intersection.
                pa.assignments_intersection =
                    AssignmentsIntersection::Derivations(last.accumulated_intersection.clone());

                self.prioritized_potential_packages.remove(p);
                if pa.assignments_intersection.term().is_positive() {
                    self.outdated_priorities.insert(*p);
                }
                true
            }
        });

        // Close the active generation, then lower every generation invalidated by this backtrack
        // to the new highest valid decision level.
        self.last_valid_decision_levels.push(DecisionLevel::MAX);
        let index = self
            .last_valid_decision_levels
            .partition_point(|&level| level <= decision_level);
        self.last_valid_decision_levels[index..].fill(decision_level);
    }

    /// Add a package version as decision if none of its dependencies conflicts with the partial
    /// solution.
    ///
    /// If the resolution never backtracked before, a fast path adds the package version directly
    /// without checking dependencies.
    ///
    /// Returns the incompatibility that caused the current version to be rejected, if a conflict
    /// in the dependencies was found.
    fn add_package_version_incompatibilities(
        &mut self,
        package: Id<DP::P>,
        version: DP::V,
        new_incompatibilities: std::ops::Range<IncompId<DP::P, DP::VS, DP::M>>,
    ) -> Option<IncompId<DP::P, DP::VS, DP::M>> {
        if !self.has_backtracked() {
            // Fast path: Nothing has yet gone wrong during this resolution. This call is unlikely to be the first problem.
            // So let's live with a little bit of risk and add the decision without checking the dependencies.
            // The worst that can happen is we will have to do a full backtrack which only removes this one decision.
            log::info!("add_decision: {package:?} @ {version} without checking dependencies");
            self.add_decision(package, version);
            return None;
        }

        // Check if any of the dependencies preclude deciding on this crate version.
        let package_term = Term::exact(version.clone());
        let relation = |incompat: IncompId<DP::P, DP::VS, DP::M>| {
            self.incompatibility_store[incompat].relation(|p| {
                // The current package isn't part of the package assignments yet.
                if p == package {
                    Some(&package_term)
                } else {
                    self.term_intersection_for_package(p)
                }
            })
        };
        if let Some(satisfied) = Id::range_to_iter(new_incompatibilities)
            .find(|incompat| relation(*incompat) == Relation::Satisfied)
        {
            log::info!(
                "rejecting decision {package:?} @ {version} because its dependencies conflict"
            );
            Some(satisfied)
        } else {
            log::info!("adding decision: {package:?} @ {version}");
            self.add_decision(package, version);
            None
        }
    }

    /// Check if the terms in the partial solution satisfy the incompatibility.
    fn relation(&self, incompat: &Incompatibility<DP::P, DP::VS, DP::M>) -> Relation<DP::P> {
        incompat.relation(|package| self.term_intersection_for_package(package))
    }

    /// Retrieve intersection of terms related to package.
    pub fn term_intersection_for_package(&self, package: Id<DP::P>) -> Option<&Term<DP::VS>> {
        self.package_assignments
            .get(&package)
            .map(|pa| pa.assignments_intersection.term())
    }

    /// Figure out if the satisfier and previous satisfier are of different decision levels.
    #[allow(clippy::type_complexity)]
    fn satisfier_search(
        &self,
        incompat: &Incompatibility<DP::P, DP::VS, DP::M>,
    ) -> (Id<DP::P>, SatisfierSearch<DP::P, DP::VS, DP::M>) {
        let satisfied_map = Self::find_satisfier(incompat, &self.package_assignments);
        let (&satisfier_package, &(satisfier_cause, _, satisfier_decision_level)) = satisfied_map
            .iter()
            .max_by_key(|(_p, (_, global_index, _))| global_index)
            .unwrap();
        let previous_satisfier_level = Self::find_previous_satisfier(
            incompat,
            satisfier_package,
            satisfied_map,
            &self.package_assignments,
            &self.incompatibility_store,
        );
        let search_result = if previous_satisfier_level >= satisfier_decision_level {
            SatisfierSearch::SameDecisionLevels {
                satisfier_cause: satisfier_cause.unwrap(),
            }
        } else {
            SatisfierSearch::DifferentDecisionLevels {
                previous_satisfier_level,
            }
        };
        (satisfier_package, search_result)
    }

    /// A satisfier is the earliest assignment in partial solution such that the incompatibility
    /// is satisfied by the partial solution up to and including that assignment.
    ///
    /// Returns a map indicating for each package term, when that was first satisfied in history.
    /// If we effectively found a satisfier, the returned map must be the same size that incompat.
    ///
    /// Question: This is possible since we added a "global_index" to every dated_derivation.
    /// It would be nice if we could get rid of it, but I don't know if then it will be possible
    /// to return a coherent previous_satisfier_level.
    #[allow(clippy::type_complexity)]
    fn find_satisfier(
        incompat: &Incompatibility<DP::P, DP::VS, DP::M>,
        package_assignments: &FnvIndexMap<Id<DP::P>, PackageAssignments<DP::P, DP::VS, DP::M>>,
    ) -> SatisfiedMap<DP::P, DP::VS, DP::M> {
        let mut satisfied = SmallMap::Empty;
        for (package, incompat_term) in incompat.iter() {
            let pa = package_assignments.get(&package).expect("Must exist");
            satisfied.insert(package, pa.satisfier(package, &incompat_term.negate()));
        }
        satisfied
    }

    /// Earliest assignment in the partial solution before satisfier
    /// such that incompatibility is satisfied by the partial solution up to
    /// and including that assignment plus satisfier.
    #[allow(clippy::type_complexity)]
    fn find_previous_satisfier(
        incompat: &Incompatibility<DP::P, DP::VS, DP::M>,
        satisfier_package: Id<DP::P>,
        mut satisfied_map: SatisfiedMap<DP::P, DP::VS, DP::M>,
        package_assignments: &FnvIndexMap<Id<DP::P>, PackageAssignments<DP::P, DP::VS, DP::M>>,
        store: &Arena<Incompatibility<DP::P, DP::VS, DP::M>>,
    ) -> DecisionLevel {
        // First, let's retrieve the previous derivations and the initial accum_term.
        let satisfier_pa = package_assignments.get(&satisfier_package).unwrap();
        let (satisfier_cause, _gidx, _dl) = satisfied_map.get(&satisfier_package).unwrap();

        let accum_term = if let &Some(cause) = satisfier_cause {
            store[cause].get(satisfier_package).unwrap().negate()
        } else {
            match &satisfier_pa.assignments_intersection {
                AssignmentsIntersection::Derivations(_) => panic!("must be a decision"),
                AssignmentsIntersection::Decision {
                    decision_level: _,
                    version: _,
                    term,
                } => term.clone(),
            }
        };

        let incompat_term = incompat
            .get(satisfier_package)
            .expect("satisfier package not in incompat");

        satisfied_map.insert(
            satisfier_package,
            satisfier_pa.satisfier(
                satisfier_package,
                &accum_term.intersection(&incompat_term.negate()),
            ),
        );

        // Finally, let's identify the decision level of that previous satisfier.
        let (_, &(_, _, decision_level)) = satisfied_map
            .iter()
            .max_by_key(|(_p, (_, global_index, _))| global_index)
            .unwrap();
        decision_level.max(DecisionLevel::new(1))
    }

    /// Retrieve the constraints on a package that will not change.
    #[cfg(feature = "unstable-state-api")]
    pub fn unchanging_term_for_package(&self, package: Id<DP::P>) -> Option<&Term<DP::VS>> {
        let pa = self.package_assignments.get(&package)?;

        let idx_newer = pa
            .dated_derivations
            .as_slice()
            .partition_point(|dd| dd.decision_level <= DecisionLevel::new(1));
        let idx = idx_newer.checked_sub(1)?;
        Some(&pa.dated_derivations[idx].accumulated_intersection)
    }
}

/// A package assignment is either a decision or a list of (accumulated) derivations without a
/// decision.
#[derive(Clone, Debug)]
struct PackageAssignments<P: Package, VS: VersionSet, M: Eq + Clone + Debug + Display> {
    /// Whether the assigment is a decision or a derivation.
    assignments_intersection: AssignmentsIntersection<VS>,
    /// All constraints on the package version from previous decisions, accumulated by decision
    /// level.
    dated_derivations: SmallVec<DatedDerivation<P, VS, M>>,
    /// Smallest [`DecisionLevel`] in `dated_derivations`.
    smallest_decision_level: DecisionLevel,
    /// Highest [`DecisionLevel`] in `dated_derivations`.
    highest_decision_level: DecisionLevel,
}

impl<P: Package, VS: VersionSet, M: Eq + Clone + Debug + Display> Display
    for PackageAssignments<P, VS, M>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let derivations: Vec<_> = self
            .dated_derivations
            .iter()
            .map(|dd| dd.to_string())
            .collect();
        write!(
            f,
            "decision range: {:?}..{:?}\nderivations:\n  {}\n,assignments_intersection: {}",
            self.smallest_decision_level,
            self.highest_decision_level,
            derivations.join("\n  "),
            self.assignments_intersection
        )
    }
}

#[derive(Clone, Debug)]
struct DatedDerivation<P: Package, VS: VersionSet, M: Eq + Clone + Debug + Display> {
    global_index: u32,
    /// Only decisions up this level has been used to compute the accumulated term.
    decision_level: DecisionLevel,
    cause: IncompId<P, VS, M>,
    /// The intersection of all terms up to `decision_level`.
    ///
    /// It may not contain all terms of this `decision_level`, there may be more than one
    /// `DatedDerivation` per decision level.
    accumulated_intersection: Term<VS>,
}

impl<P: Package, VS: VersionSet, M: Eq + Clone + Debug + Display> Display
    for DatedDerivation<P, VS, M>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}, cause: {:?}", self.decision_level, self.cause)
    }
}

#[derive(Clone, Debug)]
enum AssignmentsIntersection<VS: VersionSet> {
    /// A decision on package for version has been made at the given level.
    Decision {
        decision_level: u32,
        version: VS::V,
        /// The version, but as positive singleton term.
        term: Term<VS>,
    },
    Derivations(Term<VS>),
}

impl<VS: VersionSet> Display for AssignmentsIntersection<VS> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Decision {
                decision_level,
                version,
                term: _,
            } => {
                write!(f, "Decision: level {decision_level}, v = {version}")
            }
            Self::Derivations(term) => write!(f, "Derivations term: {term}"),
        }
    }
}

#[derive(Clone, Debug)]
enum SatisfierSearch<P: Package, VS: VersionSet, M: Eq + Clone + Debug + Display> {
    DifferentDecisionLevels {
        previous_satisfier_level: DecisionLevel,
    },
    SameDecisionLevels {
        satisfier_cause: IncompId<P, VS, M>,
    },
}

type SatisfiedMap<P, VS, M> = SmallMap<Id<P>, (Option<IncompId<P, VS, M>>, u32, DecisionLevel)>;

impl<P: Package, VS: VersionSet, M: Eq + Clone + Debug + Display> PackageAssignments<P, VS, M> {
    fn satisfier(
        &self,
        package: Id<P>,
        start_term: &Term<VS>,
    ) -> (Option<IncompId<P, VS, M>>, u32, DecisionLevel) {
        let empty = Term::empty();
        // Indicate if we found a satisfier in the list of derivations, otherwise it will be the decision.
        let idx = self
            .dated_derivations
            .as_slice()
            .partition_point(|dd| !dd.accumulated_intersection.is_disjoint(start_term));
        if let Some(dd) = self.dated_derivations.get(idx) {
            debug_assert_eq!(dd.accumulated_intersection.intersection(start_term), empty);
            return (Some(dd.cause), dd.global_index, dd.decision_level);
        }
        // If it wasn't found in the derivations,
        // it must be the decision which is last (if called in the right context).
        match &self.assignments_intersection {
            AssignmentsIntersection::Decision {
                decision_level: global_index,
                version: _,
                term: _,
            } => (None, *global_index, self.highest_decision_level),
            AssignmentsIntersection::Derivations(accumulated_intersection) => {
                unreachable!(
                    concat!(
                        "while processing package {:?}: ",
                        "accum_term = {} has overlap with incompat_term = {}, ",
                        "which means the last assignment should have been a decision, ",
                        "but instead it was a derivation. This shouldn't be possible! ",
                        "(Maybe your Version ordering is broken?)"
                    ),
                    package, accumulated_intersection, start_term
                )
            }
        }
    }
}

impl<VS: VersionSet> AssignmentsIntersection<VS> {
    /// Returns the term intersection of all assignments (decision included).
    fn term(&self) -> &Term<VS> {
        match self {
            Self::Decision {
                decision_level: _,
                version: _,
                term,
            } => term,
            Self::Derivations(term) => term,
        }
    }

    /// A package is a potential pick if there isn't an already
    /// selected version (no "decision")
    /// and if it contains at least one positive derivation term
    /// in the partial solution.
    fn potential_package_filter(&self) -> Option<&VS> {
        match self {
            Self::Decision { .. } => None,
            Self::Derivations(term_intersection) => {
                if term_intersection.is_positive() {
                    Some(term_intersection.unwrap_positive())
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod dependency_merge_tests {
    use std::fmt::{self, Display};
    use std::hash::{Hash, Hasher};

    use crate::{OfflineDependencyProvider, Ranges, VersionSet};

    use super::State;

    #[test]
    fn merge_dependencies_with_hash_collisions() {
        let mut state: State<OfflineDependencyProvider<&str, CollidingRanges>> =
            State::init("root", 0);
        let package = state.package_store.alloc("package");

        // Alternate two pairs of constraints so equal ranges recur non-adjacently, while every
        // range shares the same hash.
        for version in 0..10 {
            let first = (version % 2) * 2;
            state.add_incompatibility_from_dependencies(
                package,
                CollidingRanges::singleton(version),
                [
                    ("dependency", CollidingRanges::singleton(first)),
                    ("dependency", CollidingRanges::singleton(first + 1)),
                ],
            );
        }

        let dependency = state.package_store.alloc("dependency");
        assert_eq!(state.incompatibilities[&package].len(), 4);
        assert_eq!(state.incompatibilities[&dependency].len(), 4);
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct CollidingRanges(Ranges<u32>);

    impl Display for CollidingRanges {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Display::fmt(&self.0, f)
        }
    }

    impl Hash for CollidingRanges {
        fn hash<H: Hasher>(&self, state: &mut H) {
            0u8.hash(state);
        }
    }

    impl VersionSet for CollidingRanges {
        type V = u32;

        fn empty() -> Self {
            Self(Ranges::empty())
        }

        fn singleton(v: Self::V) -> Self {
            Self(Ranges::singleton(v))
        }

        fn complement(&self) -> Self {
            Self(self.0.complement())
        }

        fn intersection(&self, other: &Self) -> Self {
            Self(self.0.intersection(&other.0))
        }

        fn contains(&self, v: &Self::V) -> bool {
            self.0.contains(v)
        }
    }
}

#[cfg(test)]
mod tests {
    //! Resolution with dependent versions widened over the known versions must behave like
    //! resolution with singleton dependent versions.

    use std::collections::BTreeMap;
    use std::collections::BTreeSet;

    use crate::internal::{Id, State};
    use crate::{
        Dependencies, DependencyProvider, Map, OfflineDependencyProvider,
        PackageResolutionStatistics, Ranges,
    };

    type NumVS = Ranges<u32>;
    type Provider = OfflineDependencyProvider<&'static str, NumVS>;

    /// A `resolve` loop that keeps the version sets minimal by widening the version whose
    /// dependencies are added over the known versions.
    ///
    /// Returns `None` if there is no solution.
    fn resolve_with_widening(
        provider: &Provider,
        root: &'static str,
        version: u32,
        widen: bool,
    ) -> Option<BTreeMap<&'static str, u32>> {
        // Sorted version lists per package, the input for the widening.
        let versions: Map<&'static str, Vec<u32>> = provider
            .packages()
            .map(|p| (*p, provider.versions(p).unwrap().copied().collect()))
            .collect();
        let mut widened = false;

        let mut state: State<Provider> = State::init(root, version);
        let mut added_dependencies: Map<Id<&'static str>, BTreeSet<u32>> = Map::default();
        let mut next = state.root_package;
        let solution = loop {
            state.unit_propagation(next).ok()?;

            let Some(package) = state.pick_highest_priority_pkg(|_, package, r| {
                provider.prioritize(package, r, &PackageResolutionStatistics::default())
            }) else {
                break state
                    .extract_solution()
                    .map(|(p, v)| (state.package_store[p], v))
                    .collect();
            };
            next = package;
            let term_intersection = state
                .term_intersection_for_package(package)
                .expect("selected package must have a positive term")
                .unwrap_positive();

            let Some(decision) = provider
                .choose_version(&state.package_store[package], term_intersection)
                .unwrap()
            else {
                let versions = term_intersection.clone();
                state.add_no_versions(package, versions);
                continue;
            };

            if added_dependencies
                .entry(package)
                .or_default()
                .insert(decision)
            {
                let dependencies = match provider
                    .get_dependencies(&state.package_store[package], &decision)
                    .unwrap()
                {
                    Dependencies::Unavailable(reason) => {
                        state.add_unavailable(package, NumVS::singleton(decision), reason);
                        continue;
                    }
                    Dependencies::Available(dependencies) => dependencies,
                };
                let version_set = NumVS::singleton(decision);
                let versions = if widen {
                    let widened_versions = version_set
                        .widen_versions(versions.get(state.package_store[package]).unwrap());
                    widened |= widened_versions != version_set;
                    widened_versions
                } else {
                    version_set
                };
                state.add_package_version_dependencies(package, decision, versions, dependencies);
            } else {
                state.add_decision(package, decision);
            }
        };
        if widen {
            assert!(widened, "no version set was ever widened");
        }
        Some(solution)
    }

    /// A registry that requires rejecting versions of "a" and "b" one by one, similar to boto3 and
    /// botocore: Without widening, the version sets of "a" accumulate one hole per rejected version,
    /// and the incompatibilities of "a" versions depending on "j" merge into growing unions of
    /// singletons.
    fn backtracking_provider(conflict_below: u32) -> Provider {
        let mut provider = Provider::new();
        provider.add_dependencies(
            "root",
            1u32,
            [
                ("u", Ranges::strictly_lower_than(2u32)),
                ("a", Ranges::full()),
            ],
        );
        provider.add_dependencies("u", 1u32, []);
        provider.add_dependencies("u", 2u32, []);
        for k in 1..=30u32 {
            provider.add_dependencies("a", k, [("b", Ranges::singleton(k)), ("j", Ranges::full())]);
            // Versions of "b" above the threshold conflict with the "u" requirement of root.
            let u_range = if k >= conflict_below {
                Ranges::higher_than(2u32)
            } else {
                Ranges::strictly_lower_than(2u32)
            };
            provider.add_dependencies("b", k, [("u", u_range)]);
        }
        provider.add_dependencies("j", 1u32, []);
        provider
    }

    #[test]
    fn widening_finds_same_solution_after_backtracking() {
        let provider = backtracking_provider(4);
        let expected = BTreeMap::from([("root", 1), ("a", 3), ("b", 3), ("j", 1), ("u", 1)]);

        let without = resolve_with_widening(&provider, "root", 1, false).unwrap();
        let with = resolve_with_widening(&provider, "root", 1, true).unwrap();

        assert_eq!(without, expected);
        assert_eq!(with, expected);
    }

    #[test]
    fn widening_finds_same_conflict() {
        // All versions of "b" conflict, so there is no solution.
        let provider = backtracking_provider(0);

        assert_eq!(resolve_with_widening(&provider, "root", 1, false), None);
        assert_eq!(resolve_with_widening(&provider, "root", 1, true), None);
    }

    #[test]
    fn merge_recurring_non_adjacent_dependencies() {
        let mut state: State<OfflineDependencyProvider<&str, NumVS>> = State::init("root", 0);
        let package = state.package_store.alloc("package");

        // Alternate two pairs of constraints so equal ranges recur non-adjacently.
        for version in 0u32..10 {
            let first = (version % 2) * 2;
            state.add_incompatibility_from_dependencies(
                package,
                NumVS::singleton(version),
                [
                    ("dependency", NumVS::singleton(first)),
                    ("dependency", NumVS::singleton(first + 1)),
                ],
            );
        }

        let dependency = state.package_store.alloc("dependency");
        assert_eq!(state.incompatibilities[&package].len(), 4);
        assert_eq!(state.incompatibilities[&dependency].len(), 4);
    }
}
