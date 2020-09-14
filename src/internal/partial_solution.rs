// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! The partial solution is the current state
//! of the solution being built by the algorithm.

use std::collections::HashMap as Map;
use std::hash::Hash;

use crate::internal::assignment::Assignment;
use crate::internal::assignment::Kind;
use crate::internal::incompatibility::Incompatibility;
use crate::internal::incompatibility::Relation;
use crate::internal::memory::Memory;
use crate::internal::term::Term;
use crate::range::Range;
use crate::version::Version;

/// The partial solution is the current state
/// of the solution being built by the algorithm.
/// It is composed of a succession of assignments,
/// defined as either decisions or derivations.
///
/// TODO: make sure that when I use the history,
/// it is in the correct direction.
#[derive(Clone)]
pub struct PartialSolution<'a, P, V>
where
    P: Clone + Eq + Hash,
    V: Clone + Ord + Version,
{
    decision_level: usize,
    history: Vec<Assignment<'a, P, V>>,
    memory: Memory<P, V>,
}

impl<'a, P, V> PartialSolution<'a, P, V>
where
    P: Clone + Eq + Hash,
    V: Clone + Ord + Version,
{
    /// Initialize an empty partial solution.
    pub fn empty() -> Self {
        Self {
            decision_level: 0,
            history: Vec::new(),
            memory: Memory::empty(),
        }
    }

    fn from_assignments(assignments: Vec<Assignment<'a, P, V>>) -> Self {
        let mut decision_level = 0;
        let memory = assignments.iter().fold(Memory::empty(), |mut m, a| {
            m.add_assignment(a);
            decision_level = decision_level.max(a.decision_level);
            m
        });
        Self {
            decision_level,
            history: assignments,
            memory,
        }
    }

    /// If a partial solution has, for every positive derivation,
    /// a corresponding decision that satisfies that assignment,
    /// it's a total solution and version solving has succeeded.
    pub fn extract_solution(&self) -> Option<Map<P, V>> {
        self.memory.extract_solution()
    }

    /// Add a decision to the partial solution.
    pub fn add_decision(&mut self, package: P, version: V) {
        self.decision_level += 1;
        self.history.push(Assignment::new_decision(
            self.decision_level,
            package.clone(),
            version.clone(),
        ));
        self.memory.add_decision(package, version);
    }

    /// Add a derivation to the partial solution.
    pub fn add_derivation(&mut self, package: P, term: Term<V>, cause: Incompatibility<'a, P, V>) {
        self.history.push(Assignment::new_derivation(
            self.decision_level,
            package.clone(),
            term.clone(),
            cause,
        ));
        self.memory.add_derivation(package, term);
    }

    /// Backtrack the partial solution to a given decision level.
    pub fn backtrack(self, decision_level: usize) -> Self {
        let mut history = self.history;
        let index =
            Self::find_level_index(history.as_slice(), decision_level + 1).unwrap_or(history.len());
        history.truncate(index);
        Self::from_assignments(history)
    }

    // TODO: improve with dichotomic search.
    fn find_level_index(history: &[Assignment<'a, P, V>], decision_level: usize) -> Option<usize> {
        for (index, level) in history.iter().map(|a| a.decision_level).enumerate() {
            if level >= decision_level {
                return Some(index);
            }
        }
        None
    }

    /// Extract all packages that may potentially be picked next
    /// to continue solving package dependencies.
    /// A package is a potential pick if there isn't an already
    /// version selected (no "decision")
    /// and if it contains at least one positive derivation term
    /// in the partial solution.
    pub fn potential_packages(&self) -> impl Iterator<Item = (&P, &[Term<V>])> {
        self.memory.potential_packages()
    }

    /// We can add the version to the partial solution as a decision
    /// if it doesn't produce any conflict with the new incompatibilities.
    /// In practice I think it can only produce a conflict if one of the dependencies
    /// (which are used to make the new incompatibilities)
    /// is already in the partial solution with an incompatible version.
    pub fn add_version(
        &self,
        package: P,
        version: V,
        new_incompatibilities: &[Incompatibility<'a, P, V>],
    ) -> Option<PartialSolution<'a, P, V>> {
        let mut updated_partial_solution = self.clone();
        updated_partial_solution.add_decision(package, version);
        if updated_partial_solution.satisfies_any_of(new_incompatibilities) {
            None
        } else {
            Some(updated_partial_solution)
        }
    }

    fn satisfies_any_of(&self, incompatibilities: &[Incompatibility<'a, P, V>]) -> bool {
        incompatibilities
            .iter()
            .any(|incompat| self.relation(incompat) == Relation::Satisfied)
    }

    /// Check if the terms in the partial solution satisfy the incompatibility.
    pub fn relation(&self, incompat: &Incompatibility<'a, P, V>) -> Relation<P, V> {
        incompat.relation(&mut self.memory.all_terms())
    }

    /// A satisfier is the earliest assignment in partial solution such that the incompatibility
    /// is satisfied by the partial solution up to and including that assignment.
    /// Also returns all assignments earlier than the satisfier.
    /// We call the term in the incompatibility that refers to the same package "term".
    pub fn find_satisfier(
        &self,
        incompat: &Incompatibility<'a, P, V>,
    ) -> (Assignment<'a, P, V>, Self, Term<V>) {
        todo!()
    }
    // findSatisfier : Incompatibility -> PartialSolution -> ( Assignment, PartialSolution, Term )
    // findSatisfier incompat (PartialSolution partial _) =
    //     if List.isEmpty partial then
    //         Debug.todo "We should never call findSatisfier with an empty partial solution"
    //
    //     else
    //         let
    //             incompatDict =
    //                 Incompatibility.asDict incompat
    //
    //             accumSatisfier =
    //                 Dict.map (\_ _ -> ( False, Term.Negative Range.none )) incompatDict
    //         in
    //         case findSatisfierHelper incompatDict accumSatisfier [] (List.reverse partial) of
    //             -- Not using Maybe.withDefault because Debug.todo crashes
    //             Nothing ->
    //                 Debug.todo "Should always find a satisfier right?"
    //
    //             Just value ->
    //                 value

    /// Earliest assignment in the partial solution before satisfier
    /// such that incompatibility is satisfied by the partial solution up to
    /// and including that assignment plus satisfier.
    pub fn find_previous_satisfier(
        &self,
        satisfier: &Assignment<'a, P, V>,
        incompat: &Incompatibility<'a, P, V>,
    ) -> Option<(Assignment<'a, P, V>, Self, Term<V>)> {
        todo!()
    }
    // findPreviousSatisfier : Assignment -> Incompatibility -> PartialSolution -> Maybe ( Assignment, PartialSolution, Term )
    // findPreviousSatisfier satisfier incompat (PartialSolution earlierPartial _) =
    //     let
    //         incompatDict =
    //             Incompatibility.asDict incompat
    //
    //         incompatSatisfierTerm =
    //             case Dict.get satisfier.package incompatDict of
    //                 -- Not using Maybe.withDefault because Debug.todo crashes
    //                 Nothing ->
    //                     Debug.todo "shoud exist"
    //
    //                 Just t ->
    //                     t
    //
    //         satisfierTerm =
    //             Assignment.getTerm satisfier.kind
    //
    //         accumSatisfier =
    //             Dict.map (\_ _ -> ( False, Term.Negative Range.none )) incompatDict
    //                 |> Dict.insert satisfier.package
    //                     ( satisfierTerm |> Term.subsetOf incompatSatisfierTerm
    //                     , satisfierTerm
    //                     )
    //     in
    //     findSatisfierHelper incompatDict accumSatisfier [] (List.reverse earlierPartial)

    /// Iterate over the assignments (oldest must be first)
    /// until we find the first one such that the set of all assignments until this one
    /// satisfies the given incompatibility.
    pub fn find_satisfier_helper(
        incompat: &Incompatibility<'a, P, V>,
        accum_satisfier: Map<P, (bool, Term<V>)>,
        accum_assignments: Vec<Assignment<'a, P, V>>,
        new_assignment: &[Assignment<'a, P, V>],
    ) -> Option<(Assignment<'a, P, V>, Self, Term<V>)> {
        todo!()
    }
    // findSatisfierHelper : Dict String Term -> Dict String ( Bool, Term ) -> List Assignment -> List Assignment -> Maybe ( Assignment, PartialSolution, Term )
    // findSatisfierHelper incompat accumSatisfier accumAssignments newAssignments =
    //     case newAssignments of
    //         [] ->
    //             Nothing
    //
    //         assignment :: otherAssignments ->
    //             case Dict.get assignment.package incompat of
    //                 Nothing ->
    //                     -- We don't care of that assignment if its corresponding package is not in the incompatibility.
    //                     findSatisfierHelper incompat accumSatisfier (assignment :: accumAssignments) otherAssignments
    //
    //                 Just incompatTerm ->
    //                     -- If that package corresponds to a package in the incompatibility
    //                     -- check if it is satisfied with the new assignment.
    //                     case Dict.get assignment.package accumSatisfier of
    //                         Nothing ->
    //                             Debug.todo "A key in incompat should always exist in accumAssignments"
    //
    //                         Just ( True, _ ) ->
    //                             -- package term is already satisfied, no need to check
    //                             findSatisfierHelper incompat accumSatisfier (assignment :: accumAssignments) otherAssignments
    //
    //                         Just ( False, accumTerm ) ->
    //                             -- check if the addition of the new term helps satisfying
    //                             let
    //                                 newAccumTerm =
    //                                     Term.intersection (Assignment.getTerm assignment.kind) accumTerm
    //
    //                                 termSatisfied =
    //                                     newAccumTerm
    //                                         |> Term.subsetOf incompatTerm
    //
    //                                 newAccumSatisfier =
    //                                     Dict.insert assignment.package ( termSatisfied, newAccumTerm ) accumSatisfier
    //
    //                                 foundSatisfier =
    //                                     Utils.dictAll (\_ ( satisfied, _ ) -> satisfied) newAccumSatisfier
    //
    //                                 newAccumAssignment =
    //                                     assignment :: accumAssignments
    //                             in
    //                             if foundSatisfier then
    //                                 Just ( assignment, fromAssignements newAccumAssignment, incompatTerm )
    //
    //                             else
    //                                 findSatisfierHelper incompat newAccumSatisfier newAccumAssignment otherAssignments
}