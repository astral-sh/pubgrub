use crate::{Range, SmallVec};
use std::collections::Bound::{Excluded, Included, Unbounded};

use proptest::prelude::*;

/// A strategy for using versions in proptests.
///
/// Generate version sets from a random vector of deltas between bounds. Each bound is randomly
/// inclusive or exclusive.
pub fn proptest_strategy() -> impl Strategy<Value = Range<u32>> {
    (
        any::<bool>(),
        prop::collection::vec(any::<(u32, bool)>(), 1..10),
    )
        .prop_map(|(start_unbounded, deltas)| to_range(start_unbounded, deltas))
}

fn to_range(start_unbounded: bool, deltas: Vec<(u32, bool)>) -> Range<u32> {
    let mut start = if start_unbounded {
        Some(Unbounded)
    } else {
        None
    };
    let mut largest: u32 = 0;
    let mut last_bound_was_inclusive = false;
    let mut segments = SmallVec::new();
    for (delta, inclusive) in deltas {
        // Add the offset to the current bound
        largest = match largest.checked_add(delta) {
            Some(s) => s,
            None => {
                // Skip this offset, if it would result in a too large bound.
                continue;
            }
        };

        let current_bound = if inclusive {
            Included(largest)
        } else {
            Excluded(largest)
        };

        // If we already have a start bound, the next offset defines the complete range.
        // If we don't have a start bound, we have to generate one.
        if let Some(start_bound) = start.take() {
            // If the delta from the start bound is 0, the only authorized configuration is
            // Included(x), Included(x)
            if delta == 0 && !(matches!(start_bound, Included(_)) && inclusive) {
                start = Some(start_bound);
                continue;
            }
            last_bound_was_inclusive = inclusive;
            segments.push((start_bound, current_bound));
        } else {
            // If the delta from the end bound of the last range is 0 and
            // any of the last ending or current starting bound is inclusive,
            // we skip the delta because they basically overlap.
            if delta == 0 && (last_bound_was_inclusive || inclusive) {
                continue;
            }
            start = Some(current_bound);
        }
    }

    // If we still have a start bound, but didn't have enough deltas to complete another
    // segment, we add an unbounded upperbound.
    if let Some(start_bound) = start {
        segments.push((start_bound, Unbounded));
    }

    Range { segments }.check_invariants()
}
