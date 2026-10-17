// SPDX-License-Identifier: MPL-2.0

//! Non exposed modules.

mod arena;
mod core;
mod incompatibility;
mod small_map;
mod small_vec;

pub(crate) use arena::{Arena, HashArena};
pub(crate) use core::DecisionLevel;
pub(crate) use incompatibility::{IncompDpId, IncompId, Incompatibility, Relation};
pub(crate) use small_map::SmallMap;
pub(crate) use small_vec::SmallVec;

// uv-specific additions
pub use arena::Id;
#[cfg(feature = "unstable-state-api")]
pub use core::ConflictId;
pub use core::{Dependency, State};
