use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ops::{Index, Range};
use std::sync::Arc;

type FnvIndexSet<V> = indexmap::IndexSet<V, rustc_hash::FxBuildHasher>;

/// The index of a value allocated in an arena that holds `T`s.
///
/// The Clone, Copy and other traits are defined manually because
/// deriving them adds some additional constraints on the `T` generic type
/// that we actually don't need since it is phantom.
///
/// <https://github.com/rust-lang/rust/issues/26925>
///
/// The zero-based index is stored as `index + 1` in a [`NonZeroU32`], allowing
/// `Option<Id<T>>` to use the zero niche and remain four bytes. The largest
/// representable index is therefore `u32::MAX - 1`.
pub struct Id<T> {
    raw: NonZeroU32,
    _ty: PhantomData<fn() -> T>,
}

// Store the zero-based index plus one so `None` can use the zero niche.
const _: () = assert!(std::mem::size_of::<Option<Id<()>>>() == std::mem::size_of::<u32>());

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Id<T>) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.into_raw() as u32).hash(state)
    }
}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut type_name = std::any::type_name::<T>();
        if let Some(id) = type_name.rfind(':') {
            type_name = &type_name[id + 1..]
        }
        write!(f, "Id::<{}>({})", type_name, self.into_raw())
    }
}

impl<T> Id<T> {
    pub(crate) fn into_raw(self) -> usize {
        (self.raw.get() - 1) as usize
    }

    fn from_usize(index: usize) -> Self {
        assert!(index < u32::MAX as usize, "id index exceeds u32::MAX - 1");
        Self {
            raw: NonZeroU32::new(index as u32 + 1).expect("checked id index must be non-zero"),
            _ty: PhantomData,
        }
    }

    pub(crate) fn range_to_iter(range: Range<Self>) -> impl Iterator<Item = Self> {
        let start = range.start.into_raw();
        let end = range.end.into_raw();
        (start..end).map(Self::from_usize)
    }
}

/// A contiguous arena that can discard a completed fork's suffix before its sibling runs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Arena<T> {
    data: Vec<T>,
    checkpoints: Vec<ArenaCheckpoint>,
}

/// Identifies a live prefix; the token distinguishes stale checkpoints after IDs are reused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ArenaCheckpoint {
    len: usize,
    token: Arc<()>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    pub(crate) fn new() -> Self {
        Self {
            data: Vec::new(),
            checkpoints: Vec::new(),
        }
    }

    /// Mark a prefix that remains valid until an earlier checkpoint is restored.
    pub(crate) fn checkpoint(&mut self) -> ArenaCheckpoint {
        let checkpoint = ArenaCheckpoint {
            len: self.data.len(),
            token: Arc::new(()),
        };
        self.checkpoints.push(checkpoint.clone());
        checkpoint
    }

    /// Drop all records after a live checkpoint and invalidate descendant checkpoints.
    ///
    /// The caller must restore the solver indexes and assignment history from the same checkpoint
    /// before reusing IDs in the discarded suffix. The allocation remains available for reuse.
    pub(crate) fn rewind(&mut self, checkpoint: &ArenaCheckpoint) {
        let position = self
            .checkpoints
            .iter()
            .rposition(|candidate| Arc::ptr_eq(&candidate.token, &checkpoint.token))
            .expect("arena checkpoint must belong to the current ancestor chain");
        assert!(checkpoint.len <= self.data.len());
        self.data.truncate(checkpoint.len);
        self.checkpoints.truncate(position + 1);
    }

    pub(crate) fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id::from_usize(self.data.len());
        self.data.push(value);
        id
    }

    /// Append records in order and return their contiguous logical ID range.
    pub(crate) fn alloc_iter<I: Iterator<Item = T>>(&mut self, values: I) -> Range<Id<T>> {
        let start = Id::from_usize(self.data.len());
        values.for_each(|v| {
            self.alloc(v);
        });
        let end = Id::from_usize(self.data.len());
        Range { start, end }
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;
    fn index(&self, id: Id<T>) -> &T {
        &self.data[id.into_raw()]
    }
}

/// Yet another index-based arena. This one de-duplicates entries by hashing.
///
/// An arena is a kind of simple grow-only allocator, backed by a `Vec`
/// where all items have the same lifetime, making it easier
/// to have references between those items.
/// In this case the `Vec` is inside a `IndexSet` allowing fast lookup by value not just index.
/// They are all dropped at once when the arena is dropped.
#[derive(Clone, PartialEq, Eq)]
pub struct HashArena<T: Hash + Eq> {
    data: FnvIndexSet<T>,
}

impl<T: Hash + Eq + fmt::Debug> fmt::Debug for HashArena<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Arena")
            .field("len", &self.data.len())
            .field("data", &self.data)
            .finish()
    }
}

impl<T: Hash + Eq> HashArena<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc(&mut self, value: T) -> Id<T> {
        assert!(
            self.data.len() < u32::MAX as usize || self.data.contains(&value),
            "id index exceeds u32::MAX - 1"
        );
        let (raw, _) = self.data.insert_full(value);
        Id::from_usize(raw)
    }
}

impl<T: Hash + Eq> Default for HashArena<T> {
    fn default() -> Self {
        Self {
            data: FnvIndexSet::default(),
        }
    }
}

impl<T: Hash + Eq> Index<Id<T>> for HashArena<T> {
    type Output = T;
    fn index(&self, id: Id<T>) -> &T {
        &self.data[id.into_raw()]
    }
}

#[cfg(test)]
mod tests {
    use super::{Arena, Id};

    #[test]
    fn alloc_iter_preserves_order_and_duplicates() {
        let mut arena = Arena::new();
        let existing = arena.alloc("existing".to_owned());

        let appended = arena.alloc_iter(
            ["first", "duplicate", "duplicate"]
                .into_iter()
                .map(str::to_owned),
        );

        assert_eq!(existing.into_raw(), 0);
        assert_eq!(arena[existing], "existing");
        assert_eq!(appended.start.into_raw(), 1);
        assert_eq!(appended.end.into_raw(), 4);
        let appended_values: Vec<_> = Id::range_to_iter(appended.clone())
            .map(|id| arena[id].as_str())
            .collect();
        assert_eq!(appended_values, ["first", "duplicate", "duplicate"]);

        let empty = arena.alloc_iter(std::iter::empty::<String>());
        assert_eq!(empty.start.into_raw(), 4);
        assert_eq!(empty.end.into_raw(), 4);
        assert_eq!(arena.data.len(), 4);
        assert_eq!(arena[existing], "existing");
    }

    #[test]
    fn nested_rewinds_preserve_prefixes_and_reuse_ids() {
        let mut arena = Arena::new();
        let root = arena.alloc("root");
        let outer = arena.checkpoint();
        let first = arena.alloc("first branch");
        let inner = arena.checkpoint();
        let nested = arena.alloc("nested branch");
        arena.rewind(&inner);
        assert_eq!(arena[root], "root");
        assert_eq!(arena[first], "first branch");
        assert_eq!(arena.alloc("nested sibling"), nested);
        assert_eq!(arena[nested], "nested sibling");
        arena.rewind(&outer);
        assert_eq!(arena.alloc("outer sibling"), first);
        assert_eq!(arena[first], "outer sibling");
        assert_eq!(arena[root], "root");
    }

    #[test]
    fn rewind_drops_suffix_records_without_copying_the_prefix() {
        use std::cell::Cell;
        use std::rc::Rc;

        struct Record(Rc<Cell<usize>>);
        impl Drop for Record {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }
        let drops = Rc::new(Cell::new(0));
        let mut arena = Arena::new();
        arena.alloc(Record(Rc::clone(&drops)));
        let checkpoint = arena.checkpoint();
        arena.alloc(Record(Rc::clone(&drops)));
        arena.alloc(Record(Rc::clone(&drops)));
        let capacity = arena.data.capacity();
        arena.rewind(&checkpoint);
        assert_eq!(drops.get(), 2);
        assert_eq!(arena.data.len(), 1);
        assert_eq!(arena.data.capacity(), capacity);
        drop(arena);
        assert_eq!(drops.get(), 3);
    }

    #[test]
    #[should_panic(expected = "arena checkpoint must belong to the current ancestor chain")]
    fn rewind_rejects_a_discarded_descendant_even_after_ids_are_reused() {
        let mut arena = Arena::new();
        arena.alloc(1);
        let outer = arena.checkpoint();
        arena.alloc(2);
        let stale = arena.checkpoint();
        arena.rewind(&outer);
        arena.alloc(3);
        let _replacement = arena.checkpoint();
        arena.rewind(&stale);
    }

    #[test]
    #[should_panic(expected = "arena checkpoint must belong to the current ancestor chain")]
    fn rewind_rejects_a_checkpoint_from_another_arena() {
        let mut first = Arena::new();
        first.alloc(1);
        let checkpoint = first.checkpoint();
        let mut second = Arena::new();
        second.alloc(1);
        second.rewind(&checkpoint);
    }
}
