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

/// An append-only arena whose completed chunks are shared by cloned solver states.
///
/// Appending to a shared partial chunk copies at most [`ARENA_CHUNK_SIZE`] records. Existing
/// records are immutable, so propagation never copies a chunk.
#[derive(PartialEq, Eq)]
pub struct Arena<T> {
    chunks: Vec<Arc<Vec<T>>>,
    len: usize,
}

/// Bound the amount copied when two forks append to the same partial chunk.
const ARENA_CHUNK_SIZE: usize = 256;

impl<T> Clone for Arena<T> {
    fn clone(&self) -> Self {
        Self {
            chunks: self.chunks.clone(),
            len: self.len,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Arena")
            .field("len", &self.len)
            .field("chunks", &self.chunks)
            .finish()
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    pub(crate) fn new() -> Self {
        Self {
            chunks: Vec::new(),
            len: 0,
        }
    }
}

impl<T: Clone> Arena<T> {
    /// Append a record without copying any completed chunk.
    pub(crate) fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id::from_usize(self.len);
        let chunk_index = self.len / ARENA_CHUNK_SIZE;
        if chunk_index == self.chunks.len() {
            self.chunks
                .push(Arc::new(Vec::with_capacity(ARENA_CHUNK_SIZE)));
        }
        let chunk = Arc::make_mut(&mut self.chunks[chunk_index]);
        // A copied Vec retains only its length as capacity. Reserve the chunk's fixed capacity
        // so its next append cannot double an almost-full allocation.
        if chunk.capacity() < ARENA_CHUNK_SIZE {
            chunk.reserve_exact(ARENA_CHUNK_SIZE - chunk.len());
        }
        chunk.push(value);
        self.len += 1;
        id
    }

    /// Append records in order and return their contiguous logical ID range.
    pub(crate) fn alloc_iter<I: Iterator<Item = T>>(&mut self, values: I) -> Range<Id<T>> {
        let start = Id::from_usize(self.len);
        values.for_each(|v| {
            self.alloc(v);
        });
        let end = Id::from_usize(self.len);
        Range { start, end }
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;
    fn index(&self, id: Id<T>) -> &T {
        let index = id.into_raw();
        &self.chunks[index / ARENA_CHUNK_SIZE][index % ARENA_CHUNK_SIZE]
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
    use std::sync::Arc;

    use super::{ARENA_CHUNK_SIZE, Arena, Id};

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
        assert_eq!(arena.len, 4);
        assert_eq!(arena[existing], "existing");
    }

    #[test]
    fn cloned_arenas_share_full_chunks_and_isolate_appends() {
        let mut arena = Arena::new();
        let records = arena.alloc_iter(0..ARENA_CHUNK_SIZE + 1);
        let mut sibling = arena.clone();
        assert!(Arc::ptr_eq(&arena.chunks[0], &sibling.chunks[0]));
        assert!(Arc::ptr_eq(&arena.chunks[1], &sibling.chunks[1]));

        let first = arena.alloc(1000);
        let second = sibling.alloc(2000);
        assert_eq!(first, second);
        assert_eq!(arena[first], 1000);
        assert_eq!(sibling[second], 2000);
        assert!(Arc::ptr_eq(&arena.chunks[0], &sibling.chunks[0]));
        assert!(!Arc::ptr_eq(&arena.chunks[1], &sibling.chunks[1]));
        for id in Id::range_to_iter(records) {
            assert_eq!(arena[id], id.into_raw());
            assert_eq!(sibling[id], id.into_raw());
        }
    }

    #[test]
    fn appending_at_chunk_boundary_does_not_copy_existing_records() {
        #[derive(Debug, PartialEq, Eq)]
        struct ImmutableRecord(usize);

        impl Clone for ImmutableRecord {
            fn clone(&self) -> Self {
                panic!("completed chunks must not clone records");
            }
        }

        let mut arena = Arena::new();
        arena.alloc_iter((0..ARENA_CHUNK_SIZE).map(ImmutableRecord));
        let mut sibling = arena.clone();
        let first = arena.alloc(ImmutableRecord(1000));
        let second = sibling.alloc(ImmutableRecord(2000));
        assert_eq!(arena[first], ImmutableRecord(1000));
        assert_eq!(sibling[second], ImmutableRecord(2000));
        assert!(Arc::ptr_eq(&arena.chunks[0], &sibling.chunks[0]));
    }
}
