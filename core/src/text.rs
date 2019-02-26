//! Traits that are our abstraction of "text".

#![allow(clippy::stutter)]

use core::{cmp::Ordering, hash::{Hash, Hasher}};

use crate::{SourceIterItem, SourcePosition};
use crate::parser::AllocError;


pub mod iter;

/// Implementations provided for ready use.
pub mod premade {
    mod datum_list;
    pub use datum_list::TextDatumList;
}


/// The basic interface common across both `Text`s and `TextChunk`s.  This
/// determines the associated type of the characters' positional information;
/// and this provides the ability to construct and check for emptiness.
// TODO: Impl indexing?  But probably not slicing since that seems like it'd
// require dynamic allocation for dealing with chunk boundaries, which for some
// impls like TextDatumList is not possible (because the standard slicing API
// isn't able to provide the needed DatumAllocator).
pub trait TextBase
    where Self: Sized,
{
    /// Positional information used with our chunks and `char`s.
    type Pos: SourcePosition;

    /// Make an empty one.
    fn empty() -> Self;

    /// Predicate for if an instance is an empty one.
    fn is_empty(&self) -> bool;
}


/// Items related closely to the `TextChunk` trait.
pub mod chunk {
    use crate::SourceIterItem;
    use super::TextChunk;

    /// Implementations provided for ready use.
    pub mod premade {
        mod pos_str;
        pub use pos_str::*;
    }

    /// Like [`kruvi_core::SourceStream`](TODO), but without `DatumAllocator`,
    /// for `TextChunk`s.  Only accumulates within a single chunk, not across
    /// multiple chunks, unlike `kruvi_core::SourceStream`.  `iter::Iter as
    /// kruvi_core::SourceStream` builds on this.
    pub trait SourceStream<C>: Iterator<Item = SourceIterItem<C::Pos>>
        where C: TextChunk,
    {
        /// Returns a reference to the next item's value without advancing the
        /// iterator and without interfering with any pending accumulation.
        fn peek(&mut self) -> Option<&<Self as Iterator>::Item>;

        /// Get the next item, if any, and add it to a pending, or start a new,
        /// accumulation, and return the item.
        ///
        /// When there is `None` next item, any pending accumulation is
        /// preserved.
        fn next_accum(&mut self) -> Option<<Self as Iterator>::Item>;

        /// Take any pending accumulation and return it as a new chunk, or
        /// return an empty chunk if there was nothing pending.
        ///
        /// The accumulation state is reset to nothing.
        fn accum_done(&mut self) -> C;
    }
}

/// A sequence of characters that serves as a single chunk in the underlying
/// representation of some `Text` type.
pub trait TextChunk: TextBase {
    /// Our `chunk::SourceStream` type.
    // FUTURE: Use `generic_associated_types` so this can have a lifetime
    // parameter.
    type CharsSrcStrm: chunk::SourceStream<Self>;

    /// Construct a new iterator, which is also a `chunk::SourceStream`, that
    /// yields the character sequence, and their positions, of the given `self`
    /// chunk.
    // FUTURE: Use `generic_associated_types` to enable having the same lifetime
    // in `CharsSrcStrm<'_>` as this method call's borrow of `self`.  This will
    // enable new possibilities of implementation such as multi-level chunking
    // with chunks which are themselves `Text` types composed of underlying
    // chunks, where a `CharsSrcStrm<'a>` is the `TextIter<'a>` of such types.
    // This will also enable chunk types backed by things like `String` which
    // need to return borrows related to the call lifetimes to be able to return
    // a `CharsSrcStrm`.
    fn src_strm(&self) -> Self::CharsSrcStrm;
}


/// Helper for some `Text` methods.
#[inline]
fn sii_ch<P>(SourceIterItem{ch, ..}: SourceIterItem<P>) -> char {
    ch
}

/// A logical sequence of characters, possibly represented as separate chunks,
/// that can be iterated multiple times without consuming or destroying the
/// source, and that might know its characters' positions in the source it is
/// from.
///
/// Because Rust's [`generic_associated_types`] is not stable yet, this trait
/// has a design that enables a somewhat-flexible interface for relating the
/// lifetimes of borrows that enables different implementations of how the
/// chunking is represented internally.  This enables the iteration
/// functionality to generically work with all types of this trait.
///
/// Types of this trait are required to be able to be constructed from a single
/// chunk, which assists its use.
///
/// [`generic_associated_types`]: https://github.com/rust-lang/rfcs/blob/master/text/1598-generic_associated_types.md
pub trait Text: TextBase
    where Self: From<<Self as Text>::Chunk>,
{
    /// The type of underlying chunks used to represent our character sequence.
    type Chunk: TextChunk<Pos = Self::Pos>;
    /// Enables generic flexibility in the internal representation of how chunks
    /// are held and chained, while also enabling the borrowing of references to
    /// this from the `self` so that the lifetimes are those of our method
    /// calls' borrows of `self`.
    type IterChunksState: iter::chunks::State<Chunk = Self::Chunk> + ?Sized;

    /// Make an instance of our `Self` from anything that can convert into a
    /// single chunk of our `Chunk` type.
    #[inline]
    fn from_chunkish<T>(v: T) -> Self
        where T: Into<Self::Chunk>
    {
        Self::from(v.into())
    }

    /// Make an instance of our `Self` from a `&str` slice, if our `Chunk` type
    /// can convert from that.
    #[inline]
    fn from_str<'s>(s: &'s str) -> Self
        where Self::Chunk: From<&'s str>
    {
        Self::from_chunkish(s)
    }

    /// Equality comparison with any other type of `Text`.  Compares the logical
    /// sequences of `char`s.
    ///
    /// Useful here because `PartialEq` and `Eq` cannot be blanket-implemented
    /// between all generic `Text` types.  The default implementation uses our
    /// special iterator type to enable comparing across arbitrary, often
    /// inconsistent, chunk boundaries.
    ///
    /// This is a full equivalence relation.
    fn eq<O: Text>(&self, other: &O) -> bool {
        self.iter().map(sii_ch).eq(other.iter().map(sii_ch))
    }

    /// Ordering comparison with any other type of `Text`.  Compares the logical
    /// sequences of `char`s lexicographically.
    ///
    /// Useful here because `PartialOrd` and `Ord` cannot be blanket-implemented
    /// between all generic `Text` types.  The default implementation uses our
    /// special iterator type to enable comparing across arbitrary, often
    /// inconsistent, chunk boundaries.
    ///
    /// This is a total ordering relation.
    fn cmp<O: Text>(&self, other: &O) -> Ordering {
        self.iter().map(sii_ch).cmp(other.iter().map(sii_ch))
    }

    /// Hash the logical sequence of `char`s.
    ///
    /// The default implementation uses our special iterator type to enable
    /// hashing across arbitrary, often inconsistent, chunk boundaries.
    fn hash<H: Hasher>(&self, state: &mut H) {
        for ch in self.iter().map(sii_ch) {
            ch.hash(state);
        }
    }

    /// Return a borrow of our `self`'s particular representation of chained
    /// chunks to be used by our special iterator types.
    ///
    /// A `None` return means we have zero chunks (and so are logically empty),
    /// but a `Some` return with one or more chunks may also represent logical
    /// emptiness, and some types do canonically represent emptiness with at
    /// least one chunk.
    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState>;

    /// Construct a new iterator that yields borrows of each of our underlying
    /// chunks.
    ///
    /// Used by both the special `text::iter::Iter` and by some other things
    /// that want to process each chunk.
    #[inline]
    fn iter_chunks(&self) -> iter::chunks::Iter<'_, Self> {
        iter::chunks::Iter::new(self)
    }

    /// Construct a new iterator, which is also a [`kruvi_core::SourceStream`]
    /// if the `Self` type is also a [`TextConcat`], that yields the logical
    /// character sequence, and their positions, of the given `self`.
    ///
    /// The returned [`text::Iter`] type is parameterized over the same lifetime
    /// as the borrows of `self` of calls of this method, which enables it to
    /// contain borrows derived from a `self` borrow, which is essential.
    ///
    /// This is how the correct lifetime relating is achieved without generic
    /// asssociated types.  If/when the `generic_associated_types` feature
    /// becomes available in stable Rust, our design should probably be redone
    /// to leverage that feature for a cleaner design.
    ///
    /// [`kruvi_core::SourceStream`]: TODO
    /// [`TextConcat`]: TODO
    /// [`text::Iter`]: TODO
    #[inline]
    fn iter(&self) -> iter::Iter<'_, Self> {
        iter::Iter::new(self)
    }
}


/// A [`Text`](trait.Text.html) that can logically concatenate its values,
/// optionally by using a provided [`DatumAllocator`](TODO).
///
/// Separating this concatenation functionality from the `Text` trait avoids
/// difficulties that otherwise would happen with needing to have the `DA:
/// DatumAllocator` type parameter where not really needed.
///
/// The `Datum` allocation support exists to support [`TextDatumList`](TODO),
/// but it hypothetically might be useful to other potential implementations.
/// The `DA` type argument must be the same as that of the [`Parser`s](TODO)
/// this is used with.  When this is implemented for types that ignore the
/// `DatumAllocator`, the `DA` type should be a generic type parameter that
/// covers all (ignored) possibilities.
pub trait TextConcat<DA>: Text
    where DA: ?Sized,
{
    /// Concatenate two `Text`s (of the same type) to form a single `Text` that
    /// logically represents this.  The `datum_alloc` argument may be ignored by
    /// some (most) implementations and exists only to support implementations
    /// like `TextDatumList`.  If the implementation ignores `datum_alloc`, it
    /// is safe to use `unwrap` on the returned `Result`.
    fn concat(self, other: Self, datum_alloc: &mut DA) -> Result<Self, AllocError>;
}
