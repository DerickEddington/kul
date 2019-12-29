//! Traits that are our abstraction of "text".

#![allow(clippy::module_name_repetitions)]

use core::{str, iter::Map, cmp::Ordering, hash::{Hash, Hasher}};

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

    /// Like [`kul_core::SourceStream`](TODO), but without `DatumAllocator`,
    /// for `TextChunk`s.  Only accumulates within a single chunk, not across
    /// multiple chunks, unlike `kul_core::SourceStream`.  `iter::Iter as
    /// kul_core::SourceStream` builds on this.
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
fn sii_ch<P>(SourceIterItem{ch, ..}: SourceIterItem<P>) -> char {
    ch
}

/// The type returned by [`Text::chars`].
///
/// [`Text::chars`]: trait.Text.html#method.chars
pub type Chars<'text, TextType> =
    Map<iter::Iter<'text, TextType>,
        fn(SourceIterItem<<TextType as TextBase>::Pos>) -> char>;

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
    fn from_chunkish<T>(v: T) -> Self
        where T: Into<Self::Chunk>
    {
        Self::from(v.into())
    }

    /// Make an instance of our `Self` from a `&str` slice, if our `Chunk` type
    /// can convert from that.
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

    /// Construct a new iterator that yields the logical character sequence of
    /// the given `self`.
    ///
    /// The default implementation uses our special iterator type to enable
    /// yielding characters across arbitrary, often inconsistent, chunk
    /// boundaries.
    //
    // FUTURE: It'd be nice if this could instead return `impl Iterator<Item =
    // char>` but that feature of Rust is not stable yet for trait methods.
    // Once that can be done, this trait's other methods' default
    // implementations should use this.  Currently, the `fn` type in the `Map`
    // type probably results in indirected function calls and so is probably
    // slower than using `self.iter().map(sii_ch)` directly.
    fn chars(&self) -> Chars<'_, Self> {
        self.iter().map(sii_ch)
    }

    /// Encode the logical character sequence of the given `self` as UTF-8 into
    /// the provided byte buffer.  If the buffer is large enough to contain all
    /// the characters, then return the subslice of the buffer that contains
    /// them all, as an `Ok`.  If the buffer is too small, then return the
    /// subslice that contains as many as fit (which might be shorter than the
    /// provided buffer), as an `Err`.
    ///
    /// This is intended for `no_std` constrained applications where `String` is
    /// unavailable.  When it is available, instead of this function, it is
    /// probably more desirable to use: `String::from_iter(self.chars())`.
    ///
    /// The default implementation uses our special iterator type to get the
    /// characters across arbitrary, often inconsistent, chunk boundaries.
    fn encode_utf8<'b>(&self, buf: &'b mut [u8]) -> Result<&'b str, &'b str> {
        // Note: I couldn't figure out a better, `forbid(unsafe_code)`, way to
        // do this.  Seems like the `core` library could instead provide some
        // `encode_utf8_from_iter` function that takes an `Iterator<Item=char>`
        // and encodes as much as fits in the slice and returns a `&str`
        // covering it all.  Seems like that would avoid having to do
        // `str::from_utf8`, which scans across the slice to check the encoding
        // validity which is unnecessary in this case, just to get a `&str` that
        // covers it all.  Did I miss something else that the `core` library
        // already provides that would do this better?
        let mut pos = 0;
        macro_rules! as_str {
            // This `unwrap` will never fail because encoding is always valid.
            () => { str::from_utf8(&buf[..pos]).unwrap() }
        }
        for ch in self.iter().map(sii_ch) {
            if pos + ch.len_utf8() <= buf.len() {
                let s = ch.encode_utf8(&mut buf[pos..]);
                pos += s.len();
            } else {
                return Err(as_str!())
            }
        }
        Ok(as_str!())
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
    fn iter_chunks(&self) -> iter::chunks::Iter<'_, Self> {
        iter::chunks::Iter::new(self)
    }

    /// Construct a new iterator, which is also a [`kul_core::SourceStream`]
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
    /// [`kul_core::SourceStream`]: TODO
    /// [`TextConcat`]: TODO
    /// [`text::Iter`]: TODO
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
pub trait TextConcat<DA>: Text {
    /// Concatenate two `Text`s (of the same type) to form a single `Text` that
    /// logically represents this.  The `datum_alloc` argument may be ignored by
    /// some (most) implementations and exists only to support implementations
    /// like `TextDatumList`.  If the implementation ignores `datum_alloc`, it
    /// is safe to use `unwrap` on the returned `Result`.
    fn concat(self, other: Self, datum_alloc: &mut DA) -> Result<Self, AllocError>;
}


#[cfg(test)]
mod tests {
    use super::{*, premade::TextDatumList, chunk::premade::PosStr};

    type TT<'d> = TextDatumList<'d, PosStr<'static>, ()>;

    #[test]
    fn encode_utf8() {
        assert_eq!(TT::from_str("").encode_utf8(&mut []), Ok(""));
        assert_eq!(TT::from_str("").encode_utf8(&mut [0; 1]), Ok(""));
        assert_eq!(TT::from_str("").encode_utf8(&mut [0; 123]), Ok(""));
        assert_eq!(TT::from_str("a").encode_utf8(&mut [0; 1]), Ok("a"));
        assert_eq!(TT::from_str("a").encode_utf8(&mut [0; 2]), Ok("a"));
        assert_eq!(TT::from_str("a").encode_utf8(&mut [0; 3]), Ok("a"));
        assert_eq!(TT::from_str("a").encode_utf8(&mut []), Err(""));
        assert_eq!(TT::from_str("raboof").encode_utf8(&mut [0; 6]), Ok("raboof"));
        assert_eq!(TT::from_str("raboof").encode_utf8(&mut [0; 512]), Ok("raboof"));
        assert_eq!(TT::from_str("raboof").encode_utf8(&mut [0; 5]), Err("raboo"));
        assert_eq!(TT::from_str("raboof").encode_utf8(&mut [0; 2]), Err("ra"));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 6]), Ok("▷ λ"));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 8]), Ok("▷ λ"));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 99]), Ok("▷ λ"));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 5]), Err("▷ "));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 4]), Err("▷ "));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 3]), Err("▷"));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 2]), Err(""));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut [0; 1]), Err(""));
        assert_eq!(TT::from_str("▷ λ").encode_utf8(&mut []), Err(""));
    }
}
