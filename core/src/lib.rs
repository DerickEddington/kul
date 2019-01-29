//! The core of a parser for a unique textual notation that can be used as both
//! a data format and/or a markup language and that allows powerful
//! extensibility of both syntax and semantics.  It is inspired by the
//! little-known [Curl programming
//! language](http://en.wikipedia.org/wiki/Curl_(programming_language)).  It is
//! very parameterized to allow maximal reuse for different applications.  It is
//! capable of zero-copy operation (depending on how you concretize it),
//! including for its generic designs of chunked text representations and
//! omitting escape characters.
//!
//! The notation is similar to Lisp S-expressions in that there are nested forms
//! delimited by brackets and in that the first sub-form in a nest (the "head")
//! can be interpreted as an operator (which can also be thought of as a
//! constructor).  Unlike S-expressions, but like Curl, the parsing and meaning
//! of nested text and nested forms can be extended by two types of macros
//! (somewhat like "reader macros" of Lisp).  Also unlike S-expressions, all
//! text outside nested forms is preserved exactly, as is the text inside some
//! nested forms, and so the notation is also a markup language.  Head forms can
//! be bound to macros, which is what causes them to be interpreted as
//! operators, but they can also be unbound which leaves a nested form
//! uninterpreted.
//!
//! The macros are implemented as functions, termed "combiner"s.  One of the
//! types of combiner, termed "operative", takes nested text unparsed and can
//! parse it however it wants.  The other type of combiner, termed
//! "applicative", takes a list of forms produced by recursively parsing nested
//! text.  For both combiner types, whatever is returned is substituted for the
//! nested form in the abstract syntax tree (AST) returned by the parser.  (The
//! terms "combiner", "operative", and "applicative" come from the [Kernel
//! programming language](http://web.cs.wpi.edu/~jshutt/kernel.html) and its
//! F-expressions, which are somewhat analogous.)
//!
//! The parser is intended to be extended, by binding combiners, for each
//! application, but it can be used without extension, i.e. without any macros,
//! as a simplistic kind of S-expression language where the basic AST is used as
//! your data structure.
//!
//! This core crate is `no_std` and so can be used in constrained environments
//! without heap allocation.  The crate is generically parameterized over what
//! allocates the "datums" used as nodes in the constructed ASTs.  Allocation
//! can be done from fixed-size, pre-established, stack arrays.  Or, allocation
//! can be done from a heap, e.g. using the standard `Box` type, or from
//! whatever kind of allocator you can arrange.
//!
//! TODO?: Should the Kernel terms be dropped in favor of terms like "text
//! macro", "form macro", and "constructor" instead?  Those terms are probably
//! more familiar for a language focused on textual syntax, vs. the Kernel terms
//! which require an analogy between expression-form evaluation and
//! extensible-parsing to really understand.  I like the Kernel terms and
//! analogy because parsing can be viewed as evaluation of text and the AST as
//! the denoted values, which seems fitting for this crate which kind of blends
//! both into a hybrid and where there are two complementary ways of processing
//! forms like in Kernel.

// TODO: Impl PartialEq, Eq, PartialOrd, and Ord for the new text types

// TODO: Derive the usual traits like Debug, for the new types, as appropriate

// TODO: Finish doc-string links

// TODO?: Use mod modules to organize better?

// TODO: Review what's pub and not

#![no_std]

// TODO: After separating into modules, review which of these should be here or
// in a module
use core::ops::{Deref, DerefMut};
use core::str::CharIndices;
use core::iter::{self, Peekable, Map, Zip, Repeat, Enumerate};
use core::cmp::Ordering;

// TODO: Delete these to require full paths, for clarity now that this crate is
// more complex
use self::Datum::*;
use self::Combiner::*;
use self::Error::*;


/// The possible errors that might be returned by parsing.
/// It is extensible by the `CombinerError` type parameter.
#[derive(Copy, Clone, Eq, Debug)]
pub enum Error<Position, CombinerError> {
    /// Close-bracket without matching open-bracket
    UnbalancedEndChar(Position),
    /// End-of-stream reached inside nest form
    MissingEndChar, // TODO?: Position for this too?,
    /// `Datum` allocator error
    FailedAlloc(AllocError),
    /// [`DerefTryMut::get_mut`](trait.DerefTryMut.html#tymethod.get_mut) failed
    FailedDerefTryMut,
    /// Extensibility that custom macros/combiners may utilize to add additional
    /// error variants
    FailedCombiner(CombinerError),
}

/// The possible errors that might be returned by a parser's `Datum` allocator.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum AllocError {
    /// No more free
    AllocExhausted,
}

impl<POS, CE> From<AllocError> for Error<POS, CE> {
    fn from(ae: AllocError) -> Self {
        Error::FailedAlloc(ae)
    }
}

/// This allows different concrete [`Error`](enum.Error.html) types to be
/// compared with each other for equality if their [character position
/// types](enum.Error.html#variant.UnbalancedEndChar) and [combiner error
/// types](enum.Error.html#variant.FailedCombiner) can be.
impl<P1, P2, CE1, CE2> PartialEq<Error<P2, CE2>> for Error<P1, CE1>
    where P1: PartialEq<P2>,
          CE1: PartialEq<CE2>,
{
    fn eq(&self, other: &Error<P2, CE2>) -> bool {
        match (self, other) {
            (UnbalancedEndChar(pos1),
             UnbalancedEndChar(pos2))
                => *pos1 == *pos2,
            (MissingEndChar, MissingEndChar)
                => true,
            (FailedAlloc(ae1), FailedAlloc(ae2))
                => *ae1 == *ae2,
            (FailedDerefTryMut, FailedDerefTryMut)
                => true,
            (FailedCombiner(ce1), FailedCombiner(ce2))
                => *ce1 == *ce2,
            _
                => false
        }
    }
}


// TODO: Move all the text stuff, old and new, into a new module file named text.rs

// TODO: Impl `Text` (and so a SourceStream too) for:
// - &[char] (which will also work for Vec<char> outside this crate with `std`)
// ? &[u16] for UTF-16 support (or whatever type makes sense for UTF-16)
// ? Others like CStr, OsStr, etc?

// TODO: Move the Text(Base) impls into their own modules.  Then, try to shorten
// the names by using items relative to the modules names.


// FUTURE: When/if the `generic_associated_types` feature of Rust becomes
// stable, use it so all the text chunk and char iterators can be generic and
// defined by the implementors and have the needed access to the lifetimes of
// the method calls' borrows of their `self`, instead of the current design that
// has the concrete iterator types and the odd state borrowing and transforming
// (which was a workaround done to have access to the needed lifetimes).


/// TODO
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct SourceIterItem<P> {
    pub ch: char,
    pub pos: P,
}

#[inline]
fn sii_ch<P>(SourceIterItem{ch, ..}: SourceIterItem<P>) -> char {
    ch
}


/// Positional information of a text string or character relative to the
/// original source it is from.
// TODO: Should it be bound by: Display?, Debug?
pub trait TextPosition
    where Self: Clone,
{
    fn empty() -> Self;
}

impl TextPosition for () {
    #[inline]
    fn empty() -> Self {
        ()
    }
}


/// A stream of characters that can be iterated only once and that might know
/// its characters' positions in the source it is from.
///
/// It may be used with streaming sources that might consume or destroy the
/// source, or it may be used with sources that can be iterated more than once
/// by each time constructing new iterators that implement this trait.
///
/// It is able to accumulate its iterated items, when its `next_accum` method is
/// called, until its `accum_done` method is called, and this may be done
/// multiple times.  The supertrait `next` method will be called instead when
/// the next item must not be accumulated, as determined by first using the
/// `peek` method to check, which is used to exclude escape characters from the
/// results of this crate's parsing.
///
/// After the `next_accum` method has been called and returned some item, the
/// `next` method should not be called before the `accum_done` method is called,
/// to avoid interfering with a pending accumulation.  If `next` is called in
/// this case, the pending accumulation will be silently dropped.
pub trait SourceStream<TT, DA>: Iterator<Item = SourceIterItem<TT::Pos>>
    where TT: TextConcat<DA>,
          DA: DatumAllocator<TT = TT> + ?Sized,
{
    /// TODO
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item>;

    /// TODO
    fn next_accum(&mut self, dalloc: &mut DA)
                  -> Result<Option<<Self as Iterator>::Item>,
                            AllocError>;

    /// TODO ... this is the primary constructor of the text types for the
    /// parsing
    fn accum_done(&mut self, dalloc: &mut DA) -> Result<TT, AllocError>;
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
/// chunk, which assists the use of this trait in this crate.
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
    type IterChunksState: TextIterChunksState<Chunk = Self::Chunk> + ?Sized;

    #[inline]
    fn from_str<'s>(s: &'s str) -> Self
        where Self::Chunk: From<&'s str>
    {
        Self::from(Self::Chunk::from(s))
    }

    fn partial_eq<O: Text>(&self, other: &O) -> bool {
        self.iter().map(sii_ch).eq(other.iter().map(sii_ch))
    }

    fn partial_cmp<O: Text>(&self, other: &O) -> Option<Ordering> {
        self.iter().map(sii_ch).partial_cmp(other.iter().map(sii_ch))
    }

    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().map(sii_ch).cmp(other.iter().map(sii_ch))
    }

    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState>;

    #[inline]
    fn iter_chunks<'a>(&'a self) -> TextChunksIter<'a, Self> {
        TextChunksIter::new(self)
    }

    /// Construct a new iterator, which is also a [`SourceStream`] if the `Self`
    /// type is also a [`TextConcat`], that yields the logical character
    /// sequence, and their positions, of the given `self`.
    ///
    /// The returned [`TextIter`] type is parameterized over the same lifetime
    /// as the borrows of `self` of calls of this method, which enables it to
    /// contain borrows derived from a `self` borrow, which is essential.
    ///
    /// This is how the correct lifetime relating is achieved without generic
    /// asssociated types.  If/when the `generic_associated_types` feature
    /// becomes available in stable Rust, our design should probably be redone
    /// to leverage that feature for a cleaner design.
    ///
    /// [`SourceStream`]: TODO
    /// [`TextConcat`]: TODO
    /// [`TextIter`]: TODO
    #[inline]
    fn iter<'a>(&'a self) -> TextIter<'a, Self> {
        TextIter::new(self)
    }
}

/// A [`Text`](trait.Text.html) that can logically concatenate its values,
/// optionally by using a provided [`DatumAllocator`](TODO).
///
/// Separating this concatenation functionality from the `Text` trait avoids
/// difficulties that otherwise would happen with needing to have the `DA:
/// DatumAllocator` type parameter where not really needed.
///
/// The `Datum` allocation support exists to support [`TextDatumSeq`](TODO), but
/// it hypothetically might be useful to other potential implementations.  The
/// `DA` type argument must be the same as that of the [`Parser`s](TODO) this is
/// used with.  When this is implemented for types that ignore the
/// `DatumAllocator`, the `DA` type should be a generic type parameter that
/// covers all (ignored) possibilities.
pub trait TextConcat<DA>: Text
    where DA: DatumAllocator<TT = Self> + ?Sized,
{
    /// Concatenate two `Text`s (of the same type) to form a single `Text` that
    /// logically represents this.  The `datum_alloc` argument may be ignored by
    /// some (most) implementations and exists only to support implementations
    /// like `TextDatumSeq`.  If the implementation ignores `datum_alloc`, it is
    /// safe to use `unwrap` on the returned `Result`.
    fn concat(self, other: Self, datum_alloc: &mut DA) -> Result<Self, AllocError>;
}

/// The basic interface common across both `Text`s and `TextChunk`s.  This
/// determines the associated type of the characters' positional information;
/// and this provides the ability to construct and check for emptiness.
// TODO: Impl indexing?  But probably not slicing since that seems like it'd
// require dynamic allocation for dealing with chunk boundaries, which for some
// impls like TextDatumSeq is not possible (because the standard slicing API
// isn't able to provide the needed DatumAllocator).
pub trait TextBase
    where Self: Sized,
{
    /// TODO
    type Pos: TextPosition;

    fn empty() -> Self;

    fn is_empty(&self) -> bool;
}

/// A sequence of characters that serves as a single chunk in the underlying
/// representation of some `Text` type.
pub trait TextChunk: TextBase {
    // FUTURE: Use `generic_associated_types` so this can have a lifetime
    // parameter.
    type CharsSrcStrm: TextChunkSourceStream<Self>;

    // FUTURE: Use `generic_associated_types` to enable having the same lifetime
    // in `CharsSrcStrm<'_>` as this method call's borrow of `self`.  This will
    // enable new possibilities of implementation such as multi-level chunking
    // with chunks which are themselves `Text` types composed of underlying
    // chunks, where a `CharsSrcStrm<'a>` is the `TextIter<'a>` of such types.
    // This will also enable chunk types backed by things like `String` which
    // need to return borrows related to the call lifetimes to be able to return
    // a `CharsSrcStrm`.
    fn src_strm<'a>(&'a self) -> Self::CharsSrcStrm;
}

/// Like [`SourceStream`](TODO), but without `DatumAllocator`, for `TextChunk`s.
/// Only accumulates within a single chunk, not across multiple chunks, unlike
/// `SourceStream`.  `TextIter as SourceStream` builds on this.
pub trait TextChunkSourceStream<C: TextChunk>:
              Iterator<Item = SourceIterItem<C::Pos>>
{
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item>;

    fn next_accum(&mut self) -> Option<<Self as Iterator>::Item>;

    fn accum_done(&mut self) -> C;
}

/// Iterator that yields a reference to each chunk of a `Text`, by using the
/// `TextIterChunksState` trait of the `Text`'s `IterChunksState`.
pub struct TextChunksIter<'l, TT>
    where TT: Text,
{
    state: Option<&'l TT::IterChunksState>,
}

// Note: Must implement `Copy` and `Clone` manually instead of using `derive`
// because `derive` would place additional bounds on the `TT` type parameter
// which must be avoided.

impl<'l, TT> Copy for TextChunksIter<'l, TT>
    where TT: Text
{}

impl<'l, TT> Clone for TextChunksIter<'l, TT>
    where TT: Text
{
    #[inline]
    fn clone(&self) -> Self { *self }
}

impl<'l, TT> TextChunksIter<'l, TT>
    where TT: Text,
{
    #[inline]
    fn new(text: &'l TT) -> Self {
        Self {
            state: text.iter_chunks_state(),
        }
    }
}

impl<'l, TT> Iterator for TextChunksIter<'l, TT>
    where TT: Text,
          TT::Chunk: 'l,
{
    type Item = &'l TT::Chunk;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.take()
                  .and_then(TextIterChunksState::next)
                  .map(|(next_chunk, next_state)| {
            self.state = next_state;
            next_chunk
        })
    }
}

/// Enables a `Text` to supply references to each of its chunks by borrowing
/// them from its associated `Text::IterChunksState`, which must implement this
/// trait, which is borrowed by calls to the `Text` methods, which enables the
/// lifetimes to be related for those methods to work.
pub trait TextIterChunksState {
    type Chunk: TextChunk;

    fn next(&self) -> Option<(&Self::Chunk, Option<&Self>)>;
}

impl<C> TextIterChunksState for [C]
    where C: TextChunk,
{
    type Chunk = C;

    fn next(&self) -> Option<(&Self::Chunk, Option<&Self>)> {
        if self.len() > 0 {
            Some((&self[0],
                  if self.len() > 1 {
                      Some(&self[1..])
                  } else {
                      None
                  }))
        } else {
            None
        }
    }
}


/// A [`SourceStream`] (which is also an `Iterator`) of the logical sequence of
/// characters (with positions) of any [`Text`], yielded as [`SourceIterItem`]
/// items.  This is designed to handle generic chunk-chain representations.
pub struct TextIter<'l, TT>
    where TT: Text,
{
    /// The `TextChunkSourceStream` (and so also `Iterator`) for the current
    /// chunk.
    cur_chunk_src_strm: Option<<TT::Chunk as TextChunk>::CharsSrcStrm>,
    /// `Iterator` of the next chunks.
    next_chunks_iter: TextChunksIter<'l, TT>,
    /// Accumulated chunks formed and concatenated by our
    /// `SourceStream::next_accum` and `SourceStream::accum_done`.
    accum: Option<TT>,
    /// Peeked next item of our `SourceStream::peek`.
    peeked: Option<SourceIterItem<TT::Pos>>,
}

impl<'l, TT> TextIter<'l, TT>
    where TT: Text,
{
    pub fn new(text: &'l TT) -> Self {
        let mut next_chunks_iter = text.iter_chunks();
        let cur_chunk_src_strm = next_chunks_iter.next().map(TextChunk::src_strm);
        Self {
            cur_chunk_src_strm,
            next_chunks_iter,
            accum: None,
            peeked: None,
        }
    }
}

/// All `Text` types' characters (with positions) can be iterated via a normal
/// `Iterator` trait regardless of whether they are `TextConcat`.
impl<'l, TT> Iterator for TextIter<'l, TT>
    where TT: Text,
          TT::Chunk: 'l,
{
    type Item = SourceIterItem<TT::Pos>;

    /// Note: If `next_accum` was previously called (to do an accumulation) and
    /// returned some item but `accum_done` was not called (to finish an
    /// accumulation), i.e. if we have an unfinished accumulation, this will
    /// abort and drop the unfinished accumulation.
    fn next(&mut self) -> Option<Self::Item> {
        self.accum = None;
        self.peeked = None;  // Note: Can't reuse. Must do below.
        loop {
            if let Some(ccss) = &mut self.cur_chunk_src_strm {
                if let it @ Some(_) = ccss.next() {
                    break it
                } else {
                    self.cur_chunk_src_strm
                        = self.next_chunks_iter.next().map(TextChunk::src_strm);
                }
            } else {
                break None
            }
        }
    }
}

/// `SourceStream` is only implemented where a `Text` type is a `TextConcat`.
impl<'l, TT, DA> SourceStream<TT, DA> for TextIter<'l, TT>
    where TT: TextConcat<DA>,
          TT::Chunk: 'l,
          DA: DatumAllocator<TT = TT> + ?Sized,
{
    /// TODO
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if let Some(ref it) = self.peeked {
            return Some(it)
        }
        // Must use a copy of the state to avoid changing `self`'s state because
        // that state affects the pending accumulation state which must not
        // change for a peek.
        let mut cur_chunk_src_strm;
        let mut cur_chunk_src_strm_ref = &mut self.cur_chunk_src_strm;
        let mut next_chunks_iter = self.next_chunks_iter; // copy
        loop {
            if let Some(ccss) = cur_chunk_src_strm_ref {
                if let Some(it) = ccss.peek() {
                    self.peeked = Some(it.clone());
                    break self.peeked.as_ref()
                } else {
                    cur_chunk_src_strm
                        = next_chunks_iter.next().map(TextChunk::src_strm);
                    cur_chunk_src_strm_ref = &mut cur_chunk_src_strm;
                }
            } else {
                break None
            }
        }
    }

    /// TODO
    fn next_accum(&mut self, dalloc: &mut DA)
                  -> Result<Option<<Self as Iterator>::Item>,
                            AllocError>
    {
        self.peeked = None;  // Note: Can't reuse. Must do below.
        Ok(loop {
            if let Some(ccss) = &mut self.cur_chunk_src_strm {
                if let it @ Some(_) = ccss.next_accum() {
                    break it
                } else {
                    let chunk_ended = ccss.accum_done().into();
                    self.accum = Some(self.accum.take().unwrap_or_else(TT::empty)
                                      .concat(chunk_ended, dalloc)?);
                    self.cur_chunk_src_strm
                        = self.next_chunks_iter.next().map(TextChunk::src_strm);
                }
            } else {
                break None
            }
        })
    }

    /// TODO
    fn accum_done(&mut self, dalloc: &mut DA) -> Result<TT, AllocError> {
        let mut accum = self.accum.take().unwrap_or_else(TT::empty);
        if let Some(ccss) = &mut self.cur_chunk_src_strm {
            let chunk_ended = ccss.accum_done().into();
            accum = accum.concat(chunk_ended, dalloc)?;
        }
        Ok(accum)
    }
}


/// A `TextPosition` type for character or slice values from text sources that
/// are entire `str` strings where we can know the original source and the
/// relative position.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct StrPos<'s> {
    /// Original entire source string that the value is in. Might equal the
    /// slice.
    pub src: &'s str,
    /// Byte position of start of the value, relative to `src`.
    pub byte_pos: usize,
    /// Character position of start of the value, relative to the original
    /// source.
    pub char_pos: usize,
}

impl<'s> TextPosition for StrPos<'s>
{
    #[inline]
    fn empty() -> Self {
        StrPos {
            src: "",
            byte_pos: 0,
            char_pos: 0,
        }
    }
}


/// A string slice representation that knows what position in its original
/// source it is at.
#[derive(Copy, Clone, Debug)]
pub struct PosStr<'s> {
    /// The represented string slice.
    pub val: &'s str,
    /// Information about `val`'s position relative to its original source.
    pub pos: StrPos<'s>,
}

impl<'s> PosStr<'s> {
    fn empty() -> PosStr<'s> {
        PosStr {
            val: "",
            pos: StrPos::empty(),
        }
    }
}

// TODO: Is 's2 needed or is lifetime subtyping automatic for this?
impl<'s1, 's2> From<&'s2 str> for PosStr<'s1>
    where 's2: 's1,
{
    fn from(val: &'s2 str) -> Self {
        Self {
            val,
            pos: StrPos {
                src: val,
                byte_pos: 0,
                char_pos: 0,
            },
        }
    }
}

impl<'s> TextBase for PosStr<'s> {
    type Pos = StrPos<'s>;

    #[inline]
    fn empty() -> Self {
        PosStr::empty()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.val.len() == 0
    }
}

impl<'s> TextChunk for PosStr<'s> {
    type CharsSrcStrm = PosStrIter<'s>;

    #[inline]
    fn src_strm(&self) -> Self::CharsSrcStrm {
        PosStrIter::new(self)
    }
}

pub struct PosStrIter<'s> {
    pei_iter: Peekable<Map<Zip<Enumerate<CharIndices<'s>>,
                               Repeat<StrPos<'s>>>,
                           fn(((usize, (usize, char)), StrPos<'s>))
                              -> SourceIterItem<StrPos<'s>>>>,
    accum: Option<PosStr<'s>>,
    posstr: PosStr<'s>,
}

impl<'s> PosStrIter<'s> {
    fn new(posstr: &PosStr<'s>) -> Self {
        Self {
            pei_iter: posstr.val.char_indices()
                                .enumerate()
                                .zip(iter::repeat(posstr.pos))
                                .map((|((char_pos, (byte_pos, ch)), mut pos)| {
                                    pos.byte_pos += byte_pos;
                                    pos.char_pos += char_pos;
                                    SourceIterItem {
                                        ch,
                                        pos,
                                    }
                                }) as fn(((usize, (usize, char)), StrPos<'s>))
                                         -> SourceIterItem<StrPos<'s>>)
                                .peekable(),
            accum: None,
            posstr: *posstr,
        }
    }
}

impl<'s> Iterator for PosStrIter<'s> {
    type Item = SourceIterItem<StrPos<'s>>;

    /// Note: If `next_accum` was previously called (to do an accumulation) and
    /// returned some item but `accum_done` was not called (to finish an
    /// accumulation), i.e. if we have an unfinished accumulation, this will
    /// abort and drop the unfinished accumulation.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.accum = None;
        self.pei_iter.next()
    }
}

impl<'s> TextChunkSourceStream<PosStr<'s>> for PosStrIter<'s>
{
    #[inline]
    fn peek(&mut self) -> Option<&SourceIterItem<StrPos<'s>>> {
        self.pei_iter.peek()
    }

    fn next_accum(&mut self) -> Option<SourceIterItem<StrPos<'s>>> {
        let next = self.pei_iter.next();
        if let Some(next) = &next {
            let end = if let Some(peek) = self.pei_iter.peek() {
                peek.pos.byte_pos
            } else {
                self.posstr.pos.byte_pos + self.posstr.val.len()
            };
            if let Some(accum) = &mut self.accum {
                // Already set, so extend
                accum.val = &accum.pos.src[accum.pos.byte_pos .. end];
            } else {
                // Not set yet, so set
                self.accum = Some(PosStr{
                    val: &next.pos.src[next.pos.byte_pos .. end],
                    pos: next.pos,
                });
            }
        }
        next
    }

    #[inline]
    fn accum_done(&mut self) -> PosStr<'s> {
        self.accum.take().unwrap_or_else(PosStr::empty)
    }
}


/// A chunk-chain representation of texts that uses [`Datum`]s allocated by a
/// [`Parser`] as nodes in a linked list of text chunks.  The chunk type can be
/// any [`TextChunk`] type.  This is useful when heap allocation isn't available
/// (or desired) and the `Parser`'s `DatumAllocator` is the only available (or
/// desired) dynamic allocator.
///
/// TODO: Explain why DatumMutRef is used.
#[derive(Debug)]
pub struct TextDatumSeq<'d, C, ET> {
    chunk: C,
    next: Option<DatumMutRef<'d, Self, ET>>,
}

impl<'d, C, ET> From<C> for TextDatumSeq<'d, C, ET>
    where C: TextChunk,
{
    #[inline]
    fn from(chunk: C) -> Self {
        Self {
            chunk,
            next: None,
        }
    }

}

impl<'d, TT, C, ET> PartialEq<TT> for TextDatumSeq<'d, C, ET>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn eq(&self, other: &TT) -> bool {
        Text::partial_eq(self, other)
    }
}

impl<'d, C, ET> Eq for TextDatumSeq<'d, C, ET>
    where C: TextChunk,
{}

// TODO: PartialOrd, Ord

impl<'d, C, ET> TextIterChunksState for TextDatumSeq<'d, C, ET>
    where C: TextChunk,
{
    type Chunk = C;

    fn next(&self) -> Option<(&Self::Chunk, Option<&Self>)> {
        Some((&self.chunk,
              self.next.as_ref().map(
                  |datum_ref|
                  if let Datum::Text(next) = Deref::deref(datum_ref) {
                      next
                  } else {
                      // Note: This won't ever fail because we always construct the
                      // `Datum::Text` variant.
                      unreachable!()
                  })))
    }
}

impl<'d, C, ET> TextBase for TextDatumSeq<'d, C, ET>
    where C: TextChunk,
{
    type Pos = C::Pos;

    #[inline]
    fn empty() -> Self {
        Self::from(C::empty())
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.iter_chunks().all(TextBase::is_empty)
    }
}

impl<'d, C, ET> Text for TextDatumSeq<'d, C, ET>
    where C: TextChunk,
{
    type Chunk = C;
    type IterChunksState = Self;

    #[inline]
    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState> {
        Some(self)
    }
}

impl<'d, DA, C, ET> TextConcat<DA> for TextDatumSeq<'d, C, ET>
    where C: TextChunk,
          DA: DatumAllocator<TT = Self, ET = ET, DR = DatumMutRef<'d, Self, ET>>
              + ?Sized,
{
    /// Link two `TextDatumSeq`s to form a single `TextDatumSeq` that represents
    /// their logical concatenation.  Unlike most implementations of
    /// `TextConcat`, this does use the `datum_alloc` argument to allocate the
    /// new `Datum`s that are used as the storage of the nodes of our
    /// linked-list approach.
    fn concat(mut self, other: Self, datum_alloc: &mut DA) -> Result<Self, AllocError>
    {
        // If either is empty, optimize.
        if self.is_empty() {
            return Ok(other);
        } else if other.is_empty() {
            return Ok(self)
        }

        // Find the end of the linked-list and link `other` from it.
        let mut cur = &mut self;
        loop {
            match &mut cur.next {
                Some(dr) =>
                    if let Some(Datum::Text(next)) = DerefTryMut::get_mut(dr) {
                        cur = next;
                    } else {
                        // Note: This won't ever fail because we never share the
                        // datum references and always construct the
                        // `Datum::Text` variant.
                        unreachable!()
                    }
                last_next @ None => {
                    *last_next = Some(datum_alloc.new_datum(Datum::Text(other))?);
                    break Ok(self)
                }
            }
        }
    }
}


/// The abstract syntax tree (AST) type returned by parsing.  It is extensible
/// by the `ExtraType` parameter, and it is parameterized over the `DatumRef`
/// type used to refer to the other `Datum`s in an AST.  It can also be used for
/// DAGs.
#[derive(Copy, Clone, Eq, Debug)]
pub enum Datum<TextType, ExtraType, DatumRef>
    where DatumRef: DerefTryMut<Target = Datum<TextType, ExtraType, DatumRef>>,
{
    /// A logically unbroken span of text. (Only nest forms break text
    /// logically, but escape characters break the representation into chunks.)
    Text(TextType),
    /// A nest form that is not empty and so has a non-empty "operator"/"head"
    /// sub-form, as a `Datum`, and has a possibly-empty "operands" sub-form(s),
    /// as a `List` (or `EmptyList`) or a `Text`.
    Combination{operator: DatumRef, operands: DatumRef},
    /// An empty nest form
    EmptyNest,
    /// A list of other `Datum`s. Used to represent parsed operands.
    List{elem: DatumRef, next: DatumRef},
    /// An empty list
    EmptyList,
    /// Extensibility that custom macros/combiners may utilize to add additional
    /// variants
    Extra(ExtraType),
}


/// This allows different concrete [`Datum`](enum.Datum.html) types to be
/// compared with each other for equality if their ["extra"
/// types](enum.Datum.html#variant.Extra) can be.  This also avoids stack
/// overflows for long lists and deep nests (but can still overflow on other
/// deep tree shapes, but those are rare).
impl<TT1, TT2, ET1, ET2, DR1, DR2>
    PartialEq<Datum<TT2, ET2, DR2>>
    for Datum<TT1, ET1, DR1>
    where DR1: DerefTryMut<Target = Datum<TT1, ET1, DR1>>,
          DR2: DerefTryMut<Target = Datum<TT2, ET2, DR2>>,
          TT1: PartialEq<TT2>,
          ET1: PartialEq<ET2>,
{
    fn eq(&self, other: &Datum<TT2, ET2, DR2>) -> bool {
        let (mut slf, mut oth) = (self, other);
        loop {
            match (slf, oth) {
                (Text(txt1), Text(txt2))
                    => break *txt1 == *txt2,
                (Combination{operator: rtr1, operands: rnds1},
                 Combination{operator: rtr2, operands: rnds2})
                    => if **rnds1 == **rnds2 {
                        slf = &**rtr1;
                        oth = &**rtr2;
                    } else { break false },
                (EmptyNest, EmptyNest)
                    => break true,
                (List{elem: e1, next: n1}, List{elem: e2, next: n2})
                    => if **e1 == **e2 {
                        slf = &**n1;
                        oth = &**n2;
                    } else { break false },
                (EmptyList, EmptyList)
                    => break true,
                (Extra(et1), Extra(et2))
                    => break et1 == et2,
                _
                    => break false
            }
        }
    }
}

/// Exists to be used similarly to but differently than
/// [`DerefMut`](http://doc.rust-lang.org/std/ops/trait.DerefMut.html) so that
/// types like [`Rc`](http://doc.rust-lang.org/std/rc/struct.Rc.html) and its
/// [`get_mut`](http://doc.rust-lang.org/std/rc/struct.Rc.html#method.get_mut)
/// method can be used to hold [`Datum`s](enum.Datum.html).  `DerefMut` must
/// never fail, so it can't be used.  We want mutability of `Datum`s so that we
/// can construct [lists of them](enum.Datum.html#variant.List) using only the
/// space of the values allocated by a `Parser`'s
/// [`DatumAllocator`](trait.DatumAllocator.html), since this crate is intended
/// to be usable in `no_std` environments which don't provide heap allocation.
///
/// The alternative of using tree-recursion to acheive temporary stack
/// allocation for constructing the lists without mutation is not good because
/// the stack is too small for large lists and it could realistically be
/// exceeded (causing a crash).  While `no_std` environments might also have
/// restricted limits to the amounts of `Datum`s that could be allocated, with
/// our approach they can control the limit and can receive an
/// [`Error`](enum.Error.html#variant.AllocExhausted) value under their control
/// if the limit is exceeded (instead of a crash).
pub trait DerefTryMut: Deref
    where <Self as Deref>::Target: Sized,
{
    /// Returns a mutable reference to the inner `Datum` if it can.  Otherwise,
    /// returns `None`.  Some implementations may never return `None` (e.g. for
    /// types that also implement `DerefMut`).
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target>;
}

/// This assists in basic direct mutable borrow references being used as the
/// `Datum` reference type.
pub type MutRefDatum<'d, TT, ET> = Datum<TT, ET, DatumMutRef<'d, TT, ET>>;

/// This wrapper allows the needed recursive type definition for basic direct
/// mutable borrow references to be used as the `Datum` reference type.
#[derive(PartialEq, Eq, Debug)]
pub struct DatumMutRef<'d, TT, ET>(pub &'d mut MutRefDatum<'d, TT, ET>);

impl<'d, TT, ET> Deref for DatumMutRef<'d, TT, ET> {
    type Target = MutRefDatum<'d, TT, ET>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'d, TT, ET> DerefMut for DatumMutRef<'d, TT, ET> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// This allows basic direct mutable borrow references to be used as the `Datum`
/// reference type.
impl<'d, TT, ET> DerefTryMut for DatumMutRef<'d, TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(DerefMut::deref_mut(this))
    }
}


// TODO: Exercise combiners in the test suite

/// This module is needed so that the traits are public, as required by
/// `Combiner`, but not exported from the crate.
mod combiner {
    pub trait OperativeTrait { }
    pub trait ApplicativeTrait { }
}

use self::combiner::*;

/// A macro function, bound to an operator sub-form, which is called with the
/// operands sub-form(s) to determine what should be substituted for the whole
/// form.  The `OperativeRef` and `ApplicativeRef` type parameters determine the
/// types used to refer to the functions.
///
/// While these parameters as defined here can allow possibly inconsistent
/// types, further bounds on these are required by a `Parser`'s
/// [`OperatorBindings`](trait.OperatorBindings.html) which ensures that only
/// consistent ones can be used with it, which is the only intended use of this
/// type.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Combiner<OperativeRef, ApplicativeRef>
    where OperativeRef: DerefMut,
          OperativeRef::Target: OperativeTrait,
          ApplicativeRef: DerefMut,
          ApplicativeRef::Target: ApplicativeTrait,
{
    Operative(OperativeRef),
    Applicative(ApplicativeRef),
}

impl<TT, ET, DR, POS, CE> OperativeTrait for OpFn<TT, ET, DR, POS, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{ }

impl<TT, ET, DR, POS, CE> ApplicativeTrait for ApFn<TT, ET, DR, POS, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{ }

/// The type of "operative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-form as
/// a `Datum::Text` containing the unparsed operands text; and the third
/// argument is the allocator state which can be used if constructing new
/// `Datum`s is needed for the return value.  See
/// [`CombinerResult`](type.CombinerResult.html) for the description of the
/// return value.
pub type OpFn<TT, ET, DR, POS, CE>
    = dyn FnMut(DR, DR)
                -> CombinerResult<TT, ET, DR, POS, CE>;

/// The type of "applicative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-forms
/// as a `Datum::List` containing the recursively parsed operands as separate
/// `Datum`s, or it is a `Datum::EmptyList` if the operands text was empty; and
/// the third argument is the allocator state which can be used if constructing
/// new `Datum`s is needed for the return value.  See
/// [`CombinerResult`](type.CombinerResult.html) for the description of the
/// return value.
pub type ApFn<TT, ET, DR, POS, CE>
    = dyn FnMut(DR, DR)
                -> CombinerResult<TT, ET, DR, POS, CE>;

/// The type returned by "operative" and "applicative" functions.  For a
/// successful return, the returned `Datum` is substituted for the original form
/// by the parser in the AST it yields, and the returned allocator state is the
/// possibly-updated state passed into the combiner function.  An
/// [`Error`](enum.Error.html) is returned if the combiner fails for any reason.
pub type CombinerResult<TT, ET, DR, POS, CE>
    = Result<Datum<TT, ET, DR>, Error<POS, CE>>;


/// Represents: the ability to parse a string; the characters used to delimit
/// the nesting form; the method of allocating the `Datum`s; and the environment
/// of bindings of macros.
pub struct Parser<CC, DA, OB> {
    pub classifier: CC,
    pub allocator: DA,
    pub bindings: OB,
}

impl<CC, DA, OB> Parser<CC, DA, OB>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
{
    /// The primary method.  Parse the given text source, according to the
    /// specific parameterization of our `Self`, and return an iterator that
    /// yields each top-level form as a `Datum` AST.
    pub fn parse<'p, S>(&'p mut self, source: S) -> ParseIter<'p, Self, S>
        where S: SourceStream<DA::TT, DA>,
    {
        ParseIter::new(self, source)
    }
}

/// TODO
pub trait CharClassifier {
    /// Predicate that determines the character(s) used to delimit the start of
    /// our nesting form.
    fn is_nest_start(&self, c: char) -> bool;

    /// Predicate that determines the character(s) used to delimit the end of
    /// our nesting form.
    fn is_nest_end(&self, c: char) -> bool;

    /// Predicate that determines the character(s) used to escape the delimiter
    /// characters of our nesting form.
    fn is_nest_escape(&self, c: char) -> bool;

    /// Predicate that determines the character(s) considered to be whitespace,
    /// which affects the delimiting of operator and operands in our nesting
    /// form.
    fn is_whitespace(&self, c: char) -> bool;
}

/// A [`CharClassifier`](trait.CharClassifier.html) that uses the common `{`,
/// `}`, and `\` characters and the Unicode whitespace property.
pub struct DefaultCharClassifier;

impl CharClassifier for DefaultCharClassifier {
    #[inline]
    fn is_nest_start(&self, c: char) -> bool {
        '{' == c
    }

    #[inline]
    fn is_nest_end(&self, c: char) -> bool {
        '}' == c
    }

    #[inline]
    fn is_nest_escape(&self, c: char) -> bool {
        '\\' == c
    }

    #[inline]
    fn is_whitespace(&self, c: char) -> bool {
        c.is_whitespace()
    }
}

/// TODO
pub trait DatumAllocator {
    /// The [`Text` type](enum.Datum.html#variant.Text) for our `Datum` type. It
    /// must be a [`TextConcat`] for our `Self` so it supports concatenation
    /// which the parsing requires.
    type TT: TextConcat<Self>;
    /// The ["extra" type](enum.Datum.html#variant.Extra) for our `Datum` type.
    type ET;
    /// The type of references to [`Datum`s](enum.Datum.html) yielded by our
    /// parsing.
    type DR: DerefTryMut<Target = Datum<Self::TT, Self::ET, Self::DR>>;

    /// Allocate a fresh [`Datum`](enum.Datum.html), in whatever way the
    /// particular implementation wants, and set its initial value to that of
    /// the `from` argument.  An [`AllocError`](enum.AllocError.html) is
    /// returned if allocation fails for any reason.
    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>;
}

/// TODO
pub trait OperatorBindings<DA>
    where DA: DatumAllocator,
{
    /// The type of references to
    /// [`Operative`](enum.Combiner.html#variant.Operative) macro functions.
    type OR: DerefMut<Target = OpFn<DA::TT, DA::ET, DA::DR,
                                    <DA::TT as TextBase>::Pos,
                                    Self::CE>>;
    /// The type of references to
    /// [`Applicative`](enum.Combiner.html#variant.Applicative) macro functions.
    type AR: DerefMut<Target = ApFn<DA::TT, DA::ET, DA::DR,
                                    <DA::TT as TextBase>::Pos,
                                    Self::CE>>;
    /// The [combiner error extension](enum.Error.html#variant.FailedCombiner)
    /// type.
    type CE;

    /// Look-up any binding we might have associated with the given datum,
    /// referenced by the `operator` argument, which was found in operator
    /// (first, "head") position of a nested form.  If we do have a binding for
    /// it, return the "combiner" function that determines the semantics of the
    /// entire form and further parses and processes it in possibly arbitrary
    /// ways.  Else if we do not have a binding, return `None` to indicate that
    /// the form should not be handled according to the operator and that the
    /// operands should simply be recursively parsed.
    fn lookup(&mut self, operator: &DA::DR) -> Option<Combiner<Self::OR, Self::AR>>;
}

/// An [`OperatorBindings`](trait.OperatorBindings.html) that always has no
/// bindings and its [`lookup`](trait.OperatorBindings.html#tymethod.lookup)
/// method always returns `None`.
pub struct EmptyOperatorBindings;

// TODO: Have this be `pub` only in a module and not exported.
/// Trick `OperatorBindings` into accepting this for the implementation of
/// it for `EmptyOperatorBindings`.
pub struct DummyCombiner<TT, ET, DR, POS, CE>(TT, ET, DR, POS, CE);

impl<TT, ET, DR, POS, CE> Deref for DummyCombiner<TT, ET, DR, POS, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{
    type Target = dyn FnMut(DR, DR) -> CombinerResult<TT, ET, DR, POS, CE>;
    fn deref(&self) -> &Self::Target { unreachable!() }
}

impl<TT, ET, DR, POS, CE> DerefMut for DummyCombiner<TT, ET, DR, POS, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{
    fn deref_mut(&mut self) -> &mut Self::Target { unreachable!() }
}

impl<DA> OperatorBindings<DA> for EmptyOperatorBindings
    where DA: DatumAllocator,
{
    type OR = DummyCombiner<DA::TT, DA::ET, DA::DR, <DA::TT as TextBase>::Pos, Self::CE>;
    type AR = DummyCombiner<DA::TT, DA::ET, DA::DR, <DA::TT as TextBase>::Pos, Self::CE>;
    type CE = ();

    #[inline]
    fn lookup(&mut self, _operator: &DA::DR) -> Option<Combiner<Self::OR, Self::AR>> {
        None
    }
}


/// The type of values given by the parser iterator
pub type ParseIterItem<DR, POS, CE> = Result<DR, Error<POS, CE>>;

/// An [`Iterator`](http://doc.rust-lang.org/std/iter/trait.Iterator.html) that
/// parses its input text one top-level form at a time per each call to
/// [`next`](http://doc.rust-lang.org/std/iter/trait.Iterator.html#tymethod.next),
/// and yields a [`Datum`](enum.Datum.html) AST for each or an
/// [`Error`](enum.Error.html), according to the given
/// [`Parser`](struct.Parser.html)'s parameterization.
pub struct ParseIter<'p, Prsr, SrcStrm>
    where Prsr: ?Sized + 'p,
{
    parser: &'p mut Prsr,
    src_strm: SrcStrm,
    nest_depth: usize,
}

impl<'p, CC, DA, OB, S>
    Iterator
    for ParseIter<'p, Parser<CC, DA, OB>, S>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
          Parser<CC, DA, OB>: 'p,
          S: SourceStream<DA::TT, DA>,
{
    type Item = ParseIterItem<DA::DR, <DA::TT as TextBase>::Pos, OB::CE>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.do_next() {
            Ok(Some(dr)) => Some(Ok(dr)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ParseTextMode {
    Base,
    Operator,
    Operands,
}

impl<'p, CC, DA, OB, S>
    ParseIter<'p, Parser<CC, DA, OB>, S>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
          Parser<CC, DA, OB>: 'p,
          S: SourceStream<DA::TT, DA>,
{
    // TODO: Can `type = ...` be used here for shorter aliases of long ones below?

    #[inline]
    fn new(parser: &'p mut Parser<CC, DA, OB>, src_strm: S) -> Self {
        Self {
            parser,
            src_strm,
            nest_depth: 0,
        }
    }

    #[inline]
    fn do_next(&mut self) -> Result<Option<DA::DR>,
                                    Error<<DA::TT as TextBase>::Pos, OB::CE>>
    {
        self.parse_next(ParseTextMode::Base)
    }

    fn parse_next(&mut self, mode: ParseTextMode)
                  -> Result<Option<DA::DR>,
                            Error<<DA::TT as TextBase>::Pos, OB::CE>>
    {
        // Peek some next char for below, or abort appropriately if none.
        let ch = match self.src_strm.peek() {
            Some(&SourceIterItem{ch, ..}) => ch,
            None =>
                return if self.nest_depth == 0 {
                    Ok(None)
                } else {
                    Err(Error::MissingEndChar)
                }
        };
        // Start of a nest, either a combination or an empty nest. Parse it to
        // its end and return it.
        if self.parser.classifier.is_nest_start(ch) {
            self.incr_nest_depth();
            let result = self.parse_nested();
            self.decr_nest_depth();
            result.map(Some)
        }
        // End of a nest, or error. Don't parse nor return an item, only check
        // validity.
        else if self.parser.classifier.is_nest_end(ch) {
            self.check_end_char().map(|_| None)
        }
        // Start of a text. Parse it to its end and return it.
        else {
            self.parse_text(mode).map(Some)
        }
    }

    fn parse_text(&mut self, mode: ParseTextMode)
                  -> Result<DA::DR,
                            Error<<DA::TT as TextBase>::Pos, OB::CE>>
    {
        #[inline]
        fn is_end_char<CC>(ch: char, chclass: &CC, mode: ParseTextMode) -> bool
            where CC: CharClassifier,
        {
            match mode {
                ParseTextMode::Base
                    => chclass.is_nest_start(ch),
                ParseTextMode::Operator
                    => chclass.is_whitespace(ch)
                    || chclass.is_nest_start(ch)
                    || chclass.is_nest_end(ch),
                ParseTextMode::Operands
                    => chclass.is_nest_end(ch),
            }
        }

        // Giving our allocator to the accumulation calls below enables them to
        // have the option of using new `Datum`s to achieve text-chunking for
        // the breaking around, and excluding of, escape characters.  While most
        // implementations will ignore the allocator (e.g. to instead use heap
        // allocation), this unusual support is essential for `TextDatumSeq` (or
        // similar) which is intended for use in constrained environments
        // without heap allocation where reusing our `Datum` allocation ability
        // (e.g. from a stack array) is desired.

        let mut text = DA::TT::empty();
        macro_rules! concat_accum {
            () => {
                let accum = self.src_strm.accum_done(&mut self.parser.allocator)?;
                text = text.concat(accum, &mut self.parser.allocator)?;
            }
        }

        let mut nest_level: usize = 0;

        loop {
            match self.src_strm.peek() {
                Some(&SourceIterItem{ch, ..}) => {
                    // Reached end. Do not consume peeked end char
                    if nest_level == 0 && is_end_char(ch, &self.parser.classifier, mode)
                    {
                        break;
                    }
                    // Accumulate escaped char whatever it might be, but not the
                    // escape char
                    else if self.parser.classifier.is_nest_escape(ch) {
                        concat_accum!(); // Break chunk before escape char
                        self.src_strm.next(); // Skip peeked escape char first
                        self.src_strm.next_accum(&mut self.parser.allocator)?;
                    }
                    // Start of nest. Track nesting depth
                    else if self.parser.classifier.is_nest_start(ch) {
                        // Accumulate peeked
                        self.src_strm.next_accum(&mut self.parser.allocator)?;
                        nest_level += 1;
                    }
                    // End of nest. Check balanced nesting
                    else if self.parser.classifier.is_nest_end(ch) {
                        if nest_level > 0 {
                            // Accumulate peeked
                            self.src_strm.next_accum(&mut self.parser.allocator)?;
                            nest_level -= 1;
                        } else {
                            self.check_end_char()?;
                            break;
                        }
                    }
                    // Accumulate peeked
                    else {
                        self.src_strm.next_accum(&mut self.parser.allocator)?;
                    }
                },
                // End of source stream
                None => {
                    break;
                }
            }
        }
        // Done. Return what we accumulated. Or error if unbalanced nesting.
        if nest_level == 0 {
            concat_accum!();
            Ok(self.parser.allocator.new_datum(Datum::Text(text))?)
        } else {
            Err(Error::MissingEndChar)
        }
    }

    fn parse_nested(&mut self) -> Result<DA::DR,
                                         Error<<DA::TT as TextBase>::Pos, OB::CE>>
    {
        let end = |slf: &mut Self| {
            // Consume our nest's end char. A missing end char is possible, but
            // an erroneous non-end char shouldn't be.
            if let Some(SourceIterItem{ch, ..}) = slf.src_strm.next() {
                debug_assert!(slf.parser.classifier.is_nest_end(ch));
                Ok(())
            } else {
                Err(Error::MissingEndChar)
            }
        };

        // Advance past nest start char.
        let start = self.src_strm.next();
        debug_assert_eq!(start.map(|SourceIterItem{ch, ..}|
                                   self.parser.classifier.is_nest_start(ch)),
                         Some(true));
        // Skip any leading whitespace before head form.
        self.skip_whitespace();
        // Parse form in operator position, or empty.
        let operator = self.parse_next(ParseTextMode::Operator)?;
        // If operator delimited by following whitespace, advance past first
        // whitespace char.
        if let Some(&SourceIterItem{ch, ..}) = self.src_strm.peek() {
            if self.parser.classifier.is_whitespace(ch) { self.src_strm.next(); }
        }
        // Determine the result.
        let result = match operator {
            // Parse the operands according to the operator.
            Some(operator) => match self.parser.bindings.lookup(&operator) {
                // Operator is bound to a combiner macro which will process the
                // operands and determine the return value.
                Some(combiner) => match combiner {
                    // Operatives are given the operands text unparsed to do
                    // whatever they want with it.
                    Operative(mut opr) => {
                        let operands = self.parse_text(ParseTextMode::Operands)?;
                        end(self)?;
                        opr.deref_mut()(operator, operands)?
                    },
                    // Applicatives are given the recursive parse of the
                    // operands text as a list of "arguments".
                    Applicative(mut apl) => {
                        let arguments = self.parse_all(ParseTextMode::Base)?;
                        end(self)?;
                        apl.deref_mut()(operator, arguments)?
                    }
                },
                // Not bound, so simply recursively parse operands and return a
                // value representing the "combination" of operator and operands
                // forms.
                None => {
                    let operands = self.parse_all(ParseTextMode::Base)?;
                    end(self)?;
                    Combination{operator, operands}
                },
            }
            // No operator nor operands. Empty nest form.
            None => {
                end(self)?;
                EmptyNest
            }
        };
        // Return result as newly allocated `Datum`.
        Ok(self.parser.allocator.new_datum(result)?)
    }

    fn parse_all(&mut self, mode: ParseTextMode)
                 -> Result<DA::DR,
                           Error<<DA::TT as TextBase>::Pos, OB::CE>>
    {
        let mut head = self.parser.allocator.new_datum(EmptyList)?;
        let mut tail = &mut head;
        loop {
            let it = self.parse_next(mode)?;
            if let Some(next_it) = it {
                if let Some(d) = DerefTryMut::get_mut(tail) {
                    let rest = self.parser.allocator.new_datum(EmptyList)?;
                    *d = List{elem: next_it, next: rest};
                    match d {
                        List{next, ..} => tail = next,
                        _ => unreachable!()
                    }
                } else {
                    return Err(Error::FailedDerefTryMut);
                }
            } else {
                break;
            }
        }
        Ok(head)
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        let chclass = &self.parser.classifier;
        while self.src_strm.peek()
                           .map(|&SourceIterItem{ch, ..}| chclass.is_whitespace(ch))
                           .unwrap_or(false)
        {
            self.src_strm.next(); // Skip peeked whitespace char
        }
    }

    #[inline]
    fn check_end_char(&mut self) -> Result<(), Error<<DA::TT as TextBase>::Pos, OB::CE>>
    {
        {
            let chclass = &self.parser.classifier;
            debug_assert_eq!(self.src_strm.peek().map(|&SourceIterItem{ch, ..}|
                                                      chclass.is_nest_end(ch)),
                             Some(true));
        }
        if self.nest_depth > 0 {
            // Valid end of nest. Do not consume peeked char.
            Ok(())
        } else {
            // Invalid unbalanced nest end character. Consume peeked char, to
            // allow the possibility that this iterator could be resumed
            // again. Also, use its `pos` in the error. This `unwrap` will never
            // fail because we already did `peek` and know there is a next.
            let n = self.src_strm.next().unwrap();
            Err(Error::UnbalancedEndChar(n.pos))
        }
    }

    #[inline]
    fn incr_nest_depth(&mut self) {
        self.nest_depth += 1;
    }

    #[inline]
    fn decr_nest_depth(&mut self) {
        self.nest_depth -= 1;
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    /// Used as a "text" type in tests where it does not need to be a real
    /// `Text`.
    #[derive(Copy, Clone, PartialEq, Debug)]
    struct DummyText;

    #[test]
    fn datum_equality_same() {
        use super::Datum::*;

        assert_eq!(Text::<_, (), DatumMutRef<_, ()>>(DummyText),
                   Text::<_, (), DatumMutRef<_, ()>>(DummyText));

        assert_eq!(Combination::<DummyText, (), DatumMutRef<_, ()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)},
                   Combination::<DummyText, (), DatumMutRef<_, ()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyNest::<DummyText, (), DatumMutRef<_, ()>>,
                   EmptyNest::<DummyText, (), DatumMutRef<_, ()>>);

        assert_eq!(List::<DummyText, (), DatumMutRef<_, ()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)},
                   List::<DummyText, (), DatumMutRef<_, ()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyList::<DummyText, (), DatumMutRef<_, ()>>,
                   EmptyList::<DummyText, (), DatumMutRef<_, ()>>);

        assert_eq!(Extra::<DummyText, (), DatumMutRef<_, ()>>(()),
                   Extra::<DummyText, (), DatumMutRef<_, ()>>(()));

        // TODO: More cases, including !=
    }

    mod datumref {
        use super::*;

        #[derive(Copy, Clone, Debug)]
        pub struct DatumRef<'d, TT, ET>(pub &'d Datum<TT, ET, DatumRef<'d, TT, ET>>);

        impl<'d, TT, ET> Deref for DatumRef<'d, TT, ET>
        {
            type Target = Datum<TT, ET, DatumRef<'d, TT, ET>>;

            fn deref(&self) -> &Self::Target {
                self.0
            }
        }

        impl<'d, TT, ET> DerefTryMut for DatumRef<'d, TT, ET>
        {
            fn get_mut(_this: &mut Self) -> Option<&mut Self::Target> {
                None
            }
        }
    }

    #[test]
    fn datum_equality_diff_ref() {
        use super::Datum::*;
        use self::datumref::DatumRef;

        assert_eq!(Text::<_, (), DatumMutRef<_, ()>>(DummyText),
                   Text::<_, (), DatumRef<_, ()>>(DummyText));

        assert_eq!(Combination::<DummyText, (), DatumMutRef<_, ()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)},
                   Combination::<DummyText, (), DatumRef<_, ()>>{
                       operator: DatumRef(&EmptyNest),
                       operands: DatumRef(&EmptyList)});

        assert_eq!(EmptyNest::<DummyText, (), DatumMutRef<_, ()>>,
                   EmptyNest::<DummyText, (), DatumRef<_, ()>>);

        assert_eq!(List::<DummyText, (), DatumMutRef<_, ()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)},
                   List::<DummyText, (), DatumRef<_, ()>>{
                       elem: DatumRef(&EmptyNest),
                       next: DatumRef(&EmptyList)});

        assert_eq!(EmptyList::<DummyText, (), DatumMutRef<_, ()>>,
                   EmptyList::<DummyText, (), DatumRef<_, ()>>);

        assert!(Extra::<DummyText, (), DatumMutRef<_, ()>>(())
                == Extra::<DummyText, (), DatumRef<_, ()>>(()));
    }

    #[test]
    fn datum_copy_clone() {
        use super::Datum::*;
        use self::datumref::DatumRef;

        let a = List::<DummyText, (), DatumRef<_, ()>>{
            elem: DatumRef(&EmptyNest::<_, (), DatumRef<_, ()>>),
            next: DatumRef(&EmptyList::<_, (), DatumRef<_, ()>>)};
        let b = a;
        assert_eq!(a, b);

        let c = List::<DummyText, (), DatumRef<_, ()>>{
            elem: DatumRef(&EmptyNest::<_, (), DatumRef<_, ()>>),
            next: DatumRef(&EmptyList::<_, (), DatumRef<_, ()>>)};
        let d = c.clone();
        assert_eq!(c, d);
    }

    #[test]
    fn error_equality() {
        use super::Error::*;

        assert_eq!(UnbalancedEndChar::<_, ()>(()),
                   UnbalancedEndChar::<_, ()>(()));

        assert_eq!(MissingEndChar::<(), ()>, MissingEndChar::<(), ()>);

        assert_eq!(FailedAlloc::<(), ()>(AllocError::AllocExhausted),
                   FailedAlloc::<(), ()>(AllocError::AllocExhausted));

        assert_eq!(FailedDerefTryMut::<(), ()>, FailedDerefTryMut::<(), ()>);

        assert_eq!(FailedCombiner::<(), i32>(1), FailedCombiner::<(), i32>(1));
    }

    // TODO: Move these text/chunking tests to modules along with the definitions

    #[test]
    fn strpos() {
        assert_eq!(StrPos::empty(), StrPos{src: "", byte_pos: 0, char_pos: 0});
    }
}
