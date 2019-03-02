//! A `TextChunk` implementation for boxed (heap allocated) string values used
//! distinctly as chunks.  This is useful for strings produced from streaming
//! sources that buffered into one or more strings separated at possibly
//! arbitrary points.

use std::{rc::Rc, sync::Arc, ops::Range};

use crate::{
    SourcePosition, SourceIterItem, TextBase, TextChunk,
    text,
};


/// A `SourcePosition` type for characters or string chunks as the position of a
/// `char` relative to its original source.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct CharPos(pub usize);

impl SourcePosition for CharPos {
    #[inline] fn empty() -> Self { Self(0) }
}


/// Make `AsStr` inaccessible while still being `pub` as needed by our public
/// interfaces.
mod as_str {
    /// Enables generically using various compositions of boxed string types of
    /// arbitrary levels of indirection.  E.g. `Rc<String>`, `Arc<Box<str>>`,
    /// `Rc<str>`, or even `Rc<Box<Arc<Box<String>>>>` etc.  Note that arbitrary
    /// compositions and levels cannot be used with `PosStrish`, however.
    pub trait AsStr {
        fn as_str(&self) -> &str;
    }
}
use as_str::AsStr;

impl AsStr for str {
    #[inline] fn as_str(&self) -> &str { self }
}
impl AsStr for String {
    #[inline] fn as_str(&self) -> &str { self }
}
impl<T: AsStr + ?Sized> AsStr for Box<T> {
    #[inline] fn as_str(&self) -> &str { AsStr::as_str(&**self) }
}
impl<T: AsStr + ?Sized> AsStr for Rc<T> {
    #[inline] fn as_str(&self) -> &str { AsStr::as_str(&**self) }
}
impl<T: AsStr + ?Sized> AsStr for Arc<T> {
    #[inline] fn as_str(&self) -> &str { AsStr::as_str(&**self) }
}


/// Restricts what can be used in `PosStrish` to only `Rc` and `Arc` of only
/// `String`, `Box<str>`, and `str`.
///
/// Only the types that implement the private `Sealed` trait can implement this
/// trait, and so nothing else can implement it, which enforces the restriction
/// even though this trait is `pub`lic.
pub trait RefCntStrish: Clone + AsStr + seal_refcnt_strish::Sealed {
    /// Convert from a `&str`.
    fn from_str(s: &str) -> Self;
    /// Make an empty one.
    #[inline] fn empty() -> Self { Self::from_str("") }
}

mod seal_refcnt_strish {
    use super::{Rc, Arc};
    pub trait Sealed {}
    impl Sealed for Rc<String> {}
    impl Sealed for Rc<Box<str>> {}
    impl Sealed for Rc<str> {}
    impl Sealed for Arc<String> {}
    impl Sealed for Arc<Box<str>> {}
    impl Sealed for Arc<str> {}
}

impl RefCntStrish for Rc<String> {
    #[inline] fn from_str(s: &str) -> Self { Self::new(String::from(s)) }
}
impl RefCntStrish for Rc<Box<str>> {
    #[inline] fn from_str(s: &str) -> Self { Self::new(Box::from(s)) }
}
impl RefCntStrish for Rc<str> {
    #[inline] fn from_str(s: &str) -> Self { Self::from(s) }
}
impl RefCntStrish for Arc<String> {
    #[inline] fn from_str(s: &str) -> Self { Self::new(String::from(s)) }
}
impl RefCntStrish for Arc<Box<str>> {
    #[inline] fn from_str(s: &str) -> Self { Self::new(Box::from(s)) }
}
impl RefCntStrish for Arc<str> {
    #[inline] fn from_str(s: &str) -> Self { Self::from(s) }
}


/// Make `RefCntSlice` inaccessible while still being `pub` as needed by our
/// public interfaces.  It is used for the `pub` field `PosStrish.val` but only
/// its `AsRef` trait is accessible.
mod refcnt_slice {
    use super::Range;

    /// Like a slice but where we share ownership of the referenced value.
    /// Enables doing slice-like operations on owned values that can't be
    /// borrowed correctly (due to lifetime expression limitation without GATs)
    /// or that we don't want to borrow because we want to clone shared
    /// ownership anyway.
    #[derive(Clone, Debug)]
    pub struct RefCntSlice<S> {
        /// Entire chunk we are a slice of.
        pub(super) refcnt_strish: S,
        /// Range of our slice as byte positions in the chunk's `&str`.
        pub(super) range: Range<usize>,
    }
}
use refcnt_slice::RefCntSlice;

impl<S> RefCntSlice<S>
    where S: RefCntStrish,
{
    /// Make a new `RefCntSlice` that represents a subslice taken relative to
    /// our slice.  The new slice will share ownership of the underlying chunk.
    #[inline]
    fn slice(&self, subrange: Range<usize>) -> Self {
        let start = self.range.start;
        // These conditions are always met by our internal logic, and this fn is
        // private, so: arithmetic overflows will never occur, even with a
        // hypothetical string of len usize::MAX; and slicing into the
        // underlying string outside of `self.range` will never occur when
        // `self` is a subslice of another `Self` and is shorter than it.
        debug_assert!(subrange.start <= subrange.end);
        debug_assert!(subrange.end <= self.range.end - start);
        Self {
            refcnt_strish: self.refcnt_strish.clone(), // Is only Rc::clone or Arc::clone
            range: (start + subrange.start) .. (start + subrange.end),
        }
    }
}

/// Need to be able to generically borrow the `&str` as sliced by our range,
/// through different levels of indirection that different concrete
/// `RefCntStrish` types have.
impl<S> AsStr for RefCntSlice<S>
    where S: RefCntStrish,
{
    #[inline]
    fn as_str(&self) -> &str {
        &self.refcnt_strish.as_str()[self.range.clone()]
    }
}

/// Enables `PosStrish.val` to be publicly usable but still mostly hidden.
impl<S> AsRef<str> for RefCntSlice<S>
    where S: RefCntStrish,
{
    #[inline] fn as_ref(&self) -> &str { AsStr::as_str(self) }
}


/// A [`TextChunk`] implementation for boxed (heap allocated) string values,
/// termed "string-ish" throughout this crate, used distinctly as chunks.  This
/// is useful for strings produced from streaming sources that buffered into one
/// or more strings separated at possibly arbitrary points.
///
/// This is designed to generically support various boxed string types with
/// shared ownership, e.g. `Rc<String>`, `Arc<str>`, `Rc<Box<str>>`, termed
/// "reference-counted string-ish" types.  The set of such types allowed is
/// those types that already implement the sealed [`RefCntStrish`] trait.  This
/// design allows users to choose what trade-offs of convenience and efficiency
/// they want between these allowed types.  Shared ownership, i.e. `Rc` or
/// `Arc`, is required because this avoids copying string contents, achieving
/// zero-copy operations, once initially buffered or copied into a
/// reference-counted string-ish, i.e. `RefCntStrish`-implementing, value.
///
/// The character position information this provides is only the position of
/// each `char` relative to the start of the stream, because this does not
/// concern itself with what byte encoding or other position information, if
/// any, the streamed source might have had.
///
/// This is not intended for streaming sources that have richer position
/// information that is desired to be preserved.  For those, a dedicated
/// implementation of `TextChunk` should be made instead.
///
/// This is not intended for single string values that are an entire input for
/// parsing.  For that, `PosStr` (as the `Chunk` type of some `Text` type)
/// should be used instead because it provides faster zero-copy operations as
/// well as byte position information.
///
/// Note: The comparison traits are not implemented intentionally, because this
/// is intended as a chunk type and comparing those directly shouldn't be done.
///
/// [`TextChunk`]: TODO
/// [`RefCntStrish`]: trait.RefCntStrish.html
#[derive(Clone, Debug)]
pub struct PosStrish<S> {
    /// The represented heap-allocated string.  The `RefCntSlice` type is
    /// private but it implements `AsRef<str>` and so can be used as `&str`.
    pub val: RefCntSlice<S>,
    /// The `char` position of the start of our string relative to its original
    /// source.
    pub pos: CharPos,
}

impl<S> PosStrish<S>
    where S: RefCntStrish,
{
    /// Given an `Rc`- or `Arc`-boxed string and its position relative to its
    /// original source, make a new chunk that represents this and can be used
    /// with `Text` types.
    #[inline]
    pub fn new(refcnt_strish: S, pos: CharPos) -> Self {
        let end = refcnt_strish.as_str().len();
        Self {
            val: RefCntSlice{refcnt_strish, range: 0 .. end},
            pos,
        }
    }
}


impl<'s, S> From<&'s str> for PosStrish<S>
    where S: RefCntStrish,
{
    #[inline]
    fn from(val: &'s str) -> Self {
        Self::new(S::from_str(val), CharPos(0))
    }
}


impl<S> TextBase for PosStrish<S>
    where S: RefCntStrish,
{
    type Pos = CharPos;

    #[inline]
    fn empty() -> Self { Self::new(S::empty(), CharPos::empty()) }

    #[inline]
    fn is_empty(&self) -> bool { self.val.as_str().len() == 0 }
}

impl<S> TextChunk for PosStrish<S>
    where S: RefCntStrish,
{
    type CharsSrcStrm = PosStrishIter<S>;

    #[inline]
    fn src_strm(&self) -> Self::CharsSrcStrm {
        PosStrishIter::new(self)
    }
}


/// A [`chunk::SourceStream`] (and `Iterator`) of the characters, and their
/// positions, of the text chunk that a `PosStrish` represents.
///
/// The positions of the characters remain correct relative to the original
/// source that the `PosStrish` is from.  (I.e. not relative to its slice.)
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct PosStrishIter<S> {
    chunk: PosStrish<S>,
    byte_idx: usize,
    char_idx: usize,
    peeked: Option<SourceIterItem<CharPos>>,
    accum: Option<(Range<usize>, CharPos)>,
}

impl<S> PosStrishIter<S>
    where S: RefCntStrish,
{
    #[inline]
    fn new(posstrish: &PosStrish<S>) -> Self {
        Self {
            chunk: posstrish.clone(), // Does Rc::clone or Arc::clone
            byte_idx: 0,
            char_idx: 0,
            accum: None,
            peeked: None,
        }
    }

    fn do_next_no_peeked(&mut self) -> Option<<Self as Iterator>::Item> {
        let next = self.chunk.val.as_str()[self.byte_idx ..].chars().next();
        next.map(|ch| {
            let it = SourceIterItem{ch, pos: CharPos(self.chunk.pos.0 + self.char_idx)};
            self.char_idx += 1;
            self.byte_idx += ch.len_utf8();
            it
        })
    }

    #[inline]
    fn do_next(&mut self) -> Option<<Self as Iterator>::Item> {
        if let it @ Some(_) = self.peeked.take() {
            it
        } else {
            self.do_next_no_peeked()
        }
    }
}

/// Required by `chunk::SourceStream`.
impl<S> Iterator for PosStrishIter<S>
    where S: RefCntStrish,
{
    type Item = SourceIterItem<CharPos>;

    /// Note: If `next_accum` was previously called (to do an accumulation) and
    /// returned some item but `accum_done` was not called (to finish an
    /// accumulation), i.e. if we have an unfinished accumulation, this will
    /// abort and drop the unfinished accumulation.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.accum = None;
        self.do_next()
    }

}

/// Enables `PosStrish` to be used as the `Chunk` type of `Text` types that
/// implement `TextConcat` so they can be iterated with
/// `kruvi_core::SourceStream`.  A `chunk::SourceStream` can accumulate what it
/// iterates within a single chunk.  This records and grows a slice range and
/// creates a new chunk representing that, which shares ownership of the
/// original string, for each accumulation, which is zero-copy.
impl<S> text::chunk::SourceStream<PosStrish<S>> for PosStrishIter<S>
    where S: RefCntStrish,
{
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if let Some(ref it) = self.peeked {
            return Some(it)
        }
        let next = self.do_next_no_peeked();
        self.peeked = next;
        self.peeked.as_ref()
    }

    fn next_accum(&mut self) -> Option<<Self as Iterator>::Item> {
        let next = self.do_next();
        if let Some(SourceIterItem{ch, ..}) = next {
            if let Some(accum) = &mut self.accum {
                // Already set, so extend
                accum.0.end = self.byte_idx;
            } else {
                // Not set yet, so set
                self.accum = Some(((self.byte_idx - ch.len_utf8()) .. self.byte_idx,
                                   CharPos(self.chunk.pos.0 + (self.char_idx - 1))));
            }
        }
        next
    }

    fn accum_done(&mut self) -> PosStrish<S> {
        self.accum.take().map_or_else(PosStrish::empty,
                                      |(range, pos)|
                                      PosStrish {
                                          val: self.chunk.val.slice(range),
                                          pos,
                                      })
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn as_str() {
        assert_eq!("a".as_str(), "a");
        assert_eq!(String::from("bb").as_str(), "bb");
        assert_eq!(Box::<str>::from("c cc").as_str(), "c cc");
        assert_eq!(Rc::new(String::from(" dddd")).as_str(), " dddd");
        assert_eq!(Arc::new(Box::new(Rc::new(Arc::<str>::from("eee  ee ")))).as_str(),
                   "eee  ee ");
        assert_eq!(Rc::<str>::from(" f 6 ").as_str(), " f 6 ");
        assert_eq!(Rc::new(Box::<str>::from("GggGggg")).as_str(), "GggGggg");
        assert_eq!(Arc::<str>::from(" h 8 ").as_str(), " h 8 ");
        assert_eq!(Arc::new(Box::<str>::from("IiiiIiiii")).as_str(), "IiiiIiiii");
        assert_eq!(Arc::new(String::from(" jjjjj jjjjj")).as_str(), " jjjjj jjjjj");
    }

    #[test]
    fn refcnt_strish() {
        assert_eq!(<Rc<String> as RefCntStrish>::empty().as_str(), "");
        assert_eq!(<Rc<Box<str>> as RefCntStrish>::empty().as_str(), "");
        assert_eq!(<Rc<str> as RefCntStrish>::empty().as_str(), "");
        assert_eq!(<Arc<String> as RefCntStrish>::empty().as_str(), "");
        assert_eq!(<Arc<Box<str>> as RefCntStrish>::empty().as_str(), "");
        assert_eq!(<Arc<str> as RefCntStrish>::empty().as_str(), "");

        assert_eq!(<Rc<String> as RefCntStrish>::from_str("a").as_str(), "a");
        assert_eq!(<Rc<Box<str>> as RefCntStrish>::from_str("bb").as_str(), "bb");
        assert_eq!(<Rc<str> as RefCntStrish>::from_str(" c ").as_str(), " c ");
        assert_eq!(<Arc<String> as RefCntStrish>::from_str("d d").as_str(), "d d");
        assert_eq!(<Arc<Box<str>> as RefCntStrish>::from_str("e  e ").as_str(), "e  e ");
        assert_eq!(<Arc<str> as RefCntStrish>::from_str("  f").as_str(), "  f");
    }

    #[test]
    #[allow(clippy::cyclomatic_complexity)]
    fn refcnt_slice() {
        let s1 = RefCntSlice{refcnt_strish: Rc::new(String::from("a")), range: 0..1};
        assert_eq!(s1.as_str(), "a");
        assert_eq!(s1.as_ref(), "a");
        let s1s1 = s1.slice(0..1);
        assert_eq!(Rc::strong_count(&s1.refcnt_strish), 2);
        assert!(Rc::ptr_eq(&s1s1.refcnt_strish, &s1.refcnt_strish)); // zero-copy
        assert_eq!(s1s1.range, 0..1);
        assert_eq!(s1s1.as_str(), "a");
        assert_eq!(s1s1.as_ref(), "a");
        {
            let s1s2 = s1s1.slice(1..1);
            assert_eq!(Rc::strong_count(&s1.refcnt_strish), 3);
            assert!(Rc::ptr_eq(&s1s2.refcnt_strish, &s1.refcnt_strish));
            assert_eq!(s1s2.range, 1..1);
            assert_eq!(s1s2.as_str(), "");
            assert_eq!(s1s2.as_ref(), "");
        }
        assert_eq!(Rc::strong_count(&s1.refcnt_strish), 2);

        let s2 = RefCntSlice{refcnt_strish: Arc::<str>::from("aλb▷c"), range: 0..8};
        assert_eq!(s2.as_str(), "aλb▷c");
        assert_eq!(s2.as_ref(), "aλb▷c");
        {
            let s2s1 = s2.slice(1..7);
            assert_eq!(Arc::strong_count(&s2.refcnt_strish), 2);
            assert!(Arc::ptr_eq(&s2s1.refcnt_strish, &s2.refcnt_strish));
            assert_eq!(s2s1.range, 1..7);
            assert_eq!(s2s1.as_str(), "λb▷");
            assert_eq!(s2s1.as_ref(), "λb▷");
            {
                let s2s2 = s2s1.slice(2..3);
                assert_eq!(Arc::strong_count(&s2.refcnt_strish), 3);
                assert!(Arc::ptr_eq(&s2s2.refcnt_strish, &s2.refcnt_strish));
                assert_eq!(s2s2.range, 3..4);
                assert_eq!(s2s2.as_str(), "b");
                assert_eq!(s2s2.as_ref(), "b");
            }
            assert_eq!(Arc::strong_count(&s2.refcnt_strish), 2);
        }
        assert_eq!(Arc::strong_count(&s2.refcnt_strish), 1);
    }

    #[test]
    fn refcnt_slice_huge() {
        const MAX: usize = usize::max_value();
        // Not a valid construction but tests the range arithmetic the same as
        // for a huge hypothetical string that would be valid.
        let s = RefCntSlice{refcnt_strish: Rc::new(Box::<str>::from("dummy")),
                            range: 0 .. MAX};
        let ss1 = s.slice(MAX - 1 .. MAX);
        assert_eq!(ss1.range, MAX - 1 .. MAX);
        let ss2 = ss1.slice(0..0);
        assert_eq!(ss2.range, MAX - 1 .. MAX - 1);
        let ss3 = ss1.slice(0..1);
        assert_eq!(ss3.range, MAX - 1 .. MAX);
        let ss4 = ss1.slice(1..1);
        assert_eq!(ss4.range, MAX .. MAX);
    }

    #[test]
    #[should_panic]
    fn refcnt_slice_panic1() {
        let s = RefCntSlice{refcnt_strish: Rc::<str>::from("aλb▷c"), range: 0..8};
        let ss1 = s.slice(3..6); // Bad: not char boundary
        let _ss1str = ss1.as_str(); // Panics here
    }

    #[test]
    #[should_panic]
    fn refcnt_slice_panic2() {
        let s = RefCntSlice{refcnt_strish: Arc::new(Box::<str>::from("aλb▷c")),
                            range: 0..8};
        let ss1 = s.slice(3..7); // Ok
        let ss2 = ss1.slice(0..6); // Bad: beyond original range
        let _ss2str = ss2.as_str(); // Panics here in release build, above in debug
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn refcnt_slice_assert_conditions1() {
        let s = RefCntSlice{refcnt_strish: Rc::<str>::from("raboof"), range: 0..6};
        let ss1 = s.slice(4 .. 5);
        let _ss2 = ss1.slice(0 .. 2); // Invalid because slice end is beyond `ss1`'s.
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn refcnt_slice_assert_conditions2() {
        let s = RefCntSlice{refcnt_strish: Rc::<str>::from("raboof"), range: 0..6};
        let ss1 = s.slice(2 .. 4);
        let _ss2 = ss1.slice(3 .. 3); // Invalid because slice end is beyond `ss1`'s.
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn refcnt_slice_assert_conditions3() {
        let s = RefCntSlice{refcnt_strish: Rc::<str>::from("raboof"), range: 0..6};
        let ss1 = s.slice(0 .. 0);
        let _ss2 = ss1.slice(0 .. 1); // Invalid because slice end is beyond `ss1`'s.
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn refcnt_slice_assert_conditions4() {
        let s = RefCntSlice{refcnt_strish: Rc::<str>::from("raboof"), range: 0..6};
        let ss1 = s.slice(1 .. 5);
        let _ss2 = ss1.slice(1 .. 0); // Invalid because slice end is less than start.
    }

    #[test]
    fn pos_strish_new() {
        let ps0 = PosStrish::new(Rc::<str>::from(""), CharPos(0));
        assert_eq!((ps0.val.as_str(), ps0.val.range.clone(), ps0.pos),
                   ("", 0..0, CharPos(0)));
        let ps1 = PosStrish::new(Arc::new(String::from("a")), CharPos(0));
        assert_eq!((ps1.val.as_str(), ps1.val.range.clone(), ps1.pos),
                   ("a", 0..1, CharPos(0)));
        let ps2 = PosStrish::new(Rc::new(Box::<str>::from("raboof")), CharPos(2));
        assert_eq!((ps2.val.as_str(), ps2.val.range.clone(), ps2.pos),
                   ("raboof", 0..6, CharPos(2)));
        assert!(PosStrish::<Rc<str>>::empty().is_empty());
        assert!(PosStrish::new(Rc::new(String::from("")), CharPos(0)).is_empty());
        assert!(PosStrish::new(Arc::new(Box::<str>::from("")), CharPos(3)).is_empty());
        assert!(!PosStrish::new(Arc::new(String::from("a")), CharPos(0)).is_empty());
        assert!(!PosStrish::new(Rc::new(Box::<str>::from(" ")), CharPos(9)).is_empty());
        assert!(!PosStrish::new(Rc::<str>::from("zzzzzzzzzzzz"), CharPos(0)).is_empty());
    }

    #[test]
    fn pos_strish_iter() {
        use std::iter;

        let ps0 = PosStrish::new(Rc::new(String::from("")), CharPos(0));
        assert_eq!(ps0.src_strm().collect::<Vec<_>>(), vec![]);
        // Can do src_strm() multiple times
        assert_eq!(ps0.src_strm().collect::<Vec<_>>(), vec![]);

        let ps1 = PosStrish::new(Arc::new(String::from("a")), CharPos(0));
        assert_eq!(ps1.src_strm().collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: 'a', pos: CharPos(0)}]);

        let ps2 = PosStrish::new(Rc::new(Box::<str>::from(" b")), CharPos(1));
        assert_eq!(ps2.src_strm().collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: ' ', pos: CharPos(1)},
                        SourceIterItem{ch: 'b', pos: CharPos(2)}]);

        let ps3 = PosStrish::new(Rc::new(Box::<str>::from("c λ d")), CharPos(9));
        assert_eq!(ps3.src_strm().collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: 'c', pos: CharPos(9)},
                        SourceIterItem{ch: ' ', pos: CharPos(10)},
                        SourceIterItem{ch: 'λ', pos: CharPos(11)},
                        SourceIterItem{ch: ' ', pos: CharPos(12)},
                        SourceIterItem{ch: 'd', pos: CharPos(13)}]);

        // Can do src_strm() many times
        let ps4 = PosStrish::new(Rc::new(Box::<str>::from("▷▷")), CharPos(321));
        assert_eq!(iter::repeat_with(|| ps4.src_strm().collect::<Vec<_>>())
                   .take(11).collect::<Vec<_>>(),
                   iter::repeat(vec![SourceIterItem{ch: '▷', pos: CharPos(321)},
                                     SourceIterItem{ch: '▷', pos: CharPos(322)}])
                   .take(11).collect::<Vec<_>>());
    }

    #[test]
    #[allow(unused_results, clippy::cyclomatic_complexity)]
    fn pos_strish_srcstrm() {
        use text::chunk::SourceStream;
        use std::iter;

        let ps0 = PosStrish::new(Rc::new(String::from("")), CharPos(0));
        assert_eq!(Rc::strong_count(&ps0.val.refcnt_strish), 1);
        assert_eq!(ps0.src_strm().next_accum(), None);
        assert!(ps0.src_strm().accum_done().is_empty());
        assert_eq!(Rc::strong_count(&ps0.val.refcnt_strish), 1);
        {
            let mut ps0ss = ps0.src_strm();
            assert_eq!(Rc::strong_count(&ps0.val.refcnt_strish), 2);
            // zero-copy
            assert!(Rc::ptr_eq(&ps0ss.chunk.val.refcnt_strish, &ps0.val.refcnt_strish));
            assert_eq!(ps0ss.peek(), None);
            assert_eq!(ps0ss.next_accum(), None);
            assert!(ps0ss.accum_done().is_empty());
        }
        assert_eq!(Rc::strong_count(&ps0.val.refcnt_strish), 1);

        let ps1 = PosStrish::new(Arc::<str>::from(" ▷\naλ"), CharPos(5));
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 1);
        let mut ps1ss = ps1.src_strm();
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 2);
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: ' ', pos: CharPos(5)}));
        assert_eq!(ps1ss.next(), Some(SourceIterItem{ch: ' ', pos: CharPos(5)}));
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: '▷', pos: CharPos(6)}));
        assert_eq!(ps1ss.next_accum(), Some(SourceIterItem{ch: '▷', pos: CharPos(6)}));
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 2);
        let a1 = ps1ss.accum_done();
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 3);
        assert_eq!((a1.val.as_str(), a1.val.range.clone(), a1.pos),
                   ("▷", 1..4, CharPos(6)));
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: '\n', pos: CharPos(7)}));
        assert_eq!(ps1ss.next(), Some(SourceIterItem{ch: '\n', pos: CharPos(7)}));
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: 'a', pos: CharPos(8)}));
        assert_eq!(ps1ss.next_accum(), Some(SourceIterItem{ch: 'a', pos: CharPos(8)}));
        // Multiple peek
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(ps1ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(ps1ss.next_accum(), Some(SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(ps1ss.peek(), None);
        assert_eq!(ps1ss.next_accum(), None);
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 3);
        let a2 = ps1ss.accum_done();
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 4);
        assert_eq!((a2.val.as_str(), a2.val.range.clone(), a2.pos),
                   ("aλ", 5..8, CharPos(8)));
        assert!(ps1ss.accum_done().is_empty());
        assert_eq!(ps1ss.peek(), None);
        assert_eq!(ps1ss.next_accum(), None);
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 4);
        // zero-copy
        assert!(Arc::ptr_eq(&ps1ss.chunk.val.refcnt_strish, &ps1.val.refcnt_strish));

        // src_strm() on a chunk returned from prior accum_done()
        let mut a2ss = a2.src_strm();
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 5);
        assert_eq!(a2ss.peek(), Some(&SourceIterItem{ch: 'a', pos: CharPos(8)}));
        assert_eq!(a2ss.next_accum(), Some(SourceIterItem{ch: 'a', pos: CharPos(8)}));
        assert_eq!(a2ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(a2ss.next_accum(), Some(SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(a2ss.peek(), None);
        assert_eq!(a2ss.next_accum(), None);
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 5);
        let a3 = a2ss.accum_done();
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 6);
        assert_eq!((a3.val.as_str(), a3.val.range.clone(), a3.pos),
                   ("aλ", 5..8, CharPos(8)));
        assert!(a2ss.accum_done().is_empty());
        assert_eq!(a2ss.peek(), None);
        assert_eq!(a2ss.next_accum(), None);
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 6);
        // zero-copy
        assert!(Arc::ptr_eq(&a2ss.chunk.val.refcnt_strish, &ps1.val.refcnt_strish));

        // On a chunk from a chunk
        let mut a3ss = a3.src_strm();
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 7);
        // Multiple peek
        assert_eq!(a3ss.peek(), Some(&SourceIterItem{ch: 'a', pos: CharPos(8)}));
        assert_eq!(a3ss.peek(), Some(&SourceIterItem{ch: 'a', pos: CharPos(8)}));
        assert_eq!(a3ss.next_accum(), Some(SourceIterItem{ch: 'a', pos: CharPos(8)}));
        // Multiple peek
        assert_eq!(a3ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(a3ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(a3ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert_eq!(a3ss.peek(), Some(&SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        // next() after next_accum() loses the accumulation
        assert_eq!(a3ss.next(), Some(SourceIterItem{ch: 'λ', pos: CharPos(9)}));
        assert!(a3ss.accum_done().is_empty());
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 7);
        assert_eq!(a3ss.peek(), None);
        assert_eq!(a3ss.next_accum(), None);
        assert_eq!(Arc::strong_count(&ps1.val.refcnt_strish), 7);
        // zero-copy
        assert!(Arc::ptr_eq(&a3ss.chunk.val.refcnt_strish, &ps1.val.refcnt_strish));

        let ps2 = PosStrish::new(Rc::new(String::from("zyxxyz")), CharPos(0));
        let mut ps2ss = ps2.src_strm();
        assert!(ps2ss.accum_done().is_empty());
        assert_eq!(ps2ss.next(), Some(SourceIterItem{ch: 'z', pos: CharPos(0)}));
        assert_eq!(ps2ss.peek(), Some(&SourceIterItem{ch: 'y', pos: CharPos(1)}));
        assert_eq!(ps2ss.peek(), Some(&SourceIterItem{ch: 'y', pos: CharPos(1)}));
        assert_eq!(ps2ss.next(), Some(SourceIterItem{ch: 'y', pos: CharPos(1)}));
        assert_eq!(ps2ss.next_accum(), Some(SourceIterItem{ch: 'x', pos: CharPos(2)}));
        assert_eq!(ps2ss.next(), Some(SourceIterItem{ch: 'x', pos: CharPos(3)}));
        assert!(ps2ss.accum_done().is_empty());
        assert_eq!(ps2ss.peek(), Some(&SourceIterItem{ch: 'y', pos: CharPos(4)}));
        assert_eq!(ps2ss.next(), Some(SourceIterItem{ch: 'y', pos: CharPos(4)}));
        assert_eq!(ps2ss.peek(), Some(&SourceIterItem{ch: 'z', pos: CharPos(5)}));
        assert_eq!(ps2ss.peek(), Some(&SourceIterItem{ch: 'z', pos: CharPos(5)}));
        assert_eq!(ps2ss.peek(), Some(&SourceIterItem{ch: 'z', pos: CharPos(5)}));
        assert_eq!(ps2ss.next(), Some(SourceIterItem{ch: 'z', pos: CharPos(5)}));
        assert!(ps2ss.accum_done().is_empty());
        assert_eq!(ps2ss.next(), None);
        assert_eq!(ps2ss.peek(), None);
        assert_eq!(ps2ss.next_accum(), None);
        assert_eq!(ps2ss.peek(), None);
        assert_eq!(ps2ss.next(), None);
        assert!(ps2ss.accum_done().is_empty());

        // Can do src_strm() many times
        assert_eq!(iter::repeat_with(|| {
                             let mut ps2ss = ps2.src_strm();
                             assert_eq!(Rc::strong_count(&ps2.val.refcnt_strish), 3);
                             ps2ss.next();
                             ps2ss.next();
                             while let Some(_) = ps2ss.next_accum() {}
                             let a = ps2ss.accum_done();
                             (a.val.as_str().into(), a.val.range.clone(), a.pos)
                         })
                   .take(22).collect::<Vec<_>>(),
                   iter::repeat((String::from("xxyz"), 2..6, CharPos(2)))
                   .take(22).collect::<Vec<_>>());
        assert_eq!(Rc::strong_count(&ps2.val.refcnt_strish), 2);
    }
}
