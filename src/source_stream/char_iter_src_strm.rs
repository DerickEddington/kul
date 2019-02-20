//! A generic `SourceStream` implementation for any `Iterator` of `char`s and
//! that provides the position of each `char` relative to the start of the
//! iteration.

use std::{
    iter::{Peekable, Map, Enumerate},
    rc::Rc, sync::Arc,
};

use crate::{
    SourceStream, SourceIterItem, Text, TextBase,
    text::chunk::{CharPos, PosStrish, RefCntStrish},
    parser::{DatumAllocator, AllocError},
};


// These functions offer different trade-offs for converting the `String`s used
// for buffering incoming `char`s to the shared-ownership types needed by
// `PosStrish` to achieve zero-copy operations.

/// Converting a `String` to a boxed `str` slice gives the program a chance to
/// use minimal memory by freeing the `String`'s extra capacity.
///
/// This might cheaply convert in place, because it only shrinks the allocated
/// size if the `String`'s capacity is greater than its length (but this is
/// likely), and the allocator has a chance to not move it if reallocation is
/// done and a chance to free and reuse the extra capacity (depending on the
/// allocator used).  But the allocator might move it, which is a cost, but this
/// might be acceptable for the application because that one-time performance
/// cost could be worth giving it the chance to reclaim unused memory from these
/// now-immutable strings which might live a long time.
///
/// But the additional disadvantage of `Rc<Box<str>>` is that there is an extra
/// level of pointer indirection compared to `Rc<str>`, which could impact cache
/// locality.  However, [converting to `Rc<str>`](fn.to_rc_str.html) always must
/// move the string contents and never has the chance of cheaply converting in
/// place.
#[inline]
pub fn to_rc_box_str(s: String) -> Rc<Box<str>> {
    Rc::new(s.into_boxed_str())
}

/// Simply putting a `String` in an `Rc` has the benefit that no moving of the
/// string contents will ever occur, which is always very fast to construct.
///
/// But the disadvantages are: unused `String` capacity is not freed, which
/// might be a problem if you keep many of these alive; and there is an extra
/// level of pointer indirection [compared to `Rc<str>`](fn.to_rc_str.html),
/// which could impact cache locality.
#[inline]
pub fn to_rc_string(s: String) -> Rc<String> {
    Rc::new(s)
}

/// Converting a `String` to an `Rc<str>` slice (which is also "boxed",
/// i.e. heap-allocated) has the benefits of:
///
/// 1) Always freeing the `String`'s unused capacity, which might be important
/// if you keep many of these alive.
///
/// 2) Minimizing pointer indirection to only the one level that is an `Rc`
/// pointing at a heap block, which might be important for cache locality.
///
/// However, this conversion always has the one-time cost of moving the string
/// contents into the new `Rc` allocation.
#[inline]
pub fn to_rc_str(s: String) -> Rc<str> {
    s.into()
}

/// Use an `Arc`, for multi-threaded sharing, for the conversion with the same
/// trade-offs as [`to_rc_box_str`](fn.to_rc_box_str.html).
#[inline]
pub fn to_arc_box_str(s: String) -> Arc<Box<str>> {
    Arc::new(s.into_boxed_str())
}

/// Use an `Arc`, for multi-threaded sharing, for the conversion with the same
/// trade-offs as [`to_rc_string`](fn.to_rc_string.html).
#[inline]
pub fn to_arc_string(s: String) -> Arc<String> {
    Arc::new(s)
}

/// Use an `Arc`, for multi-threaded sharing, for the conversion with the same
/// trade-offs as [`to_rc_str`](fn.to_rc_str.html).
#[inline]
pub fn to_arc_str(s: String) -> Arc<str> {
    s.into()
}


/// A generic [`SourceStream`] implementation for any `Iterator` of `char`s and
/// that provides the position of each `char` relative to the start of the
/// iteration.  The various `to_a?rc(_box)?_str(ing)?` converter functions are
/// used with this.
///
/// This is useful for streaming sources that are not entirely in memory, that
/// do not buffer in ways compatible with `StrishIterSourceStream`, and that do
/// not provide their own (or better) character position information.  Most
/// streaming sources will have their own internal buffers and copy `char`s out
/// of that, and those will be buffered again by `CharIterSourceStream`, so this
/// is not zero-copy in this regard.  But once copied in this way and converted
/// to a shared-ownership type by a converter function (which might copy again
/// one last time, depending on which one is used), the `Text` types we produce
/// have zero-copy operations thereafter (achieved via shared ownership).
///
/// If the streaming source's internal buffering can be made compatible with
/// [`StrishIterSourceStream`], then that should be used instead of
/// `CharIterSourceStream`, because that can achieve full zero-copy operation.
///
/// [`SourceStream`]: ../../kruvi_core/trait.SourceStream.html
/// [`StrishIterSourceStream`]: struct.StrishIterSourceStream.html
#[derive(Debug)]
pub struct CharIterSourceStream<CI, F, R>
    where CI: Iterator<Item = char>,
          F: Fn(String) -> R,
          R: RefCntStrish,
{
    pe_iter: PeekableSourceIterItemIter<CI>,
    accum: Option<(String, CharPos)>,
    // Zero-sized when our above converters (or any "function item type") are
    // used. http://doc.rust-lang.org/reference/types/function-item.html
    to_refcnt_strish: F,
}

type PeekableSourceIterItemIter<CI> = Peekable<Map<Enumerate<CI>, MapFn>>;

type MapFn = fn((usize, char)) -> SourceIterItem<CharPos>;

impl<CI, F, R> CharIterSourceStream<CI, F, R>
    where CI: Iterator<Item = char>,
          F: Fn(String) -> R,
          R: RefCntStrish,
{
    /// Given anything that can convert into an `Iterator` of `char`s, make a
    /// new `SourceStream` from it that yields its `char`s and their positions
    /// relative to the start of the iteration and that can accumulate these
    /// items to make new `Text`s from.
    ///
    /// The `to_refcnt_strish` argument must be a function/closure that converts
    /// the `String`s we will accumulate into the reference-counted string-ish,
    /// `RefCntStrish`-implementing, type we want to use with the `PosStrish`
    /// chunk values we will produce as a `SourceStream`.
    pub fn new<I>(iter: I, to_refcnt_strish: F) -> Self
        where I: IntoIterator<IntoIter = CI, Item = char>,
    {
        let map_fn: MapFn = |(pos, ch)| SourceIterItem{ch, pos: CharPos(pos)};
        Self {
            pe_iter: iter.into_iter()
                         .enumerate()
                         .map(map_fn)
                         .peekable(),
            accum: None,
            to_refcnt_strish,
        }
    }
}


/// Required by `SourceStream`.
impl<CI, F, R> Iterator for CharIterSourceStream<CI, F, R>
    where CI: Iterator<Item = char>,
          F: Fn(String) -> R,
          R: RefCntStrish,
{
    type Item = SourceIterItem<CharPos>;

    /// Note: If `next_accum` was previously called (to do an accumulation) and
    /// returned some item but `accum_done` was not called (to finish an
    /// accumulation), i.e. if we have an unfinished accumulation, this will
    /// abort and drop the unfinished accumulation.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.accum = None;
        self.pe_iter.next()
    }
}


/// Enables `CharIterSourceStream` to be used as the input source for parsing
/// with compatible `Parser` types.
impl<CI, F, R, TT, DA> SourceStream<DA> for CharIterSourceStream<CI, F, R>
    where CI: Iterator<Item = char>,
          F: Fn(String) -> R,
          R: RefCntStrish,
          TT: Text<Pos = CharPos>,
          TT::Chunk: From<PosStrish<R>>,
          DA: DatumAllocator<TT = TT> + ?Sized,  // Ignored
{
    #[inline]
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.pe_iter.peek()
    }

    fn next_accum(&mut self, _: &mut DA)
                  -> Result<Option<<Self as Iterator>::Item>, AllocError>
    {
        let next = self.pe_iter.next();
        if let Some(SourceIterItem{ch, pos}) = next {
            if let Some((s, _)) = &mut self.accum {
                // Already set, so extend
                s.push(ch);
            } else {
                // Not set yet, so set
                let mut s = String::new();
                s.push(ch);
                self.accum = Some((s, pos));
            }
        }
        Ok(next)
    }

    fn accum_done(&mut self, _: &mut DA) -> Result<TT, AllocError> {
        let ps = if let Some((s, pos)) = self.accum.take() {
            // These calls to `to_refcnt_strish` are direct (do not involve
            // function pointer indirection) when the field is a "function item
            // type".
            PosStrish::new((self.to_refcnt_strish)(s), pos)
        } else {
            PosStrish::empty()
        };
        Ok(TT::from_chunkish(ps))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter() {
        let mut ciss0 = CharIterSourceStream::new("".chars(), to_rc_string);
        assert!(ciss0.next().is_none());

        let ciss1 = CharIterSourceStream::new("a".chars(), to_arc_string);
        assert_eq!(ciss1.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: 'a', pos: CharPos(0)}]);

        let ciss2 = CharIterSourceStream::new("λb".chars(), to_rc_box_str);
        assert_eq!(ciss2.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: 'λ', pos: CharPos(0)},
                        SourceIterItem{ch: 'b', pos: CharPos(1)}]);

        let s = String::from("c▷ ▷");
        let ciss3 = CharIterSourceStream::new(s.chars(), to_arc_box_str);
        assert_eq!(ciss3.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: 'c', pos: CharPos(0)},
                        SourceIterItem{ch: '▷', pos: CharPos(1)},
                        SourceIterItem{ch: ' ', pos: CharPos(2)},
                        SourceIterItem{ch: '▷', pos: CharPos(3)}]);

        let ciss4 = CharIterSourceStream::new(" 1 ".chars().chain("23".chars()),
                                              to_rc_str);
        assert_eq!(ciss4.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: ' ', pos: CharPos(0)},
                        SourceIterItem{ch: '1', pos: CharPos(1)},
                        SourceIterItem{ch: ' ', pos: CharPos(2)},
                        SourceIterItem{ch: '2', pos: CharPos(3)},
                        SourceIterItem{ch: '3', pos: CharPos(4)}]);

        let ciss5 = CharIterSourceStream::new(
            (0..4321).map(|n| if n % 2 == 0 { 'λ' } else { '-' }),
            to_arc_str);
        assert_eq!(ciss5.collect::<Vec<_>>(),
                   (0..4321)
                   .map(|n| SourceIterItem{ch: if n % 2 == 0 { 'λ' } else { '-' },
                                           pos: CharPos(n)})
                   .collect::<Vec<_>>());
    }

    #[test]
    #[allow(clippy::cyclomatic_complexity)]
    fn source_stream() {
        use std::marker::PhantomData;
        use crate::{text::TextVec, Datum, datum::DatumBox};

        struct DummyDA<R>(PhantomData<R>);
        impl<R> DatumAllocator for DummyDA<R>
            where R: RefCntStrish,
        {
            type TT = TextVec<PosStrish<R>>;
            type ET = ();
            type DR = DatumBox<Self::TT, Self::ET>;
            fn new_datum(&mut self, _from: Datum<Self::TT, Self::ET, Self::DR>)
                         -> Result<Self::DR, AllocError> {
                unreachable!()
            }
        }

        fn txt_to_chunks<R>(t: &TextVec<PosStrish<R>>) -> Vec<(&str, usize)>
            where R: RefCntStrish,
        {
            use crate::Text;
            t.iter_chunks().map(|c| (c.val.as_ref(), c.pos.0)).collect::<Vec<_>>()
        }

        let dda_rc_string: &mut DummyDA<Rc<String>> = &mut DummyDA(PhantomData);
        let dda_arc_box_str: &mut DummyDA<Arc<Box<str>>> = &mut DummyDA(PhantomData);
        let dda_rc_str: &mut DummyDA<Rc<str>> = &mut DummyDA(PhantomData);
        let dda_arc_string: &mut DummyDA<Arc<String>> = &mut DummyDA(PhantomData);

        let mut ciss0 = CharIterSourceStream::new("".chars(), to_rc_string);
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss0), None);
        assert_eq!(ciss0.next_accum(dda_rc_string), Ok(None));
        assert_eq!(ciss0.accum_done(dda_rc_string).map(|t| t.is_empty()), Ok(true));

        let mut ciss1 = CharIterSourceStream::new("Z".chars(), to_arc_box_str);
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss1),
                   Some(&SourceIterItem{ch: 'Z', pos: CharPos(0)}));
        assert_eq!(ciss1.next_accum(dda_arc_box_str),
                   Ok(Some(SourceIterItem{ch: 'Z', pos: CharPos(0)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss1), None);
        assert_eq!(ciss1.next_accum(dda_arc_box_str), Ok(None));
        assert_eq!(ciss1.accum_done(dda_arc_box_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("Z", 0)]));

        let mut ciss2 = CharIterSourceStream::new(r"y\\x {\}}".chars(), to_rc_str);
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: 'y', pos: CharPos(0)}));
        assert_eq!(ciss2.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: 'y', pos: CharPos(0)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: '\\', pos: CharPos(1)}));
        assert_eq!(ciss2.accum_done(dda_rc_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("y", 0)]));
        assert_eq!(ciss2.next(), Some(SourceIterItem{ch: '\\', pos: CharPos(1)}));
        assert_eq!(ciss2.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: '\\', pos: CharPos(2)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: 'x', pos: CharPos(3)}));
        assert_eq!(ciss2.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: 'x', pos: CharPos(3)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: ' ', pos: CharPos(4)}));
        assert_eq!(ciss2.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: ' ', pos: CharPos(4)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: '{', pos: CharPos(5)}));
        assert_eq!(ciss2.accum_done(dda_rc_str).as_ref().map(txt_to_chunks),
                   Ok(vec![(r"\x ", 2)]));
        assert_eq!(ciss2.next(), Some(SourceIterItem{ch: '{', pos: CharPos(5)}));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: '\\', pos: CharPos(6)}));
        assert_eq!(ciss2.accum_done(dda_rc_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(ciss2.next(), Some(SourceIterItem{ch: '\\', pos: CharPos(6)}));
        assert_eq!(ciss2.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: '}', pos: CharPos(7)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2),
                   Some(&SourceIterItem{ch: '}', pos: CharPos(8)}));
        assert_eq!(ciss2.accum_done(dda_rc_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("}", 7)]));
        assert_eq!(ciss2.next(), Some(SourceIterItem{ch: '}', pos: CharPos(8)}));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2), None);
        assert_eq!(ciss2.accum_done(dda_rc_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(ciss2.next_accum(dda_rc_str), Ok(None));
        assert_eq!(ciss2.accum_done(dda_rc_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(ciss2.next(), None);
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss2), None);

        let mut ciss3 = CharIterSourceStream::new("wVu".chars(), to_arc_string);
        assert_eq!(ciss3.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: 'w', pos: CharPos(0)})));
        assert_eq!(ciss3.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: 'V', pos: CharPos(1)})));
        // next() after next_accum() loses the accumulation
        assert_eq!(ciss3.next(), Some(SourceIterItem{ch: 'u', pos: CharPos(2)}));
        assert_eq!(ciss3.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut ciss3), None);
        assert_eq!(ciss3.next_accum(dda_arc_string), Ok(None));
    }
}
