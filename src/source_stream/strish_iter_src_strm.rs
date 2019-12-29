//! A generic `SourceStream` implementation for any `Iterator` of
//! shared-ownership (i.e. reference-counted) string-ish types and that provides
//! the position of each `char` relative to the start of the iteration.

use crate::{
    SourceStream, SourceIterItem, TextConcat, TextChunk,
    text::chunk::{PosStrish, PosStrishIter, RefCntStrish, CharPos,
                  SourceStream as ChunkSourceStream},
    parser::{DatumAllocator, AllocError},
};


/// A generic [`SourceStream`] implementation for any `Iterator` of
/// shared-ownership (i.e. reference-counted) string-ish types and that provides
/// the position of each `char` relative to the start of the iteration.
///
/// This is useful for streaming sources that are not entirely in memory, that
/// internally buffer into reference-counted string-ish types that can be given
/// directly as iterated items, and that do not provide their own (or better)
/// character position information.  For sources that buffer directly into the
/// iterated item type, zero-copy operation is achieved (not counting any
/// (usual) copying from the OS into the user-space process).  The `Text` types
/// we produce have zero-copy operations thereafter (achieved via shared
/// ownership)
///
/// [`SourceStream`]: ../../kul_core/trait.SourceStream.html
#[derive(Debug)]
pub struct StrishIterSourceStream<SI, TT>
    where SI: Iterator,
          SI::Item: RefCntStrish,
{
    cur_posstrish_src_strm: Option<PosStrishIter<SI::Item>>,
    char_idx: usize,
    strish_iter: SI,
    peeked_posstrish_src_strm: Option<PosStrishIter<SI::Item>>,
    accum: Option<TT>,
}

impl<SI, TT> StrishIterSourceStream<SI, TT>
    where SI: Iterator,
          SI::Item: RefCntStrish,
{
    /// Given anything that can convert into an `Iterator` of reference-counted
    /// string-ish values, make a new `SourceStream` from it that yields its
    /// logical sequence of `char`s (across string boundaries) and their
    /// positions relative to the start of the iteration and that can accumulate
    /// these items to make new `Text`s from.
    pub fn new<I>(iter: I) -> Self
        where I: IntoIterator<IntoIter = SI, Item = SI::Item>,
    {
        let mut strish_iter = iter.into_iter();
        let char_idx = 0;
        let cur_posstrish_src_strm = Self::next_posstrish_of(&mut strish_iter, char_idx);
        Self {
            cur_posstrish_src_strm,
            char_idx,
            strish_iter,
            peeked_posstrish_src_strm: None,
            accum: None,
        }
    }

    /// Get next from underlying strish iterator and convert into a
    /// `PosStrishIter`.  A `Some` return is guaranteed to be of a non-empty
    /// strish.
    fn next_posstrish_of(strish_iter: &mut SI, char_idx: usize)
                         -> Option<PosStrishIter<SI::Item>> {
        // Skip any empty strishes
        for strish in strish_iter {
            let ps = PosStrish::new(strish, CharPos(char_idx));
            let len = ps.val.as_ref().len();
            if len > 0 { return Some(ps.src_strm()) }
        }
        None
    }

    /// Get and convert the next strish into a `PosStrishIter` and set the
    /// current one to be it, using an already peeked one if available.
    fn next_posstrish(&mut self) {
        // The current one should only be replaced when it's at its end.
        debug_assert_eq!(self.cur_posstrish_src_strm.as_mut()
                         .map(|cpss| cpss.peek().is_none()),
                         Some(true));
        self.cur_posstrish_src_strm
            = self.peeked_posstrish_src_strm.take().or_else(
                || Self::next_posstrish_of(&mut self.strish_iter, self.char_idx));
    }
}


/// Required by `SourceStream`.
impl<SI, TT> Iterator for StrishIterSourceStream<SI, TT>
    where SI: Iterator,
          SI::Item: RefCntStrish,
{
    type Item = SourceIterItem<CharPos>;

    /// Note: If `next_accum` was previously called (to do an accumulation) and
    /// returned some item but `accum_done` was not called (to finish an
    /// accumulation), i.e. if we have an unfinished accumulation, this will
    /// abort and drop the unfinished accumulation.
    fn next(&mut self) -> Option<Self::Item> {
        self.accum = None;
        loop {
            if let Some(cpss) = &mut self.cur_posstrish_src_strm {
                if let it @ Some(_) = cpss.next() {
                    self.char_idx += 1;
                    break it
                } else {
                    self.next_posstrish();
                }
            } else {
                break None
            }
        }
    }
}


/// Enables `StrishIterSourceStream` to be used as the input source for parsing
/// with compatible `Parser` types.
impl<SI, TT, DA> SourceStream<DA> for StrishIterSourceStream<SI, TT>
    where SI: Iterator,
          SI::Item: RefCntStrish,
          TT: TextConcat<DA, Pos = CharPos>,
          TT::Chunk: From<PosStrish<SI::Item>>,
          DA: DatumAllocator<TT = TT>,
{
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if let Some(ref mut cpss) = self.cur_posstrish_src_strm {
            if let it @ Some(_) = cpss.peek() {
                return it
            } else {
                if self.peeked_posstrish_src_strm.is_none() {
                    // Using `next_posstrish_of` (instead of `next_posstrish`)
                    // avoids borrow conflicts that would otherwise happen.
                    self.peeked_posstrish_src_strm
                        = Self::next_posstrish_of(&mut self.strish_iter, self.char_idx);
                }
                if let Some(ref mut ppss) = self.peeked_posstrish_src_strm {
                    return ppss.peek() // Guaranteed to be `Some`
                }
            }
        }
        None
    }

    fn next_accum(&mut self, dalloc: &mut DA)
                  -> Result<Option<<Self as Iterator>::Item>,
                            AllocError>
    {
        Ok(loop {
            if let Some(cpss) = &mut self.cur_posstrish_src_strm {
                if let it @ Some(_) = cpss.next_accum() {
                    self.char_idx += 1;
                    break it
                } else {
                    let chunk_ended = TT::from_chunkish(cpss.accum_done());
                    self.accum = Some(self.accum.take().unwrap_or_else(TT::empty)
                                      .concat(chunk_ended, dalloc)?);
                    self.next_posstrish();
                }
            } else {
                break None
            }
        })
    }

    fn accum_done(&mut self, dalloc: &mut DA) -> Result<TT, AllocError> {
        let mut accum = self.accum.take().unwrap_or_else(TT::empty);
        if let Some(cpss) = &mut self.cur_posstrish_src_strm {
            let chunk_ended = TT::from_chunkish(cpss.accum_done());
            accum = accum.concat(chunk_ended, dalloc)?;
        }
        Ok(accum)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::{rc::Rc, sync::Arc, iter::{empty, once, repeat}};
    use crate::text::TextVec;

    type SISS<SI> = StrishIterSourceStream<SI,
                                           TextVec<PosStrish<<SI as Iterator>::Item>>>;

    fn strish<S: RefCntStrish>(s: &str) -> S { S::from_str(s) }

    #[test]
    fn iter() {
        let mut siss00 = SISS::<_>::new(empty::<Rc<str>>());
        assert!(siss00.next().is_none());
        let mut siss01 = SISS::<_>::new(once(strish::<Arc<String>>("")));
        assert!(siss01.next().is_none());
        let mut siss02 = SISS::<_>::new(repeat(strish::<Rc<Box<str>>>("")).take(2));
        assert!(siss02.next().is_none());
        let mut siss03 = SISS::<_>::new(repeat(strish::<Arc<str>>("")).take(321));
        assert!(siss03.next().is_none());

        let siss11 = SISS::<_>::new(once(strish::<Rc<String>>("a")));
        assert_eq!(siss11.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: 'a', pos: CharPos(0)}]);
        let siss12 = SISS::<_>::new(once(strish(""))
                                    .chain(once(strish::<Rc<String>>("𝝰"))
                                           .chain(once(strish("")))));
        assert_eq!(siss12.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: '𝝰', pos: CharPos(0)}]);

        let siss21 = SISS::<_>::new(once(strish::<Arc<Box<str>>>("𝞫b")));
        assert_eq!(siss21.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: '𝞫', pos: CharPos(0)},
                        SourceIterItem{ch: 'b', pos: CharPos(1)}]);
        let siss22 = SISS::<_>::new(once(strish::<Rc<Box<str>>>("𝞫"))
                                    .chain(once(strish(""))
                                           .chain(once(strish("b")))));
        assert_eq!(siss22.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: '𝞫', pos: CharPos(0)},
                        SourceIterItem{ch: 'b', pos: CharPos(1)}]);
        let siss23 = SISS::<_>::new(
            repeat(strish("")).take(3)
            .chain(once(strish::<Arc<String>>(" "))
                   .chain(repeat(strish("")).take(4)
                          .chain(once(strish(" "))
                                 .chain(repeat(strish("")).take(2))))));
        assert_eq!(siss23.collect::<Vec<_>>(),
                   vec![SourceIterItem{ch: ' ', pos: CharPos(0)},
                        SourceIterItem{ch: ' ', pos: CharPos(1)}]);

        let siss31 = SISS::<_>::new(
            vec![" テ", "ス", "トはとても", "", "退屈で", "す!"]
                .into_iter().map(strish::<Rc<str>>).cycle().take(6 * 1234));
        assert_eq!(siss31.collect::<Vec<_>>(),
                   " テストはとても退屈です!".chars().cycle().take(13 * 1234).enumerate()
                   .map(|(count, ch)| SourceIterItem{ch, pos: CharPos(count)})
                   .collect::<Vec<_>>());
    }

    #[test]
    #[allow(clippy::cyclomatic_complexity)]
    fn source_stream() {
        use std::marker::PhantomData;
        use crate::{Datum, datum::DatumBox, TextBase};

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
        let dda_rc_box_str: &mut DummyDA<Rc<Box<str>>> = &mut DummyDA(PhantomData);
        let dda_rc_str: &mut DummyDA<Rc<str>> = &mut DummyDA(PhantomData);
        let dda_arc_string: &mut DummyDA<Arc<String>> = &mut DummyDA(PhantomData);
        let dda_arc_box_str: &mut DummyDA<Arc<Box<str>>> = &mut DummyDA(PhantomData);
        let dda_arc_str: &mut DummyDA<Arc<str>> = &mut DummyDA(PhantomData);

        let mut siss00 = SISS::<_>::new(empty::<Rc<str>>());
        assert!(SourceStream::<DummyDA<_>>::peek(&mut siss00).is_none());
        assert_eq!(siss00.next_accum(dda_rc_str), Ok(None));
        assert_eq!(siss00.accum_done(dda_rc_str).map(|t| t.is_empty()), Ok(true));
        let mut siss01 = SISS::<_>::new(once(strish::<Arc<String>>("")));
        assert!(SourceStream::<DummyDA<_>>::peek(&mut siss01).is_none());
        assert_eq!(siss01.next_accum(dda_arc_string), Ok(None));
        assert_eq!(siss01.accum_done(dda_arc_string).map(|t| t.is_empty()), Ok(true));
        let mut siss02 = SISS::<_>::new(repeat(strish::<Rc<Box<str>>>("")).take(2));
        assert!(SourceStream::<DummyDA<_>>::peek(&mut siss02).is_none());
        assert_eq!(siss02.next_accum(dda_rc_box_str), Ok(None));
        assert_eq!(siss02.accum_done(dda_rc_box_str).map(|t| t.is_empty()), Ok(true));
        let mut siss03 = SISS::<_>::new(repeat(strish::<Arc<str>>("")).take(321));
        assert!(SourceStream::<DummyDA<_>>::peek(&mut siss03).is_none());
        assert_eq!(siss03.next_accum(dda_arc_str), Ok(None));
        assert_eq!(siss03.accum_done(dda_arc_str).map(|t| t.is_empty()), Ok(true));

        let mut siss11 = SISS::<_>::new(once(strish::<Rc<String>>("a")));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss11),
                   Some(&SourceIterItem{ch: 'a', pos: CharPos(0)}));
        assert_eq!(siss11.next_accum(dda_rc_string),
                   Ok(Some(SourceIterItem{ch: 'a', pos: CharPos(0)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss11), None);
        assert_eq!(siss11.next_accum(dda_rc_string), Ok(None));
        assert_eq!(siss11.accum_done(dda_rc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("a", 0)]));
        assert_eq!(siss11.accum_done(dda_rc_string).map(|t| t.is_empty()), Ok(true));
        let mut siss12 = SISS::<_>::new(once(strish(""))
                                        .chain(once(strish::<Arc<Box<str>>>("𝝰"))
                                               .chain(once(strish("")))));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss12),
                   Some(&SourceIterItem{ch: '𝝰', pos: CharPos(0)}));
        assert_eq!(siss12.next_accum(dda_arc_box_str),
                   Ok(Some(SourceIterItem{ch: '𝝰', pos: CharPos(0)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss12), None);
        assert_eq!(siss12.next_accum(dda_arc_box_str), Ok(None));
        assert_eq!(siss12.accum_done(dda_arc_box_str).as_ref().map(txt_to_chunks),
                   Ok(vec![("𝝰", 0)]));
        assert_eq!(siss12.accum_done(dda_arc_box_str).map(|t| t.is_empty()), Ok(true));

        let mut siss2 = SISS::<_>::new(
            repeat(strish("")).take(3)
            .chain(once(strish::<Arc<String>>(r"y\\"))
                   .chain(repeat(strish("")).take(4)
                          .chain(once(strish(r"𝞫 {\}}"))
                                 .chain(repeat(strish("")).take(2))))));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: 'y', pos: CharPos(0)}));
        assert_eq!(siss2.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: 'y', pos: CharPos(0)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: '\\', pos: CharPos(1)}));
        assert_eq!(siss2.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("y", 0)]));
        assert_eq!(siss2.next(), Some(SourceIterItem{ch: '\\', pos: CharPos(1)}));
        assert_eq!(siss2.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: '\\', pos: CharPos(2)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: '𝞫', pos: CharPos(3)}));
        assert_eq!(siss2.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: '𝞫', pos: CharPos(3)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: ' ', pos: CharPos(4)}));
        assert_eq!(siss2.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: ' ', pos: CharPos(4)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: '{', pos: CharPos(5)}));
        // Accum across strish boundary
        assert_eq!(siss2.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![(r"\", 2), ("𝞫 ", 3)]));
        assert_eq!(siss2.next(), Some(SourceIterItem{ch: '{', pos: CharPos(5)}));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: '\\', pos: CharPos(6)}));
        assert_eq!(siss2.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(siss2.next(), Some(SourceIterItem{ch: '\\', pos: CharPos(6)}));
        assert_eq!(siss2.next_accum(dda_arc_string),
                   Ok(Some(SourceIterItem{ch: '}', pos: CharPos(7)})));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2),
                   Some(&SourceIterItem{ch: '}', pos: CharPos(8)}));
        assert_eq!(siss2.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("}", 7)]));
        assert_eq!(siss2.next(), Some(SourceIterItem{ch: '}', pos: CharPos(8)}));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2), None);
        assert_eq!(siss2.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(siss2.next_accum(dda_arc_string), Ok(None));
        assert_eq!(siss2.accum_done(dda_arc_string).as_ref().map(txt_to_chunks),
                   Ok(vec![("", 0)]));
        assert_eq!(siss2.next(), None);
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss2), None);

        let mut siss3 = SISS::<_>::new(
            vec!["w", "V", "u"].into_iter().map(strish::<Rc<str>>));
        assert_eq!(siss3.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: 'w', pos: CharPos(0)})));
        assert_eq!(siss3.next_accum(dda_rc_str),
                   Ok(Some(SourceIterItem{ch: 'V', pos: CharPos(1)})));
        // next() after next_accum() loses the accumulation
        assert_eq!(siss3.next(), Some(SourceIterItem{ch: 'u', pos: CharPos(2)}));
        assert_eq!(siss3.accum_done(dda_rc_str).map(|t| t.is_empty()), Ok(true));
        assert_eq!(SourceStream::<DummyDA<_>>::peek(&mut siss3), None);
        assert_eq!(siss3.next_accum(dda_rc_str), Ok(None));
    }
}
