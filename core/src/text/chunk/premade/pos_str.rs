//! A `TextChunk` implementation for borrowed string slices (`&str`) that knows
//! what position in its original source string it is at.  This is useful for
//! zero-copy parsing of in-memory UTF-8 strings.

use core::str::CharIndices;
use core::iter::{self, Peekable, Map, Zip, Repeat, Enumerate};

use crate::text;
use crate::{TextBase, TextChunk, SourceIterItem, SourcePosition};


/// A `SourcePosition` type for character or slice values from text sources that
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

impl<'s> SourcePosition for StrPos<'s> {
    #[inline]
    fn empty() -> Self {
        StrPos {
            src: "",
            byte_pos: 0,
            char_pos: 0,
        }
    }
}


/// A `TextChunk` implementation for borrowed string slices (`&str`) that knows
/// what position in its original source string it is at.  This is useful for
/// zero-copy parsing of in-memory UTF-8 strings.
///
/// Note: The comparison traits are not implemented intentionally, because this
/// is intended as a chunk type and comparing those directly shouldn't be done.
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


/// A [`chunk::SourceStream`] (and `Iterator`) of the characters, and their
/// positions, of the text chunk that a `PosStr` represents.
///
/// The positions of the characters remain correct relative to the original
/// source string that the `PosStr`'s slice is from.  (I.e. not relative to the
/// slice.)
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


/// Required by `chunk::SourceStream`.
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


/// Enables `PosStr` to be used as the `Chunk` type of `Text` types that
/// implement `TextConcat` so they can be iterated with
/// `kruvi_core::SourceStream`.  A `chunk::SourceStream` can accumulate what it
/// iterates within a single chunk.  This simply records and grows a slice, to
/// represent each accumulation, which is zero-copy.
impl<'s> text::chunk::SourceStream<PosStr<'s>> for PosStrIter<'s>
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


#[cfg(test)]
mod tests {
    use super::*;

    // TODO

    #[test]
    fn strpos() {
        assert_eq!(StrPos::empty(), StrPos{src: "", byte_pos: 0, char_pos: 0});
    }
}
