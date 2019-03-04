//! Parts for iterating the characters, and their positions, of a `Text`.
//! Designed to allow the state to borrow from the `Text` instance so that the
//! lifetime relates to that of `Text` method calls.

use core::fmt;

use crate::{Text, TextChunk, TextConcat, SourceIterItem, SourceStream};
use crate::text::chunk::SourceStream as ChunkSourceStream;
use crate::parser::{DatumAllocator, AllocError};


pub mod chunks;


/// A [`kul_core::SourceStream`] (which is also an `Iterator`) of the logical
/// sequence of characters (with positions) of any [`Text`], yielded as
/// [`SourceIterItem`] items.  This is designed to handle generic chunk-chain
/// representations.
pub struct Iter<'l, TT>
    where TT: Text,
{
    /// The `text::chunk::SourceStream` (and so also `Iterator`) for the current
    /// chunk.
    cur_chunk_src_strm: Option<<TT::Chunk as TextChunk>::CharsSrcStrm>,
    /// `Iterator` of the next chunks.
    next_chunks_iter: chunks::Iter<'l, TT>,
    /// Accumulated chunks formed and concatenated by our
    /// `SourceStream::next_accum` and `SourceStream::accum_done`.
    accum: Option<TT>,
    /// Peeked next item of our `SourceStream::peek`.
    peeked: Option<SourceIterItem<TT::Pos>>,
}

/// Manually implemented because deriving it doesn't work.
impl<TT> fmt::Debug for Iter<'_, TT>
    where TT: Text,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::Iter", module_path!())
    }
}

impl<'l, TT> Iter<'l, TT>
    where TT: Text,
{
    /// Make a new one for a given `Text`.
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
impl<'l, TT> Iterator for Iter<'l, TT>
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
impl<'l, TT, DA> SourceStream<DA> for Iter<'l, TT>
    where TT: TextConcat<DA>,
          TT::Chunk: 'l,
          DA: DatumAllocator<TT = TT> + ?Sized,
{
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if let Some(ref it) = self.peeked {
            return Some(it)
        }
        // Must use a copy of the state to avoid changing `self`'s state because
        // that state affects the pending accumulation state which must not
        // change for a peek.
        let mut cur_chunk_src_strm;
        let mut cur_chunk_src_strm_ref = &mut self.cur_chunk_src_strm;
        let mut next_chunks_iter = self.next_chunks_iter.clone();
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

    fn accum_done(&mut self, dalloc: &mut DA) -> Result<TT, AllocError> {
        let mut accum = self.accum.take().unwrap_or_else(TT::empty);
        if let Some(ccss) = &mut self.cur_chunk_src_strm {
            let chunk_ended = ccss.accum_done().into();
            accum = accum.concat(chunk_ended, dalloc)?;
        }
        Ok(accum)
    }
}


#[cfg(test)]
mod tests {
    // TODO
}
