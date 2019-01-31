//! A generic `Text` implementation for chaining underlying chunks by using a
//! `Vec`.

use crate::{Text, TextChunk, TextBase, TextConcat};
use crate::parser::{DatumAllocator, AllocError};


/// A representation of texts that uses a `Vec` of chunks, that can work with
/// any [`TextChunk`] type, and that is a [`TextConcat`] that can be used with
/// [`Parser`s].
#[derive(Clone, Debug)]
pub struct TextVec<C> {
    chunks: Vec<C>,
}


impl<C> From<C> for TextVec<C>
    where C: TextChunk,
{
    #[inline]
    fn from(chunk: C) -> Self {
        Self {
            chunks: vec![chunk],
        }
    }
}


impl<TT, C> PartialEq<TT> for TextVec<C>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn eq(&self, other: &TT) -> bool {
        Text::partial_eq(self, other)
    }
}

impl<C> Eq for TextVec<C>
    where C: TextChunk,
{}

// TODO: PartialOrd, Ord


impl<C> TextBase for TextVec<C>
    where C: TextChunk,
{
    type Pos = C::Pos;

    #[inline]
    fn empty() -> Self {
        Self {
            chunks: vec![],
        }
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.chunks.iter().all(TextBase::is_empty)
    }
}

impl<C> Text for TextVec<C>
    where C: TextChunk,
{
    type Chunk = C;
    // Relies on `kruvi_core::text::iter::chunks::premade::slice`
    type IterChunksState = [Self::Chunk];

    #[inline]
    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState> {
        Some(&self.chunks[..])
    }

}


/// Enables `TextVec` to be used with `Parser`s as the produced `Datum`s' text
/// type.
impl<C, DA> TextConcat<DA> for TextVec<C>
    where C: TextChunk,
          DA: DatumAllocator<TT = Self>,
{
    fn concat(mut self, mut other: Self, _: &mut DA) -> Result<Self, AllocError> {
        self.chunks.append(&mut other.chunks);
        Ok(self)
    }
}


#[cfg(test)]
mod tests {
    // TODO
}
