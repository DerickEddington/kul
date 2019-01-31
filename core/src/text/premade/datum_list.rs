//! A generic `Text` implementation for chaining underlying chunks by using a
//! linked list of `Datum`s provided by a `DatumAllocator`.  This is useful when
//! heap allocation isn't available (or desired) and a `Parser`'s
//! `DatumAllocator` is the only available (or desired) dynamic allocator.

use core::ops::Deref;
use core::cmp::Ordering;

use crate::text;
use crate::{Text, TextBase, TextChunk, TextConcat, Datum, DerefTryMut};
use crate::datum::premade::DatumMutRef;
use crate::parser::{DatumAllocator, AllocError};


/// A generic `Text` implementation for chaining underlying chunks by using a
/// linked list of `Datum`s provided by a `DatumAllocator`.  The chunk type can
/// be any `TextChunk`.  This is useful when heap allocation isn't available (or
/// desired) and a `Parser`'s `DatumAllocator` is the only available (or
/// desired) dynamic allocator.
///
/// It is a `TextConcat` and so can be used with `Parser`s as the text type of
/// `Datum`s, where a `Parser` allocates the same type of `Datum` as this
/// references as its links.
///
/// [`DatumMutRef`] is used as the `Datum` reference type used as the links,
/// because it is the type for allocating `Datum`s from arrays instead of the
/// heap.  This effectively restricts the genericity of what `Datum` types can
/// be used to only being generic over the "extra" type, which is fine because
/// this isn't intended for any other types.
#[derive(Debug)]
pub struct TextDatumList<'d, C, ET> {
    chunk: C,
    next: Option<DatumMutRef<'d, Self, ET>>,
}


impl<'d, C, ET> From<C> for TextDatumList<'d, C, ET>
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


impl<'d, TT, C, ET> PartialEq<TT> for TextDatumList<'d, C, ET>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn eq(&self, other: &TT) -> bool {
        Text::partial_eq(self, other)
    }
}

impl<'d, C, ET> Eq for TextDatumList<'d, C, ET>
    where C: TextChunk,
{}

impl<'d, TT, C, ET> PartialOrd<TT> for TextDatumList<'d, C, ET>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn partial_cmp(&self, other: &TT) -> Option<Ordering> {
        Text::partial_cmp(self, other)
    }
}

impl<'d, C, ET> Ord for TextDatumList<'d, C, ET>
    where C: TextChunk,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        Text::cmp(self, other)
    }
}


/// Enables iterating the chunks of a `TextDatumList`, by traversing the linked
/// list of them.
///
/// This type is its own `iter::chunks::State` because the links are contained
/// in it.
impl<'d, C, ET> text::iter::chunks::State for TextDatumList<'d, C, ET>
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


impl<'d, C, ET> TextBase for TextDatumList<'d, C, ET>
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

impl<'d, C, ET> Text for TextDatumList<'d, C, ET>
    where C: TextChunk,
{
    type Chunk = C;
    type IterChunksState = Self;

    #[inline]
    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState> {
        Some(self)
    }
}


/// Enables `TextDatumList` to be used with `Parser`s as the produced `Datum`s'
/// text type.  This can only be used with `Parser`s that allocate the same type
/// of `Datum` that `Self` uses, which is required by this `impl`'s `where`
/// bounds.
impl<'d, DA, C, ET> TextConcat<DA> for TextDatumList<'d, C, ET>
    where C: TextChunk,
          DA: DatumAllocator<TT = Self, ET = ET, DR = DatumMutRef<'d, Self, ET>>
              + ?Sized,
{
    /// Link two `TextDatumList`s to form a single `TextDatumList` that
    /// represents their logical concatenation.  Unlike most implementations of
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


#[cfg(test)]
mod tests {
    // TODO
}
