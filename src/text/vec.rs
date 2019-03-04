//! A generic `Text` implementation for chaining underlying chunks by using a
//! `Vec`.

use std::{slice, cmp::Ordering, hash::{Hash, Hasher}};

use crate::{Text, TextChunk, TextBase, TextConcat};
use crate::parser::AllocError;

use Repr::{Single, Multi};


/// A representation of texts that uses a `Vec` of chunks, that can work with
/// any [`TextChunk`] type, and that is a [`TextConcat`] that can be used with
/// [`Parser`s].
///
/// When it contains only a single chunk, the chunk is directly contained
/// instead of using a `Vec`.
#[derive(Clone, Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct TextVec<C>(Repr<C>);

#[derive(Clone, Debug)]
enum Repr<C> {
    Single(C),
    Multi(Vec<C>),
}


impl<C> From<C> for TextVec<C>
    where C: TextChunk,
{
    #[inline]
    fn from(chunk: C) -> Self {
        Self(Single(chunk))
    }
}


impl<TT, C> PartialEq<TT> for TextVec<C>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn eq(&self, other: &TT) -> bool {
        Text::eq(self, other)
    }
}

impl<C> Eq for TextVec<C>
    where C: TextChunk,
{}

impl<TT, C> PartialOrd<TT> for TextVec<C>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn partial_cmp(&self, other: &TT) -> Option<Ordering> {
        Some(Text::cmp(self, other))
    }
}

impl<C> Ord for TextVec<C>
    where C: TextChunk,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        Text::cmp(self, other)
    }
}

impl<C> Hash for TextVec<C>
    where C: TextChunk,
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        Text::hash(self, state)
    }
}


impl<C> TextBase for TextVec<C>
    where C: TextChunk,
{
    type Pos = C::Pos;

    #[inline]
    fn empty() -> Self {
        Self(Multi(vec![])) // Empty Vec doesn't allocate
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.iter_chunks().all(TextBase::is_empty)
    }
}

impl<C> Text for TextVec<C>
    where C: TextChunk,
{
    type Chunk = C;
    // Relies on `kul_core::text::iter::chunks::premade::slice`
    type IterChunksState = [Self::Chunk];

    #[inline]
    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState> {
        Some(match &self.0 {
            Single(chunk) => slice::from_ref(chunk),
            Multi(chunks) => chunks,
        })
    }

}


/// Enables `TextVec` to be used with `Parser`s as the produced `Datum`s' text
/// type.
impl<C, DA> TextConcat<DA> for TextVec<C>
    where C: TextChunk,
{
    fn concat(mut self, mut other: Self, _: &mut DA) -> Result<Self, AllocError> {
        // If either is empty, optimize.
        if self.is_empty() {
            return Ok(other);
        } else if other.is_empty() {
            return Ok(self)
        }

        match self.0 {
            Single(chunk) =>
                self = match other.0 {
                    Single(other_chunk) =>
                        Self(Multi(vec![chunk, other_chunk])),
                    Multi(ref mut other_chunks) => {
                        other_chunks.insert(0, chunk);
                        other
                    },
                },
            Multi(ref mut chunks) =>
                match other.0 {
                    Single(other_chunk) =>
                        chunks.push(other_chunk),
                    Multi(mut other_chunks) =>
                        chunks.append(&mut other_chunks),
                },
        }
        Ok(self)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::text::chunk::PosStr;

    type TV = TextVec<PosStr<'static>>;

    fn concat(head: TV, tail: TV) -> TV {
        head.concat(tail, &mut ()).unwrap()
    }

    #[test]
    fn basic() {
        assert!(TV::empty().is_empty());
        assert!(concat(TV::empty(), TV::empty()).is_empty());
        assert!(TV::from_str("").is_empty());
        assert!(concat(TV::from_str(""), TV::from_str("")).is_empty());
        assert!(concat(TV::from_str(""), TV::empty()).is_empty());
        assert!(concat(TV::empty(), TV::from_str("")).is_empty());
        assert!(concat(TV::from_str(""), concat(TV::from_str(""), TV::from_str("")))
                .is_empty());
        assert_eq!(TV::empty(), TV::empty());
        assert_eq!(TV::from_str(""), TV::from_str(""));
        assert_eq!(TV::empty(), TV::from_str(""));
        assert_eq!(TV::from_str("a"), TV::from_str("a"));
        assert_ne!(TV::empty(), TV::from_str("a"));
        assert_ne!(TV::from_str("a"), TV::from_str("b"));
        assert_ne!(TV::from_str("aa"), TV::from_str("aaa"));
        assert_eq!(concat(TV::from_str("cc"), TV::from_str("d")),
                   TV::from_str("ccd"));
        assert_eq!(concat(concat(TV::from_str("e"), TV::from_str("  ")),
                          TV::from_str("fff")),
                   TV::from_str("e  fff"));
        assert_eq!(concat(TV::from_str("gg"),
                          concat(concat(TV::empty(), TV::from_str("")),
                                 TV::from_str("hh"))),
                   TV::from_str("gghh"));
        assert_eq!(concat(concat(TV::from_str(""), TV::from_str("i")), TV::from_str("")),
                   TV::from_str("i"));
        assert_ne!(concat(TV::from_str("j"), TV::from_str("kk")),
                   TV::from_str("lm"));
        assert_ne!(concat(TV::from_str("n"), concat(TV::from_str("o o"),
                                                    TV::from_str("p"))),
                   TV::from_str("qrs"));
    }
}
