//! Parts for iterating the chunks of a `Text`.  Designed to allow borrowing the
//! state from the `Text` instance so that the lifetime relates to that of
//! `Text` method calls.

use crate::{Text, TextChunk};


/// Enables a `Text` to supply references to each of its chunks by borrowing
/// them from its associated `Text::IterChunksState`, which must implement this
/// trait, which is borrowed by calls to the `Text` methods, which enables the
/// lifetimes to be related for those methods to work.
///
/// The implementing type must be able to transition to the next states by
/// borrowing its type from itself.  E.g. slices and linked lists can do this.
pub trait State {
    /// The `Text::Chunk` type we are for.
    type Chunk: TextChunk;

    /// Return the next chunk and the next state, or return `None` when
    /// finished.
    fn next(&self) -> Option<(&Self::Chunk, Option<&Self>)>;
}


/// Iterator that yields a reference to each chunk of a `Text`, by using the
/// [`State`](trait.State.html) trait of the [`Text::IterChunksState`](TODO).
#[derive(Debug)]
pub struct Iter<'l, TT>
    where TT: Text,
{
    state: Option<&'l TT::IterChunksState>,
}

impl<'l, TT> Iter<'l, TT>
    where TT: Text,
{
    /// Make a new one for a given `Text`.
    pub fn new(text: &'l TT) -> Self {
        Self {
            state: text.iter_chunks_state(),
        }
    }
}


// Note: Must implement `Clone` manually instead of using `derive`
// because `derive` would place additional bounds on the `TT` type parameter
// which must be avoided.

impl<TT> Clone for Iter<'_, TT>
    where TT: Text
{
    fn clone(&self) -> Self { Self {.. *self} }
}


impl<'l, TT> Iterator for Iter<'l, TT>
    where TT: Text,
          TT::Chunk: 'l,
{
    type Item = &'l TT::Chunk;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.take()
                  .and_then(State::next)
                  .map(|(next_chunk, next_state)| {
            self.state = next_state;
            next_chunk
        })
    }
}


/// Implementations provided for ready use.
// In the future, this might have more and need to be public.
mod premade {
    mod slice {
        use crate::text::iter::chunks::State;
        use crate::TextChunk;

        /// Implementation of [`State`](trait.State.html) for any generic slice
        /// of chunks.  Useful for `Text` types that represent their chunks this
        /// way.  E.g. [`TextVec`](TODO) uses this for its `Vec` representation;
        /// or this could be used for some fixed-size array.
        impl<C> State for [C]
            where C: TextChunk,
        {
            type Chunk = C;

            fn next(&self) -> Option<(&Self::Chunk, Option<&Self>)> {
                if self.is_empty() {
                    None
                } else {
                    Some((&self[0],
                          if self.len() > 1 {
                              Some(&self[1..])
                          } else {
                              None
                          }))
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    // TODO
}
