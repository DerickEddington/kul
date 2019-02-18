//! Errors that might be returned by parsing.

use crate::parser::AllocError;


/// The possible errors that might be returned by parsing.
///
/// It is generic over the `SourcePosition` and `CombinerError` type parameters
/// to enable different applications to customize what they use for these
/// general aspects.
#[derive(Copy, Clone, Eq, Debug)]
pub enum Error<SourcePosition, CombinerError> {
    /// Close-bracket without matching open-bracket
    UnbalancedEndChar(SourcePosition),
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


/// This allows different concrete [`Error`](enum.Error.html) types to be
/// compared with each other for equality if their [source position
/// types](enum.Error.html#variant.UnbalancedEndChar) and [combiner error
/// types](enum.Error.html#variant.FailedCombiner) can be.
#[allow(clippy::match_same_arms)]
impl<P1, P2, CE1, CE2> PartialEq<Error<P2, CE2>> for Error<P1, CE1>
    where P1: PartialEq<P2>,
          CE1: PartialEq<CE2>,
{
    fn eq(&self, other: &Error<P2, CE2>) -> bool {
        use Error::*;

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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality() {
        use Error::*;

        assert_eq!(UnbalancedEndChar::<_, ()>(()),
                   UnbalancedEndChar::<_, ()>(()));

        assert_eq!(MissingEndChar::<(), ()>, MissingEndChar::<(), ()>);

        assert_eq!(FailedAlloc::<(), ()>(AllocError::AllocExhausted),
                   FailedAlloc::<(), ()>(AllocError::AllocExhausted));

        assert_eq!(FailedDerefTryMut::<(), ()>, FailedDerefTryMut::<(), ()>);

        assert_eq!(FailedCombiner::<(), i32>(1), FailedCombiner::<(), i32>(1));
    }
}
