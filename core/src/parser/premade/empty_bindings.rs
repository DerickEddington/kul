use core::ops::{Deref, DerefMut};

use crate::parser::{OperatorBindings, DatumAllocator};
use crate::combiner::{self, Combiner};
use crate::{Datum, DerefTryMut, TextBase};


/// An [`OperatorBindings`](trait.OperatorBindings.html) that always has no
/// bindings and its [`lookup`](trait.OperatorBindings.html#tymethod.lookup)
/// method always returns `None`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct EmptyOperatorBindings;


/// Trick `OperatorBindings` into accepting this for the implementation of
/// it for `EmptyOperatorBindings`.
pub struct DummyCombiner<TT, ET, DR, POS, CE>(TT, ET, DR, POS, CE);


impl<TT, ET, DR, POS, CE> Deref for DummyCombiner<TT, ET, DR, POS, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{
    type Target = dyn FnMut(DR, DR) -> combiner::Result<TT, ET, DR, POS, CE>;
    fn deref(&self) -> &Self::Target { unreachable!() }
}


impl<TT, ET, DR, POS, CE> DerefMut for DummyCombiner<TT, ET, DR, POS, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{
    fn deref_mut(&mut self) -> &mut Self::Target { unreachable!() }
}


impl<DA> OperatorBindings<DA> for EmptyOperatorBindings
    where DA: DatumAllocator,
{
    type OR = DummyCombiner<DA::TT, DA::ET, DA::DR, <DA::TT as TextBase>::Pos, Self::CE>;
    type AR = DummyCombiner<DA::TT, DA::ET, DA::DR, <DA::TT as TextBase>::Pos, Self::CE>;
    type CE = ();

    #[inline]
    fn lookup(&mut self, _operator: &DA::DR) -> Option<Combiner<Self::OR, Self::AR>> {
        None
    }
}
