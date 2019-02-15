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
pub struct DummyCombiner<TT, ET, DR, Pos, CE>(TT, ET, DR, Pos, CE);


impl<TT, ET, DR, Pos, CE> Deref for DummyCombiner<TT, ET, DR, Pos, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{
    type Target = dyn FnMut(Datum<TT, ET, DR>, Datum<TT, ET, DR>)
                            -> combiner::Result<TT, ET, DR, Pos, CE>;
    fn deref(&self) -> &Self::Target { unreachable!() }
}


impl<TT, ET, DR, Pos, CE> DerefMut for DummyCombiner<TT, ET, DR, Pos, CE>
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
    fn lookup(&mut self, _operator: &Datum<DA::TT, DA::ET, DA::DR>)
              -> Option<Combiner<Self::OR, Self::AR>> {
        None
    }
}
