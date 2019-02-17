use core::ops::{Deref, DerefMut};

use crate::parser::{OperatorBindings, DatumAllocator};
use crate::combiner::{Combiner, OpFn};
use crate::Datum;


/// An [`OperatorBindings`](trait.OperatorBindings.html) that always has no
/// bindings and its [`lookup`](trait.OperatorBindings.html#tymethod.lookup)
/// method always returns `None`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct EmptyOperatorBindings;


/// Trick `OperatorBindings` into accepting this for the implementation of
/// it for `EmptyOperatorBindings`.
#[derive(Debug)]
pub struct DummyCombiner<DA, CE>(DA, CE);


impl<DA, CE> Deref for DummyCombiner<DA, CE>
    where DA: DatumAllocator,
{
    type Target = OpFn<DA, CE>;
    fn deref(&self) -> &Self::Target { unreachable!() }
}


impl<DA, CE> DerefMut for DummyCombiner<DA, CE>
    where DA: DatumAllocator,
{
    fn deref_mut(&mut self) -> &mut Self::Target { unreachable!() }
}


impl<DA> OperatorBindings<DA> for EmptyOperatorBindings
    where DA: DatumAllocator,
{
    type OR = DummyCombiner<DA, Self::CE>;
    type AR = DummyCombiner<DA, Self::CE>;
    type CE = ();

    #[inline]
    fn lookup(&mut self, _operator: &Datum<DA::TT, DA::ET, DA::DR>)
              -> Option<Combiner<Self::OR, Self::AR>> {
        None
    }
}
