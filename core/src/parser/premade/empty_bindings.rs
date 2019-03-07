use core::ops::Deref;

use crate::parser::{OperatorBindings, DatumAllocator};
use crate::combiner::{Combiner, OpFn, ApFn};
use crate::Datum;


/// An [`OperatorBindings`](trait.OperatorBindings.html) that always has no
/// bindings and its [`lookup`](trait.OperatorBindings.html#tymethod.lookup)
/// method always returns `None`.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct EmptyOperatorBindings;


mod private {
    /// Trick `OperatorBindings` into accepting this for the implementation of
    /// it for `EmptyOperatorBindings`.
    #[derive(Debug)]
    pub struct DummyOperativeRef<DA, CE>(DA, CE);

    /// Trick `OperatorBindings` into accepting this for the implementation of
    /// it for `EmptyOperatorBindings`.
    #[derive(Debug)]
    pub struct DummyApplicativeRef<DA, CE>(DA, CE);
}
use private::*;

impl<DA, CE> Deref for DummyOperativeRef<DA, CE>
    where DA: DatumAllocator,
{
    type Target = OpFn<DA, CE>;
    fn deref(&self) -> &Self::Target { unreachable!() }
}

impl<DA, CE> Deref for DummyApplicativeRef<DA, CE>
    where DA: DatumAllocator,
{
    type Target = ApFn<DA, CE>;
    fn deref(&self) -> &Self::Target { unreachable!() }
}


impl<DA> OperatorBindings<DA> for EmptyOperatorBindings
    where DA: DatumAllocator,
{
    type OR = DummyOperativeRef<DA, Self::CE>;
    type AR = DummyApplicativeRef<DA, Self::CE>;
    type CE = ();

    #[inline]
    fn lookup(&self, _operator: &Datum<DA::TT, DA::ET, DA::DR>)
              -> Option<&Combiner<Self::OR, Self::AR>> {
        None
    }
}
