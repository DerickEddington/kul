use std::marker::PhantomData;

use crate::{
    parser::{DatumAllocator, AllocError},
    datum::{DatumRc, RcDatum},
    Text,
};


/// A [`DatumAllocator`] for generic `Datum` types which allocates `Datum`
/// values in heap-allocated `Rc`s.
///
/// [`DatumAllocator`]: ../../kul_core/parser/trait.DatumAllocator.html
#[derive(Debug)]
pub struct RcDatumAllocator<TT, ET>(PhantomData<(*const TT, *const ET)>);

/// Must implement this manually because deriving would place unwanted bounds on
/// the type parameters.
impl<TT, ET> Default for RcDatumAllocator<TT, ET>
    where TT: Text,
{
    /// This type only has one, default, value.
    #[inline]
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<TT, ET> DatumAllocator for RcDatumAllocator<TT, ET>
    where TT: Text,
{
    type TT = TT;
    type ET = ET;
    type DR = DatumRc<Self::TT, Self::ET>;

    #[inline]
    fn new_datum(&mut self, from: RcDatum<Self::TT, Self::ET>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumRc::new(from))
    }
}


// Note: Tested by the rc_alloc integration test.
