use std::marker::PhantomData;

use crate::{
    parser::{DatumAllocator, AllocError},
    datum::{DatumBox, BoxDatum},
    Text,
};


/// A [`DatumAllocator`] for generic `Datum` types which allocates `Datum`
/// values in heap-allocated `Box`es.
///
/// [`DatumAllocator`]: ../../kruvi_core/parser/trait.DatumAllocator.html
#[derive(Debug)]
pub struct BoxDatumAllocator<TT, ET>(PhantomData<(*const TT, *const ET)>);

/// Must implement this manually because deriving would place unwanted bounds on
/// the type parameters.
impl<TT, ET> Default for BoxDatumAllocator<TT, ET>
    where TT: Text,
{
    /// This type only has one, default, value.
    #[inline]
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<TT, ET> DatumAllocator for BoxDatumAllocator<TT, ET>
    where TT: Text,
{
    type TT = TT;
    type ET = ET;
    type DR = DatumBox<Self::TT, Self::ET>;

    #[inline]
    fn new_datum(&mut self, from: BoxDatum<Self::TT, Self::ET>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumBox::new(from))
    }
}


// Note: Tested by the box_alloc integration test.
