use std::marker::PhantomData;

use crate::{
    parser::{DatumAllocator, AllocError},
    datum::{DatumArc, ArcDatum},
    Text,
};


/// A [`DatumAllocator`] for generic `Datum` types which allocates `Datum`
/// values in heap-allocated `Arc`s.
///
/// [`DatumAllocator`]: ../../kruvi_core/parser/trait.DatumAllocator.html
#[derive(Debug)]
pub struct ArcDatumAllocator<TT, ET>(PhantomData<(*const TT, *const ET)>);

/// Must implement this manually because deriving would place unwanted bounds on
/// the type parameters.
impl<TT, ET> Default for ArcDatumAllocator<TT, ET>
    where TT: Text,
{
    /// This type only has one, default, value.
    #[inline]
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<TT, ET> DatumAllocator for ArcDatumAllocator<TT, ET>
    where TT: Text,
{
    type TT = TT;
    type ET = ET;
    type DR = DatumArc<Self::TT, Self::ET>;

    #[inline]
    fn new_datum(&mut self, from: ArcDatum<Self::TT, Self::ET>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumArc::new(from))
    }
}


// Note: Tested by the arc_alloc integration test.
