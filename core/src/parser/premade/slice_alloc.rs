use crate::{
    datum::premade::{MutRefDatum, DatumMutRef},
    text::Text,
    parser::{DatumAllocator, AllocError},
};


/// Mutable slice of generic `MutRefDatum` elements which reference each other
/// with mutable borrows of the same lifetime.
type Slice<'a, TT, ET> = &'a mut [MutRefDatum<'a, TT, ET>];

/// A [`DatumAllocator`] for generic `Datum` types which simplistically
/// allocates `Datum` values from a contiguous mutable slice of them by
/// supplying mutable borrows of them.  Once a `Datum` element has been
/// allocated, it is not reused if dropped.
///
/// This is useful for either or both of:
///
/// - Allocating from stack arrays in constrained environments without heap
/// allocation.
///
/// - Limiting the memory consumed by parsing, e.g. to prevent D.o.S. attacks.
/// Heap-allocated arrays/slices could be used for this.
///
/// Allocated elements are not reused if dropped because that would require
/// implementing `Drop` for the element type which would prevent being able to
/// use basic arrays/slices (because the `Datum` elements reference each other
/// with the same lifetime, and this only works for element types without
/// `Drop`).  To reuse a slice, a new allocator instance can be constructed with
/// it again, once the prior parsing and borrows of the elements are finished.
///
/// [`DatumAllocator`]: ../trait.DatumAllocator.html
#[derive(Debug)]
pub struct SliceDatumAllocator<'a, TT, ET> {
    free: Option<Slice<'a, TT, ET>>,
}

impl<'a, TT, ET> SliceDatumAllocator<'a, TT, ET>
    where TT: Text,
{
    /// Given a mutable slice of `Datum`s of our type, make a new allocator from
    /// it.
    pub fn new(slice: Slice<'a, TT, ET>) -> Self {
        Self { free: if slice.is_empty() { None } else { Some(slice) } }
    }

    /// Can be used to regain control of the borrow of the unused rest of the
    /// slice, so that it can be used for something else if desired.
    pub fn unused(self) -> Option<Slice<'a, TT, ET>> {
        self.free
    }
}

impl<'a, TT, ET> DatumAllocator for SliceDatumAllocator<'a, TT, ET>
    where TT: Text,
{
    type TT = TT;
    type ET = ET;
    type DR = DatumMutRef<'a, Self::TT, Self::ET>;

    fn new_datum(&mut self, from: MutRefDatum<'a, Self::TT, Self::ET>)
                 -> Result<Self::DR, AllocError>
    {
        match self.free.take().and_then(<[_]>::split_first_mut) {
            Some((dr, rest)) => {
                *dr = from;
                self.free = Some(rest);
                Ok(DatumMutRef(dr))
            }
            None => Err(AllocError::AllocExhausted)
        }
    }
}


// Note: Tested by the mutref_stack_alloc integration test.
