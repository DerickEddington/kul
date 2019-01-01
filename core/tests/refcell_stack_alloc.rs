#![no_std]

use core::cell::{RefCell, RefMut};
use core::ops::{Deref, DerefMut};
use core::mem::ManuallyDrop;

use kruvi_core::*;
use kruvi_core::Datum::*;
use kruvi_shared_tests::suites::*;


/// This assists in `RefMut`s of `RefCell`s being used as the `Datum` reference
/// type.
pub type RefMutDatum<'d, 's, ET> = Datum<'s, ET, DatumRefMut<'d, 's, ET>>;

/// This wrapper allows the needed recursive type definition for `RefMut`s of
/// `RefCell`s to be used as the `Datum` reference type.
#[derive(Debug)]
pub struct DatumRefMut<'d, 's, ET>(pub RefMut<'d, RefMutDatum<'d, 's, ET>>);

impl<'d, 's, ET> Deref for DatumRefMut<'d, 's, ET>
    where 's: 'd
{
    type Target = RefMutDatum<'d, 's, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

impl<'d, 's, ET> DerefMut for DatumRefMut<'d, 's, ET>
    where 's: 'd
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        DerefMut::deref_mut(&mut self.0)
    }
}

/// This allows `RefMut`s of `RefCell`s to be used as the `Datum` reference type.
impl<'d, 's, ET> DerefTryMut for DatumRefMut<'d, 's, ET>
    where 's: 'd,
{
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(DerefMut::deref_mut(this))
    }
}

#[derive(Debug)]
struct ParserRefCell<'a> {
    datum_array_free: &'a [ManuallyDrop<RefCell<RefMutDatum<'a, 'static, ()>>>]
}

impl<'a> ParserRefCell<'a> {
    fn new(arr: &'a [ManuallyDrop<RefCell<RefMutDatum<'a, 'static, ()>>>])
           -> Self
    {
        ParserRefCell {datum_array_free: arr}
    }
}

impl<'a> Parser<'static> for ParserRefCell<'a> {
    type AS = ();
    type ET = ();
    type DR = DatumRefMut<'a, 'static, Self::ET>;
    // Note: OR and DR are not actually used for this test case
    type OR = &'a mut OpFn<'static, Self::ET, Self::DR, Self::CE, Self::AS>;
    type AR = &'a mut ApFn<'static, Self::ET, Self::DR, Self::CE, Self::AS>;
    type CE = ();

    fn supply_alloc_state(&mut self) -> Self::AS { () }

    fn receive_alloc_state(&mut self, _: Self::AS) { }

    fn env_lookup(&mut self, _: &Self::DR)
                  -> Option<Combiner<Self::OR, Self::AR>>
    {
        None
    }

    fn new_datum(&mut self, from: Datum<'static, Self::ET, Self::DR>, _: Self::AS)
                 -> Result<(Self::DR, Self::AS), AllocError>
    {
        match self.datum_array_free.split_first() {
            Some((refcell, rest)) => {
                let mut dr = refcell.borrow_mut();
                *dr = from;
                self.datum_array_free = rest;
                Ok((DatumRefMut(dr), ()))
            }
            None => Err(AllocError::AllocExhausted)
        }
    }
}

#[test]
fn suites() {
    // This uses `ManuallyDrop` to prevent Rust from running the `drop`
    // method from the `RefMut`s contained in the `Datum`s.  Not running
    // them in our case is ok because we know that the `RefMut`s only refer
    // to the `RefCell`s in the same array, and since the entire array is
    // being dropped at once, the borrow tracking that the `drop` method
    // deals with is not actually needed at that point.  We must prevent the
    // `drop` method from being used because otherwise this would cause a
    // compiler error about the array being dropped while still borrowed
    // (because the `drop` method would try to access the `RefCell` elements
    // which have already been destroyed, as the elements are iteratively
    // destroyed).
    let datum_array: [ManuallyDrop<RefCell<RefMutDatum<'_, 'static, ()>>>; 256] = [ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(()))), ManuallyDrop::new(RefCell::new(Extra(())))];

    let mut p = ParserRefCell::new(&datum_array);
    test_suite0(&mut p);
}
