//! The `Datum`-reference type that uses basic mutable borrow references.

use core::ops::{Deref, DerefMut};

use crate::{Datum, DerefTryMut};


/// This assists in basic direct mutable borrow references being used as the
/// `Datum` reference type.
pub type MutRefDatum<'d, TT, ET> = Datum<TT, ET, DatumMutRef<'d, TT, ET>>;

/// The `Datum`-reference type that uses basic mutable borrow references.
/// Intended to be the element of arrays used to allocate `Datum`s for parsing
/// when heap allocation is unavailable (or undesired).
///
/// This must not implement `Drop`.  This wrapper allows the needed recursive
/// type definition.
#[derive(PartialEq, Eq, Hash, Debug)]
pub struct DatumMutRef<'d, TT, ET>(pub &'d mut MutRefDatum<'d, TT, ET>);

/// Required by `DerefTryMut`.
impl<'d, TT, ET> Deref for DatumMutRef<'d, TT, ET> {
    type Target = MutRefDatum<'d, TT, ET>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Not strictly required but useful.
impl<TT, ET> DerefMut for DatumMutRef<'_, TT, ET> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// This allows basic direct mutable borrow references to be used as the `Datum`
/// reference type.
impl<TT, ET> DerefTryMut for DatumMutRef<'_, TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(this)
    }
}
