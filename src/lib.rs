//! Additional, more convenient, functionality, which leverages the Rust
//! standard library, layered on top of [`kruvi_core`].
//!
//! This crate:
//!
//! * Re-exports all of [`kruvi_core`].
//!
//! * Provides [`Datum`] reference types for [`Box`], [`Rc`], and [`Arc`].
//!
//! * TODO: Provides an implementation of [`Parser`] that uses [`Box`] and has a
//! facility for establishing macro bindings.
//!
//! [`kruvi_core`]: ../kruvi_core/index.html
//! [`Datum`]: enum.Datum.html
//! [`Box`]: http://doc.rust-lang.org/std/boxed/struct.Box.html
//! [`Rc`]: http://doc.rust-lang.org/std/rc/struct.Rc.html
//! [`Arc`]: http://doc.rust-lang.org/std/sync/struct.Arc.html
//! [`Parser`]: trait.Parser.html

use std::boxed::Box;
use std::rc::Rc;
use std::sync::Arc;
use std::ops::{Deref, DerefMut};

// Re-export everything from the core crate
pub use kruvi_core::*;


/// This assists in `Box` being used as the `Datum` reference type.
pub type BoxDatum<'s, ET> = Datum<'s, ET, DatumBox<'s, ET>>;

/// This wrapper allows the needed recursive type definition for `Box` to be
/// used as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DatumBox<'s, ET>(pub Box<BoxDatum<'s, ET>>);

impl<'s, ET> DatumBox<'s, ET> {
    pub fn new(val: BoxDatum<'s, ET>) -> Self {
        DatumBox(Box::new(val))
    }
}

impl<'s, ET> Deref for DatumBox<'s, ET> {
    type Target = BoxDatum<'s, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

impl<'s, ET> DerefMut for DatumBox<'s, ET> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        DerefMut::deref_mut(&mut self.0)
    }
}

/// This allows `Box` to be used as the `Datum` reference type.
impl<'s, ET> DerefTryMut for DatumBox<'s, ET> {
    type SizedTarget = Self::Target;

    fn get_mut(this: &mut Self) -> Option<&mut Self::SizedTarget> {
        Some(DerefMut::deref_mut(this))
    }
}


/// This assists in `Rc` being used as the `Datum` reference type.
pub type RcDatum<'s, ET> = Datum<'s, ET, DatumRc<'s, ET>>;

/// This wrapper allows the needed recursive type definition for `Rc` to be used
/// as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DatumRc<'s, ET>(pub Rc<RcDatum<'s, ET>>);

impl<'s, ET> DatumRc<'s, ET> {
    pub fn new(val: RcDatum<'s, ET>) -> Self {
        DatumRc(Rc::new(val))
    }
}

impl<'s, ET> Deref for DatumRc<'s, ET> {
    type Target = RcDatum<'s, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

/// This allows `Rc` to be used as the `Datum` reference type.
impl<'s, ET> DerefTryMut for DatumRc<'s, ET> {
    type SizedTarget = Self::Target;

    fn get_mut(this: &mut Self) -> Option<&mut Self::SizedTarget> {
        Rc::get_mut(&mut this.0)
    }
}


/// This assists in `Arc` being used as the `Datum` reference type.
pub type ArcDatum<'s, ET> = Datum<'s, ET, DatumArc<'s, ET>>;

/// This wrapper allows the needed recursive type definition for `Arc` to be
/// used as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DatumArc<'s, ET>(pub Arc<ArcDatum<'s, ET>>);

impl<'s, ET> DatumArc<'s, ET> {
    pub fn new(val: ArcDatum<'s, ET>) -> Self {
        DatumArc(Arc::new(val))
    }
}

impl<'s, ET> Deref for DatumArc<'s, ET> {
    type Target = ArcDatum<'s, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

/// This allows Arc` to be used as the `Datum` reference type.
impl<'s, ET> DerefTryMut for DatumArc<'s, ET> {
    type SizedTarget = Self::Target;

    fn get_mut(this: &mut Self) -> Option<&mut Self::SizedTarget> {
        Arc::get_mut(&mut this.0)
    }
}


// TODO: Parser impl that makes it easier by using Box and that provides API for
// establishing combiner bindings.


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn datum_equality() {
        use super::Datum::*;

        assert_eq!(DatumBox::new(EmptyNest::<u32, DatumBox<u32>>),
                   DatumBox::new(EmptyNest::<u32, DatumBox<u32>>));

        assert_ne!(DatumBox::new(EmptyNest::<(), DatumBox<()>>),
                   DatumBox::new(EmptyList::<(), DatumBox<()>>));

        assert_eq!(DatumRc::new(EmptyNest::<bool, DatumRc<bool>>),
                   DatumRc::new(EmptyNest::<bool, DatumRc<bool>>));

        assert_ne!(DatumRc::new(EmptyNest::<(), DatumRc<()>>),
                   DatumRc::new(EmptyList::<(), DatumRc<()>>));

        assert_eq!(DatumArc::new(EmptyNest::<char, DatumArc<char>>),
                   DatumArc::new(EmptyNest::<char, DatumArc<char>>));

        assert_ne!(DatumArc::new(EmptyNest::<(), DatumArc<()>>),
                   DatumArc::new(EmptyList::<(), DatumArc<()>>));
    }

    #[test]
    fn datum_clone() {
        use super::Datum::*;

        let a = List::<(), DatumBox<()>>{
            elem: DatumBox::new(EmptyNest::<(), DatumBox<()>>),
            next: DatumBox::new(EmptyList::<(), DatumBox<()>>)};
        let b = a.clone();
        assert_eq!(a, b);

        let c = List::<(), DatumRc<()>>{
            elem: DatumRc::new(EmptyNest::<(), DatumRc<()>>),
            next: DatumRc::new(EmptyList::<(), DatumRc<()>>)};
        let d = c.clone();
        assert_eq!(c, d);

        let e = List::<(), DatumArc<()>>{
            elem: DatumArc::new(EmptyNest::<(), DatumArc<()>>),
            next: DatumArc::new(EmptyList::<(), DatumArc<()>>)};
        let f = e.clone();
        assert_eq!(e, f);
    }
}
