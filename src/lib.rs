//! Additional, more convenient, functionality, which leverages the Rust
//! standard library, layered on top of [`kruvi_core`].
//!
//! This crate:
//!
//! * Re-exports all of [`kruvi_core`].
//!
//! * Provides [`Datum`] reference types that wrap the standard [`Box`], [`Rc`],
//! and [`Arc`] types, for heap-allocating `Datum`s.
//!
//! * Avoids stack overflows (when possible) when dropping the provided
//! heap-allocated `Datum` types, so they can handle being used for very-deep
//! trees (e.g. long lists), by using a custom [`Drop`] implementation for them.
//! (Otherwise the compiler's default drop recursion could overflow.)
//!
//! * TODO: Provides an implementation of [`Parser`] that uses [`Box`] and has a
//! facility for establishing macro bindings.
//!
//! [`kruvi_core`]: ../kruvi_core/index.html
//! [`Datum`]: enum.Datum.html
//! [`Box`]: http://doc.rust-lang.org/std/boxed/struct.Box.html
//! [`Rc`]: http://doc.rust-lang.org/std/rc/struct.Rc.html
//! [`Arc`]: http://doc.rust-lang.org/std/sync/struct.Arc.html
//! [`Drop`]: http://doc.rust-lang.org/std/ops/trait.Drop.html
//! [`Parser`]: trait.Parser.html

use std::boxed::Box;
use std::rc::Rc;
use std::sync::Arc;
use std::ops::{Deref, DerefMut};

// Re-export everything from the core crate
pub use kruvi_core::*;


pub mod drop;


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
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
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
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
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
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Arc::get_mut(&mut this.0)
    }
}


// TODO: Parser impl that makes it easier by using Box and that provides API for
// establishing combiner bindings.


#[cfg(test)]
mod tests {
    use super::*;
    use kruvi_shared_tests::utils::*;

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

    // The following tests core functionality, equality comparison of `Datum`s
    // without overflowing the stack on `eq` recursion, which usually wouldn't
    // be done in this non-core crate, but we need to have heap allocation to
    // make the deep trees.  (This also exercises our `Drop` implementation that
    // avoids extensive drop recursion that would otherwise overflow the stack,
    // but that is tested primarily in the `drop` module.)

    #[test]
    fn deep_list_equality() {
        let len = list_len(get_arg_tree_size());

        let boxes = make_box_list(len);
        assert_eq!(boxes, boxes);

        let rcs = make_rc_list(len);
        assert_eq!(rcs, rcs);

        let arcs = make_arc_list(len);
        assert_eq!(arcs, arcs);

        assert_eq!(*boxes, *rcs);
        assert_eq!(*boxes, *arcs);
        assert_eq!(*rcs, *arcs);
    }

    #[test]
    fn deep_nest_equality() {
        let depth = nest_depth(get_arg_tree_size());

        let boxes = make_box_nest(depth);
        assert_eq!(boxes, boxes);

        let rcs = make_rc_nest(depth);
        assert_eq!(rcs, rcs);

        let arcs = make_arc_nest(depth);
        assert_eq!(arcs, arcs);

        assert_eq!(*boxes, *rcs);
        assert_eq!(*boxes, *arcs);
        assert_eq!(*rcs, *arcs);
    }

    #[test]
    fn deep_zigzag_equality() {
        let depth = zigzag_depth(get_arg_tree_size());

        let boxes = make_box_zigzag(depth);
        assert_eq!(boxes, boxes);

        let rcs = make_rc_zigzag(depth);
        assert_eq!(rcs, rcs);

        let arcs = make_arc_zigzag(depth);
        assert_eq!(arcs, arcs);

        assert_eq!(*boxes, *rcs);
        assert_eq!(*boxes, *arcs);
        assert_eq!(*rcs, *arcs);
    }

    // Note: This still causes stack overflows because the unusual zig-zag shape
    // still causes extensive `eq` call recursion.
    #[test]
    #[ignore]
    fn deep_unusual_zigzag_equality_overflow()
    {
        fn make_unusual_zigzag(depth: usize) -> DatumBox<'static, usize> {
            use super::Datum::*;
            make_zigzag(depth, &DatumBox::new,
                        || EmptyNest,
                        |elem, next| List{elem, next},
                        |operator, operands| Combination{operator, operands},
                        |cnt, new| new(Extra(cnt)),
                        |_, new| new(EmptyList))
        }

        let depth = zigzag_depth(get_arg_tree_size());
        let boxes = make_unusual_zigzag(depth);
        assert_eq!(boxes, boxes);
    }

    // Note: These fan shapes wouldn't be expected to cause stack overflows even
    // with a naive equality implementation because the depth isn't enough.
    #[test]
    fn deep_fan_equality() {
        let depth = fan_depth(get_arg_tree_size());

        let boxes = make_box_fan(depth);
        assert_eq!(boxes, boxes);

        let rcs = make_rc_fan(depth);
        assert_eq!(rcs, rcs);

        let arcs = make_arc_fan(depth);
        assert_eq!(arcs, arcs);

        assert_eq!(*boxes, *rcs);
        assert_eq!(*boxes, *arcs);
        assert_eq!(*rcs, *arcs);
    }

    #[test]
    fn deep_vee_equality() {
        let half_size = get_arg_tree_size() / 2;
        let (left_depth, right_depth) = vee_depths(half_size, half_size);

        let boxes = make_box_vee(left_depth, right_depth);
        assert_eq!(boxes, boxes);

        let rcs = make_rc_vee(left_depth, right_depth);
        assert_eq!(rcs, rcs);

        let arcs = make_arc_vee(left_depth, right_depth);
        assert_eq!(arcs, arcs);

        assert_eq!(*boxes, *rcs);
        assert_eq!(*boxes, *arcs);
        assert_eq!(*rcs, *arcs);
    }
}
