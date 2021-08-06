//! `Datum` reference types for heap-allocating `Datum`s.  Also re-exports the
//! core crate's module and premades.

#![allow(clippy::module_name_repetitions)]

use std::boxed::Box;
use std::rc::Rc;
use std::sync::Arc;
use std::ops::{Deref, DerefMut};

// Re-export everything from the core mod.
#[doc(no_inline)]
pub use kul_core::datum::{*, premade::*};


/// This assists in `Box` being used as the `Datum` reference type.
pub type BoxDatum<TT, ET> = Datum<TT, ET, DatumBox<TT, ET>>;

/// This wrapper allows the needed recursive type definition for `Box` to be
/// used as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct DatumBox<TT, ET>(pub Box<BoxDatum<TT, ET>>);

impl<TT, ET> DatumBox<TT, ET> {
    /// Make a `Box`ed `Datum` with value from the given argument.
    pub fn new(val: BoxDatum<TT, ET>) -> Self {
        Self(Box::new(val))
    }
}

/// Required by `DerefTryMut`.
impl<TT, ET> Deref for DatumBox<TT, ET> {
    type Target = BoxDatum<TT, ET>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Not strictly required but useful.
impl<TT, ET> DerefMut for DatumBox<TT, ET> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// This allows `Box` to be used as the `Datum` reference type.
impl<TT, ET> DerefTryMut for DatumBox<TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(this)
    }
}


/// This assists in `Rc` being used as the `Datum` reference type.
pub type RcDatum<TT, ET> = Datum<TT, ET, DatumRc<TT, ET>>;

/// This wrapper allows the needed recursive type definition for `Rc` to be used
/// as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct DatumRc<TT, ET>(pub Rc<RcDatum<TT, ET>>);

impl<TT, ET> DatumRc<TT, ET> {
    /// Make an `Rc`ed `Datum` with value from the given argument.
    pub fn new(val: RcDatum<TT, ET>) -> Self {
        Self(Rc::new(val))
    }
}

/// Required by `DerefTryMut`.
impl<TT, ET> Deref for DatumRc<TT, ET> {
    type Target = RcDatum<TT, ET>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// This allows `Rc` to be used as the `Datum` reference type.
impl<TT, ET> DerefTryMut for DatumRc<TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Rc::get_mut(&mut this.0)
    }
}


/// This assists in `Arc` being used as the `Datum` reference type.
pub type ArcDatum<TT, ET> = Datum<TT, ET, DatumArc<TT, ET>>;

/// This wrapper allows the needed recursive type definition for `Arc` to be
/// used as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct DatumArc<TT, ET>(pub Arc<ArcDatum<TT, ET>>);

impl<TT, ET> DatumArc<TT, ET> {
    /// Make an `Arc`ed `Datum` with value from the given argument.
    pub fn new(val: ArcDatum<TT, ET>) -> Self {
        Self(Arc::new(val))
    }
}

/// Required by `DerefTryMut`.
impl<TT, ET> Deref for DatumArc<TT, ET> {
    type Target = ArcDatum<TT, ET>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// This allows `Arc` to be used as the `Datum` reference type.
impl<TT, ET> DerefTryMut for DatumArc<TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Arc::get_mut(&mut this.0)
    }
}


#[cfg(test)]
mod tests {
    #![allow(clippy::many_single_char_names, clippy::eq_op)]
    use super::*;
    use kul_shared_tests::utils::tree_shapes::*;
    use Datum::*;

    /// Used as a "text" type in tests where it does not need to be a real
    /// `Text`.
    #[derive(Copy, Clone, PartialEq, Debug)]
    struct DummyText;

    #[test]
    fn ref_equality_same() {
        assert_eq!(DatumBox::new(EmptyNest::<DummyText, u32, _>),
                   DatumBox::new(EmptyNest::<_, _, _>));

        assert_ne!(DatumBox::new(EmptyNest::<DummyText, (), _>),
                   DatumBox::new(EmptyList::<_, _, _>));

        assert_eq!(DatumRc::new(EmptyNest::<DummyText, bool, _>),
                   DatumRc::new(EmptyNest::<_, _, _>));

        assert_ne!(DatumRc::new(EmptyNest::<DummyText, (), _>),
                   DatumRc::new(EmptyList::<_, _, _>));

        assert_eq!(DatumArc::new(EmptyNest::<DummyText, char, _>),
                   DatumArc::new(EmptyNest::<_, _, _>));

        assert_ne!(DatumArc::new(EmptyNest::<DummyText, (), _>),
                   DatumArc::new(EmptyList::<_, _, _>));
    }

    #[test]
    fn equality_diff_ref() {
        assert_eq!(*DatumBox::new(EmptyNest::<DummyText, u32, _>),
                   *DatumRc::new(EmptyNest::<DummyText, u32, _>));

        assert_eq!(*DatumBox::new(EmptyNest::<DummyText, f64, _>),
                   *DatumArc::new(EmptyNest::<DummyText, f64, _>));

        assert_eq!(*DatumBox::new(EmptyList::<DummyText, bool, _>),
                   *DatumRc::new(EmptyList::<DummyText, bool, _>));

        assert_ne!(*DatumArc::new(EmptyList::<DummyText, i128, _>),
                   *DatumRc::new(EmptyNest::<DummyText, i128, _>));

        assert_eq!(*DatumBox::new(List::<DummyText, _, _>{
                                      elem: DatumBox::new(Extra('λ')),
                                      next: DatumBox::new(EmptyList)}),
                   *DatumRc::new(List::<DummyText, _, _>{
                                     elem: DatumRc::new(Extra('λ')),
                                     next: DatumRc::new(EmptyList)}));
    }

    #[test]
    fn clone() {
        let a = List::<DummyText, (), _>{
                    elem: DatumBox::new(EmptyNest::<_, _, _>),
                    next: DatumBox::new(EmptyList::<_, _, _>)};
        let b = a.clone();
        assert_eq!(a, b);

        let c = List::<DummyText, (), _>{
                    elem: DatumRc::new(EmptyNest::<_, _, _>),
                    next: DatumRc::new(EmptyList::<_, _, _>)};
        let d = c.clone();
        assert_eq!(c, d);

        let e = List::<DummyText, (), _>{
                    elem: DatumArc::new(EmptyNest::<_, _, _>),
                    next: DatumArc::new(EmptyList::<_, _, _>)};
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
        use kul_shared_tests::TestStrText;

        fn make_unusual_zigzag(depth: usize) -> DatumBox<TestStrText, usize> {
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
