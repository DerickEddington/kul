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
//! * Provides [`Text`] types that use heap allocation for their chunking.
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
//! [`Text`]: trait.Text.html
//! [`Drop`]: http://doc.rust-lang.org/std/ops/trait.Drop.html
//! [`Parser`]: struct.Parser.html

use std::boxed::Box;
use std::rc::Rc;
use std::sync::Arc;
use std::ops::{Deref, DerefMut};

// Re-export everything from the core crate
pub use kruvi_core::*;


pub mod drop;


/// This assists in `Box` being used as the `Datum` reference type.
pub type BoxDatum<TT, ET> = Datum<TT, ET, DatumBox<TT, ET>>;

/// This wrapper allows the needed recursive type definition for `Box` to be
/// used as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DatumBox<TT, ET>(pub Box<BoxDatum<TT, ET>>);

impl<TT, ET> DatumBox<TT, ET> {
    pub fn new(val: BoxDatum<TT, ET>) -> Self {
        DatumBox(Box::new(val))
    }
}

impl<TT, ET> Deref for DatumBox<TT, ET> {
    type Target = BoxDatum<TT, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

impl<TT, ET> DerefMut for DatumBox<TT, ET> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        DerefMut::deref_mut(&mut self.0)
    }
}

/// This allows `Box` to be used as the `Datum` reference type.
impl<TT, ET> DerefTryMut for DatumBox<TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(DerefMut::deref_mut(this))
    }
}


/// This assists in `Rc` being used as the `Datum` reference type.
pub type RcDatum<TT, ET> = Datum<TT, ET, DatumRc<TT, ET>>;

/// This wrapper allows the needed recursive type definition for `Rc` to be used
/// as the `Datum` reference type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DatumRc<TT, ET>(pub Rc<RcDatum<TT, ET>>);

impl<TT, ET> DatumRc<TT, ET> {
    pub fn new(val: RcDatum<TT, ET>) -> Self {
        DatumRc(Rc::new(val))
    }
}

impl<TT, ET> Deref for DatumRc<TT, ET> {
    type Target = RcDatum<TT, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
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
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DatumArc<TT, ET>(pub Arc<ArcDatum<TT, ET>>);

impl<TT, ET> DatumArc<TT, ET> {
    pub fn new(val: ArcDatum<TT, ET>) -> Self {
        DatumArc(Arc::new(val))
    }
}

impl<TT, ET> Deref for DatumArc<TT, ET> {
    type Target = ArcDatum<TT, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

/// This allows `Arc` to be used as the `Datum` reference type.
impl<TT, ET> DerefTryMut for DatumArc<TT, ET> {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Arc::get_mut(&mut this.0)
    }
}


/// A text representation that uses a `Vec` to keep its chunks, that can work
/// with any [`TextChunk`] type, and that is a [`TextConcat`] that can be used
/// with [`Parser`s].
#[derive(Clone, Debug)]
pub struct TextVec<C> {
    chunks: Vec<C>,
}

impl<C> From<C> for TextVec<C>
    where C: TextChunk,
{
    #[inline]
    fn from(chunk: C) -> Self {
        Self {
            chunks: vec![chunk],
        }
    }
}

impl<TT, C> PartialEq<TT> for TextVec<C>
    where TT: Text,
          C: TextChunk,
{
    #[inline]
    fn eq(&self, other: &TT) -> bool {
        Text::partial_eq(self, other)
    }
}

impl<C> Eq for TextVec<C>
    where C: TextChunk,
{}

// TODO: PartialOrd, Ord

impl<C> TextBase for TextVec<C>
    where C: TextChunk,
{
    type Pos = C::Pos;

    #[inline]
    fn empty() -> Self {
        Self {
            chunks: vec![],
        }
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.chunks.iter().all(TextBase::is_empty)
    }
}

impl<C> Text for TextVec<C>
    where C: TextChunk,
{
    type Chunk = C;
    type IterChunksState = [Self::Chunk];

    #[inline]
    fn iter_chunks_state(&self) -> Option<&Self::IterChunksState> {
        Some(&self.chunks[..])
    }

}

impl<C, DA> TextConcat<DA> for TextVec<C>
    where C: TextChunk,
          DA: DatumAllocator<TT = Self>,
{
    fn concat(mut self, mut other: Self, _: &mut DA) -> Result<Self, AllocError> {
        self.chunks.append(&mut other.chunks);
        Ok(self)
    }
}

// TODO: Impl SourceStream for:
// - IntoIterator<Item=char>, by embedding in some struct that uses heap-alloc
//   for the accum'ing

// TODO: Integration tests that impl Text (and so a SourceStream) for:
// - Vec<char>, using kruvi_core's Text impl for &[char]
// - LinkedList<char>, just to test it can be done



// TODO: Parser impl that makes it easier by using Box and that provides API for
// establishing combiner bindings.


#[cfg(test)]
mod tests {
    use super::*;
    use kruvi_shared_tests::utils::*;

    /// Used as a "text" type in tests where it does not need to be a real
    /// `Text`.
    #[derive(Copy, Clone, PartialEq, Debug)]
    struct DummyText;

    #[test]
    fn datum_ref_equality_same() {
        use super::Datum::*;

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
    fn datum_equality_diff_ref() {
        use super::Datum::*;

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
    fn datum_clone() {
        use super::Datum::*;

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
        use kruvi_shared_tests::TestStrText;

        fn make_unusual_zigzag(depth: usize) -> DatumBox<TestStrText, usize> {
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
