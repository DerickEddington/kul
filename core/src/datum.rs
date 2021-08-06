//! Datum type used in the abstract syntax tree (AST) returned by parsing.

use core::ops::Deref;


/// Implementations provided for ready use.
pub mod premade {
    mod mutref;
    pub use mutref::{MutRefDatum, DatumMutRef};
}


/// The abstract syntax tree (AST) type returned by parsing.  It is extensible
/// by the `ExtraType` parameter, and it is parameterized over the `DatumRef`
/// type used to refer to the other `Datum`s in an AST.  It can also be used for
/// DAGs.
#[derive(Copy, Clone, Eq, Hash, Debug)]
pub enum Datum<TextType, ExtraType, DatumRef>
    where DatumRef: DerefTryMut<Target = Datum<TextType, ExtraType, DatumRef>>,
{
    /// A logically unbroken span of text. (Only nest forms break text
    /// logically, but escape characters break the representation into chunks.)
    Text(TextType),
    /// A nest form that is not empty and so has a non-empty "operator"/"head"
    /// sub-form and has a possibly-empty "operands" sub-form(s).
    Combination {
        /// The operator form, as any `Datum` variant.
        operator: DatumRef,
        /// The operands form, as a `List` or `EmptyList` or a `Text` variant.
        operands: DatumRef,
    },
    /// An empty nest form
    EmptyNest,
    /// A list of other `Datum`s. Used to represent parsed operands.
    List {
        /// An element of the list, as any `Datum` variant.
        elem: DatumRef,
        /// The rest of the list, as a `List` or `EmptyList` variant.
        next: DatumRef,
    },
    /// An empty list
    EmptyList,
    /// Extensibility that custom macros/combiners may utilize to add additional
    /// variants
    Extra(ExtraType),
}


/// This allows different concrete [`Datum`](enum.Datum.html) types to be
/// compared with each other for equality if their ["extra"
/// types](enum.Datum.html#variant.Extra) can be.  This also avoids stack
/// overflows for long lists and deep nests (but can still overflow on other
/// deep tree shapes, but those are rare).
#[allow(clippy::match_same_arms)]
impl<TT1, TT2, ET1, ET2, DR1, DR2>
    PartialEq<Datum<TT2, ET2, DR2>>
    for Datum<TT1, ET1, DR1>
    where DR1: DerefTryMut<Target = Datum<TT1, ET1, DR1>>,
          DR2: DerefTryMut<Target = Datum<TT2, ET2, DR2>>,
          TT1: PartialEq<TT2>,
          ET1: PartialEq<ET2>,
{
    fn eq(&self, other: &Datum<TT2, ET2, DR2>) -> bool {
        use Datum::*;

        let (mut left, mut right) = (self, other);
        loop {
            match (left, right) {
                (Text(txt1), Text(txt2))
                    => break *txt1 == *txt2,
                (Combination{operator: rtr1, operands: rnds1},
                 Combination{operator: rtr2, operands: rnds2})
                    => if **rnds1 == **rnds2 {
                        left = &**rtr1;
                        right = &**rtr2;
                    } else { break false },
                (EmptyNest, EmptyNest)
                    => break true,
                (List{elem: e1, next: n1}, List{elem: e2, next: n2})
                    => if **e1 == **e2 {
                        left = &**n1;
                        right = &**n2;
                    } else { break false },
                (EmptyList, EmptyList)
                    => break true,
                (Extra(et1), Extra(et2))
                    => break et1 == et2,
                _
                    => break false
            }
        }
    }
}


/// Exists to be used similarly to but differently than
/// [`DerefMut`](http://doc.rust-lang.org/std/ops/trait.DerefMut.html) so that
/// types like [`Rc`](http://doc.rust-lang.org/std/rc/struct.Rc.html) and its
/// [`get_mut`](http://doc.rust-lang.org/std/rc/struct.Rc.html#method.get_mut)
/// method can be used to hold [`Datum`s](enum.Datum.html).  `DerefMut` must
/// never fail, so it can't be used.  We want mutability of `Datum`s so that we
/// can construct [lists of them](enum.Datum.html#variant.List) using only the
/// space of the values allocated by a `Parser`'s
/// [`DatumAllocator`](trait.DatumAllocator.html), since this crate is intended
/// to be usable in `no_std` environments which don't provide heap allocation.
///
/// The alternative of using tree-recursion to acheive temporary stack
/// allocation for constructing the lists without mutation is not good because
/// the stack is too small for large lists and it could realistically be
/// exceeded (causing a crash).  While `no_std` environments might also have
/// restricted limits to the amounts of `Datum`s that could be allocated, with
/// our approach they can control the limit and can receive an
/// [`Error`](enum.Error.html#variant.AllocExhausted) value under their control
/// if the limit is exceeded (instead of a crash).
pub trait DerefTryMut: Deref
    where <Self as Deref>::Target: Sized,
{
    /// Returns a mutable reference to the inner `Datum` if it can.  Otherwise,
    /// returns `None`.  Some implementations may never return `None` (e.g. for
    /// types that also implement `DerefMut`).
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target>;
}


#[cfg(test)]
mod tests {
    #![allow(clippy::eq_op)]
    use super::{*, premade::*};

    /// Used as a "text" type in tests where it does not need to be a real
    /// `Text`.
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    struct DummyText;

    #[test]
    fn equality_same() {
        use Datum::*;

        assert_eq!(Text::<_, (), DatumMutRef<'_, _, ()>>(DummyText),
                   Text::<_, (), DatumMutRef<'_, _, ()>>(DummyText));

        assert_eq!(Combination::<DummyText, (), DatumMutRef<'_, _, ()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)},
                   Combination::<DummyText, (), DatumMutRef<'_, _, ()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyNest::<DummyText, (), DatumMutRef<'_, _, ()>>,
                   EmptyNest::<DummyText, (), DatumMutRef<'_, _, ()>>);

        assert_eq!(List::<DummyText, (), DatumMutRef<'_, _, ()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)},
                   List::<DummyText, (), DatumMutRef<'_, _, ()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyList::<DummyText, (), DatumMutRef<'_, _, ()>>,
                   EmptyList::<DummyText, (), DatumMutRef<'_, _, ()>>);

        assert_eq!(Extra::<DummyText, (), DatumMutRef<'_, _, ()>>(()),
                   Extra::<DummyText, (), DatumMutRef<'_, _, ()>>(()));

        // TODO: More cases, including !=
    }

    mod datumref {
        use super::*;

        #[derive(Copy, Clone, Debug)]
        pub(super) struct DatumRef<'d, TT, ET>(
            pub(super) &'d Datum<TT, ET, DatumRef<'d, TT, ET>>);

        impl<'d, TT, ET> Deref for DatumRef<'d, TT, ET>
        {
            type Target = Datum<TT, ET, DatumRef<'d, TT, ET>>;

            fn deref(&self) -> &Self::Target {
                self.0
            }
        }

        impl<TT, ET> DerefTryMut for DatumRef<'_, TT, ET>
        {
            fn get_mut(_this: &mut Self) -> Option<&mut Self::Target> {
                None
            }
        }
    }

    #[test]
    fn equality_diff_ref() {
        use Datum::*;
        use datumref::DatumRef;

        assert_eq!(Text::<_, (), DatumMutRef<'_, _, ()>>(DummyText),
                   Text::<_, (), DatumRef<'_, _, ()>>(DummyText));

        assert_eq!(Combination::<DummyText, (), DatumMutRef<'_, _, ()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)},
                   Combination::<DummyText, (), DatumRef<'_, _, ()>>{
                       operator: DatumRef(&EmptyNest),
                       operands: DatumRef(&EmptyList)});

        assert_eq!(EmptyNest::<DummyText, (), DatumMutRef<'_, _, ()>>,
                   EmptyNest::<DummyText, (), DatumRef<'_, _, ()>>);

        assert_eq!(List::<DummyText, (), DatumMutRef<'_, _, ()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)},
                   List::<DummyText, (), DatumRef<'_, _, ()>>{
                       elem: DatumRef(&EmptyNest),
                       next: DatumRef(&EmptyList)});

        assert_eq!(EmptyList::<DummyText, (), DatumMutRef<'_, _, ()>>,
                   EmptyList::<DummyText, (), DatumRef<'_, _, ()>>);

        assert!(Extra::<DummyText, (), DatumMutRef<'_, _, ()>>(())
                == Extra::<DummyText, (), DatumRef<'_, _, ()>>(()));
    }

    #[test]
    fn copy_clone() {
        use Datum::*;
        use datumref::DatumRef;

        let a = List::<DummyText, (), DatumRef<'_, _, ()>>{
            elem: DatumRef(&EmptyNest::<_, (), DatumRef<'_, _, ()>>),
            next: DatumRef(&EmptyList::<_, (), DatumRef<'_, _, ()>>)};
        let b = a;
        assert_eq!(a, b);

        let c = List::<DummyText, (), DatumRef<'_, _, ()>>{
            elem: DatumRef(&EmptyNest::<_, (), DatumRef<'_, _, ()>>),
            next: DatumRef(&EmptyList::<_, (), DatumRef<'_, _, ()>>)};
        #[allow(clippy::clone_on_copy)]
        let d = c.clone();
        assert_eq!(c, d);
    }
}
