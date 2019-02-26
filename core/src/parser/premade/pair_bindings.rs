use core::{ops::Deref, borrow::Borrow, marker::PhantomData};

use crate::{
    Datum, Combiner,
    parser::{OperatorBindings, DatumAllocator},
    combiner::{OpFn, ApFn},
};


/// An [`OperatorBindings`] that associates generic `Datum`s with generic
/// `Combiner`s, using a slice of pairs (2-tuples) of them.
///
/// The `Datum` values represent parsed operator sub-forms (of non-empty nest
/// forms) that are bound to macro functions represented by the `Combiner`
/// values.
///
/// You are responsible for initializing the slice with the desired bindings,
/// and it may be dynamically mutated inbetween parser invocations, if desired
/// and if you use a mutable type for it.
///
/// [`OperatorBindings`]: ../trait.OperatorBindings.html
#[derive(PartialEq, Eq, Debug)]
pub struct PairOperatorBindings<P, DA, OR, AR, CE>
    where P: Borrow<[(Datum<DA::TT, DA::ET, DA::DR>, Combiner<OR, AR>)]>,
          DA: DatumAllocator,
{
    /// The bindings of operators to macros.  You must initialize and manage
    /// this yourself.
    pub pairs: P,
    /// We want `Self` to be parameterized over these types because this better
    /// fits with the `OperatorBindings` trait and the uses of it.
    have_type_params: PhantomData<(*const DA, *const OR, *const AR, *const CE)>,
}


impl<P, DA, OR, AR, CE>
    PairOperatorBindings<P, DA, OR, AR, CE>
    where P: Borrow<[(Datum<DA::TT, DA::ET, DA::DR>, Combiner<OR, AR>)]>,
          DA: DatumAllocator,
{
    /// Given a value of a type that can borrow as a slice of pairs (2-tuples)
    /// of `Datum`s with `Combiner`s, make a new instance of `Self` that uses it
    /// for its bindings.
    #[inline]
    pub fn new(pairs: P) -> Self {
        Self {
            pairs,
            have_type_params: PhantomData,
        }
    }
}


impl<P, DA, OR, AR, CE>
    Default
    for PairOperatorBindings<P, DA, OR, AR, CE>
    where P: Default + Borrow<[(Datum<DA::TT, DA::ET, DA::DR>, Combiner<OR, AR>)]>,
          DA: DatumAllocator,
{
    /// Make a new instance of `Self` using the `Default::default()` of our
    /// pairs' type.
    #[inline]
    fn default() -> Self {
        Self::new(P::default())
    }
}


impl<P, DA, OR, AR, CE>
    OperatorBindings<DA>
    for PairOperatorBindings<P, DA, OR, AR, CE>
    where P: Borrow<[(Datum<DA::TT, DA::ET, DA::DR>, Combiner<OR, AR>)]>,
          DA: DatumAllocator,
          Datum<DA::TT, DA::ET, DA::DR>: PartialEq,
          OR: Deref<Target = OpFn<DA, CE>>,
          AR: Deref<Target = ApFn<DA, CE>>,
{
    type OR = OR;
    type AR = AR;
    type CE = CE;

    #[inline]
    fn lookup(&self, operator: &Datum<DA::TT, DA::ET, DA::DR>)
              -> Option<&Combiner<OR, AR>>
    {
        self.pairs.borrow().iter().find_map(
            |(datum, combiner)|
            if *operator == *datum {
                Some(combiner)
            } else {
                None
            })
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::premade::SliceDatumAllocator,
        text::{premade::TextDatumList, chunk::premade::PosStr, Text},
        datum::premade::DatumMutRef,
    };

    type TT<'d> = TextDatumList<'d, PosStr<'static>, ()>;
    type DA<'d> = SliceDatumAllocator<'d, TT<'d>, ()>;
    type Op<'f, 'd> = &'f OpFn<DA<'d>, ()>;
    type Ap<'f, 'd> = &'f ApFn<DA<'d>, ()>;
    type POB<'d, 'f, P> = PairOperatorBindings<P, DA<'d>, Op<'f, 'd>, Ap<'f, 'd>, ()>;

    #[test]
    fn empty_slice() {
        let bindings_array = [];
        let pob = POB::new(&bindings_array[..]);
        assert_eq!(pob.lookup(&Datum::EmptyNest).map(|_| true), None);
    }

    #[test]
    fn empty_array() {
        let pob = POB::new([]);
        assert_eq!(pob.lookup(&Datum::EmptyNest).map(|_| true), None);
    }

    #[test]
    fn full_slice_mut() {
        let op: Op<'_, '_> = &(|_, _, _| unreachable!());
        let ap: Ap<'_, '_> = &(|_, _, _| unreachable!());
        let (mut d1, mut d2) = (Datum::Extra(()), Datum::EmptyList);
        let mut bindings_array = [
            (Datum::Text(TT::from_str("foo")),
             Combiner::Operative(op)),
            (Datum::Combination{operator: DatumMutRef(&mut d1),
                                operands: DatumMutRef(&mut d2)},
             Combiner::Applicative(ap)),
        ];

        let mut pob = POB::new(&mut bindings_array[..]);
        assert_eq!(pob.lookup(&Datum::EmptyNest).map(|_| true), None);
        assert_eq!(pob.lookup(&Datum::Text(TT::from_str("foo")))
                      .map(|c| if let Combiner::Operative(_) = c { true }
                               else { false }),
                   Some(true));
        let (mut d3, mut d4) = (Datum::Extra(()), Datum::EmptyList);
        assert_eq!(pob.lookup(&Datum::Combination {
                                   operator: DatumMutRef(&mut d3),
                                   operands: DatumMutRef(&mut d4),
                               })
                      .map(|c| if let Combiner::Applicative(_) = c { true }
                               else { false }),
                   Some(true));

        // Remove a binding
        let pairs = pob.pairs;
        pob.pairs = &mut pairs[1..];
        assert_eq!(pob.lookup(&Datum::Text(TT::from_str("foo"))).map(|_| true),
                   None);

        // Change a binding
        pob.pairs[0].0 = Datum::Text(TT::from_str("bar"));
        assert_eq!(pob.lookup(&Datum::Text(TT::from_str("bar")))
                      .map(|c| if let Combiner::Applicative(_) = c { true }
                               else { false }),
                   Some(true));
        assert_eq!(pob.lookup(&Datum::Combination {
                                   operator: DatumMutRef(&mut Datum::Extra(())),
                                   operands: DatumMutRef(&mut Datum::EmptyList),
                               })
                      .map(|_| true),
                   None);
    }

    #[test]
    fn full_array() {
        let ap: Ap<'_, '_> = &(|_, _, _| unreachable!());
        let pob = POB::new([(Datum::Text(TT::from_str("zab")),
                             Combiner::Applicative(ap))]);
        assert_eq!(pob.lookup(&Datum::EmptyNest).map(|_| true), None);
        assert_eq!(pob.lookup(&Datum::Text(TT::from_str("zab")))
                      .map(|c| if let Combiner::Applicative(_) = c { true }
                               else { false }),
                   Some(true));
    }
}
