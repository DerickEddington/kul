#![allow(clippy::type_complexity)]

use std::{
    collections::{HashMap, hash_map::RandomState},
    hash::{Hash, BuildHasher},
    ops::Deref,
    marker::PhantomData,
};

use crate::{
    Datum, Combiner,
    parser::{OperatorBindings, DatumAllocator},
    combiner::{OpFn, ApFn},
};


/// An [`OperatorBindings`] that associates generic `Datum`s with generic
/// `Combiner`s, using a `HashMap`.
///
/// The `Datum` keys represent parsed operator sub-forms (of non-empty nest
/// forms) that are bound to macro functions represented by the `Combiner`
/// values.
///
/// You are responsible for populating the `hashmap` field with the desired
/// bindings, and it may be dynamically mutated inbetween parser invocations, if
/// desired.
///
/// [`OperatorBindings`]: ../../kruvi_core/parser/trait.OperatorBindings.html
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct HashMapOperatorBindings<DA, OR, AR, CE, S = RandomState>
    where DA: DatumAllocator,
          DA::TT: Hash + Eq,
          DA::ET: Hash + Eq,
          DA::DR: Hash + Eq,
          S: BuildHasher,
{
    /// The `HashMap` of operators bound to macros.  You must populate and
    /// manage this yourself.
    pub hashmap: HashMap<Datum<DA::TT, DA::ET, DA::DR>, Combiner<OR, AR>, S>,
    /// We want `Self` to be parameterized over the "combiner error" type
    /// because this better fits with the `OperatorBindings` trait and the uses
    /// if it.
    have_combiner_error_type: PhantomData<*const CE>,
}

impl<DA, OR, AR, CE, S>
    HashMapOperatorBindings<DA, OR, AR, CE, S>
    where DA: DatumAllocator,
          DA::TT: Hash + Eq,
          DA::ET: Hash + Eq,
          DA::DR: Hash + Eq,
          S: BuildHasher,
{
    /// Given a `HashMap` of our type, make a new instance of `Self` that uses
    /// it for its `hashmap` field.
    ///
    /// This enables using a premade `HashMap`, possibly with a certain capacity
    /// and/or with a different hashing algorithm (as supported by `HashMap`).
    #[inline]
    pub fn new(hashmap: HashMap<Datum<DA::TT, DA::ET, DA::DR>, Combiner<OR, AR>, S>)
               -> Self
    {
        Self {
            hashmap,
            have_combiner_error_type: PhantomData,
        }
    }
}


impl<DA, OR, AR, CE, S>
    Default
    for HashMapOperatorBindings<DA, OR, AR, CE, S>
    where DA: DatumAllocator,
          DA::TT: Hash + Eq,
          DA::ET: Hash + Eq,
          DA::DR: Hash + Eq,
          S: BuildHasher + Default,
{
    /// Make a new instance of `Self` using the `HashMap::default()` of our
    /// type.
    #[inline]
    fn default() -> Self {
        Self::new(HashMap::default())
    }
}


impl<DA, OR, AR, CE, S>
    OperatorBindings<DA>
    for HashMapOperatorBindings<DA, OR, AR, CE, S>
    where DA: DatumAllocator,
          DA::TT: Hash + Eq,
          DA::ET: Hash + Eq,
          DA::DR: Hash + Eq,
          S: BuildHasher,
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
        self.hashmap.get(operator)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        text::{Text, TextVec, chunk::PosStr},
        parser::BoxDatumAllocator,
        datum::DatumBox,
    };

    #[test]
    #[allow(unused_results)]
    fn basic() {
        let mut hmob: HashMapOperatorBindings<BoxDatumAllocator<TextVec<PosStr<'_>>, ()>,
                                              Box<OpFn<_, _>>, Box<ApFn<_, _>>, ()>
            = HashMapOperatorBindings::default();

        let rator = Datum::Text(TextVec::<PosStr<'_>>::from_str("rator"));

        assert_eq!(hmob.lookup(&rator).map(|_| true), None);

        hmob.hashmap.insert(rator.clone(),
                            Combiner::Operative(Box::new(|_, _, _| unreachable!())));

        assert_eq!(hmob.lookup(&rator)
                       .map(|c| if let Combiner::Operative(_) = c { true }
                                else { false }),
                   Some(true));

        assert_eq!(hmob.lookup(&Datum::EmptyNest).map(|_| true), None);

        hmob.hashmap.remove(&rator);

        assert_eq!(hmob.lookup(&rator).map(|_| true), None);

        let compound = Datum::Combination{operator: DatumBox::new(Datum::Extra(())),
                                          operands: DatumBox::new(Datum::EmptyList)};

        assert_eq!(hmob.lookup(&compound).map(|_| true), None);

        hmob.hashmap.insert(compound.clone(),
                            Combiner::Applicative(Box::new(|_, _, _| unreachable!())));

        assert_eq!(hmob.lookup(&compound)
                       .map(|c| if let Combiner::Applicative(_) = c { true }
                                else { false }),
                   Some(true));
    }
}
