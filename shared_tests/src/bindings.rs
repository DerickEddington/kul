//! A trait for generically setting bindings in `OperatorBindings`
//! implementations, for testing them; and implementations of the trait for the
//! premade types of our crates so that testing them is supported.
//!
//! It's infeasible for this to cover all possible instantiations of all their
//! type parameters, so only a limited subset it covered, but it's enough to
//! cover testing their logic.

#![allow(clippy::module_name_repetitions)]

use std::hash::{Hash, BuildHasher};

use kul_core::{
    Datum,
    parser::{OperatorBindings, DatumAllocator, premade::PairOperatorBindings},
    combiner::{Combiner, OpFn, ApFn},
};

use kul::parser::HashMapOperatorBindings;


type DADatum<DA> = Datum<<DA as DatumAllocator>::TT,
                         <DA as DatumAllocator>::ET,
                         <DA as DatumAllocator>::DR>;

/// The form of specification of bindings given to
/// `TestOperatorBindings::set_bindings`.
pub type BindingsSpec<DA, CE> = Vec<(DADatum<DA>, Combiner<Box<OpFn<DA, CE>>,
                                                           Box<ApFn<DA, CE>>>)>;


/// A trait for generically setting bindings in `OperatorBindings`
/// implementations, for testing them.
pub trait TestOperatorBindings<DA>: OperatorBindings<DA>
    where DA: DatumAllocator,
{
    /// Given a vector of 2-tuples that pair `Datum`s that represent operator
    /// sub-forms with `Combiner`s, establish bindings for the pairs in our
    /// `self` according to our `Self`'s particular way.
    fn set_bindings(&mut self, bindings: BindingsSpec<DA, Self::CE>);
}


/// An implementation of `TestOperatorBindings` suitable for use with the basic
/// test suites.
pub type BasicTestOperatorBindings<DA>
    = PairOperatorBindings<BindingsSpec<DA, ()>,
                           DA,
                           Box<OpFn<DA, ()>>,
                           Box<ApFn<DA, ()>>,
                           ()>;


impl<DA, CE, S>
    TestOperatorBindings<DA>
    for HashMapOperatorBindings<DA, Box<OpFn<DA, CE>>, Box<ApFn<DA, CE>>, CE, S>
    where DA: DatumAllocator,
          DA::TT: Hash + Eq,
          DA::ET: Hash + Eq,
          DA::DR: Hash + Eq,
          S: BuildHasher,
{
    fn set_bindings(&mut self, bindings: BindingsSpec<DA, Self::CE>) {
        self.hashmap.clear();
        self.hashmap.extend(bindings);
    }
}


impl<DA, CE>
    TestOperatorBindings<DA>
    for PairOperatorBindings<BindingsSpec<DA, CE>,
                             DA,
                             Box<OpFn<DA, CE>>,
                             Box<ApFn<DA, CE>>,
                             CE>
    where DA: DatumAllocator,
          DADatum<DA>: PartialEq,
{
    fn set_bindings(&mut self, bindings: BindingsSpec<DA, Self::CE>) {
        self.pairs = bindings;
    }
}
