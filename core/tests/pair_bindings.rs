#![allow(clippy::type_complexity)]

use std::{borrow::Borrow, iter::repeat_with};

use kul_core::{
    Parser, Datum,
    parser::{DatumAllocator, premade::{PairOperatorBindings, SliceDatumAllocator,
                                       DefaultCharClassifier}},
    combiner::{Combiner, OpFn, ApFn},
    text::{premade::TextDatumList, chunk::premade::PosStr},
    datum::premade::MutRefDatum,
};

use kul_shared_tests::{suites::test_suite0, bindings::BindingsSpec};


fn parser<DA, B, CE>(
    allocator: DA,
    bindings: B,
)
    -> Parser<DefaultCharClassifier,
              DA,
              PairOperatorBindings<B, DA, Box<OpFn<DA, CE>>, Box<ApFn<DA, CE>>, CE>>
where DA: DatumAllocator,
      B: Borrow<[(Datum<DA::TT, DA::ET, DA::DR>,
                  Combiner<Box<OpFn<DA, CE>>, Box<ApFn<DA, CE>>>)]>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator,
        bindings: PairOperatorBindings::new(bindings),
    }
}


#[test]
fn suite0_core_types() {
    type CombErr = u16;
    type Extra = bool;
    type Array<'a, 's> = [MutRefDatum<'a, TextDatumList<'a, PosStr<'s>, Extra>, Extra>];

    let mut datum_array: Box<Array<'_, '_>> =
        repeat_with(|| Datum::Extra(true)).take(0x200).collect::<Vec<_>>()
        .into_boxed_slice();

    test_suite0(parser::<_, _, CombErr>(SliceDatumAllocator::new(&mut datum_array[..]),
                                        BindingsSpec::default()));
}
