#![allow(clippy::type_complexity)]

use std::{
    collections::HashMap, hash::Hash,
    iter::{repeat_with, FromIterator},
    rc::Rc, sync::Arc,
};

use kul::{
    Parser, Datum,
    parser::{HashMapOperatorBindings, DefaultCharClassifier,
             DatumAllocator, SliceDatumAllocator,
             BoxDatumAllocator, RcDatumAllocator, ArcDatumAllocator},
    combiner::{Combiner, OpFn, ApFn},
    text::{TextDatumList, chunk::PosStr, TextVec, chunk::PosStrish},
    datum::MutRefDatum,
};

use kul_shared_tests::suites::test_suite0;


fn parser<DA, CE>(
    allocator: DA,
    bindings: HashMap<Datum<DA::TT, DA::ET, DA::DR>,
                      Combiner<Box<OpFn<DA, CE>>,
                               Box<ApFn<DA, CE>>>>,
)
    -> Parser<DefaultCharClassifier,
              DA,
              HashMapOperatorBindings<DA, Box<OpFn<DA, CE>>, Box<ApFn<DA, CE>>, CE>>
where DA: DatumAllocator,
      DA::TT: Hash + Eq,
      DA::ET: Hash + Eq,
      DA::DR: Hash + Eq,
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator,
        bindings: HashMapOperatorBindings::new(bindings),
    }
}


#[test]
fn suite0_core_types() {
    type CombErr = bool;
    type Extra = i32;
    type Array<'a, 's> = [MutRefDatum<'a, TextDatumList<'a, PosStr<'s>, Extra>, Extra>];

    let mut datum_array: Box<Array<'_, '_>> =
        Vec::from_iter(repeat_with(|| Datum::Extra(0)).take(0x200))
        .into_boxed_slice();

    test_suite0(parser::<_, CombErr>(SliceDatumAllocator::new(&mut datum_array[..]),
                                     HashMap::new()));
}

#[test]
fn suite0_box_types() {
    test_suite0(parser::<_, ()>(BoxDatumAllocator::<TextVec<PosStrish<Rc<Box<str>>>>, ()>
                                    ::default(),
                                HashMap::new()));
}

#[test]
fn suite0_rc_types() {
    test_suite0(parser::<_, u8>(RcDatumAllocator::<TextVec<PosStrish<Rc<str>>>, u64>
                                    ::default(),
                                HashMap::new()));
}

#[test]
fn suite0_arc_types() {
    test_suite0(parser::<_, i64>(ArcDatumAllocator::<TextVec<PosStrish<Arc<String>>>, i8>
                                     ::default(),
                                 HashMap::new()));
}
