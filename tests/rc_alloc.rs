use kruvi::{
    Parser, Datum,
    parser::{DatumAllocator, AllocError, DefaultCharClassifier, EmptyOperatorBindings},
    datum::DatumRc,
    text::{TextVec, chunk::PosStr},
};

use kruvi_shared_tests::suites::test_suite0;


fn parser() -> Parser<DefaultCharClassifier,
                      RcDatumAllocator,
                      EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: RcDatumAllocator,
        bindings: EmptyOperatorBindings,
    }
}

#[derive(Debug)]
struct RcDatumAllocator;

impl DatumAllocator for RcDatumAllocator {
    type TT = TextVec<PosStr<'static>>;
    type ET = ();
    type DR = DatumRc<Self::TT, Self::ET>;

    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumRc::new(from))
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
