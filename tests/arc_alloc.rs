use kruvi::{
    Parser, Datum,
    parser::{DatumAllocator, AllocError, DefaultCharClassifier, EmptyOperatorBindings},
    datum::DatumArc,
    text::{TextVec, chunk::PosStr},
};

use kruvi_shared_tests::suites::test_suite0;


fn parser() -> Parser<DefaultCharClassifier,
                      ArcDatumAllocator,
                      EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: ArcDatumAllocator,
        bindings: EmptyOperatorBindings,
    }
}

#[derive(Debug)]
struct ArcDatumAllocator;

impl DatumAllocator for ArcDatumAllocator {
    type TT = TextVec<PosStr<'static>>;
    type ET = ();
    type DR = DatumArc<Self::TT, Self::ET>;

    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumArc::new(from))
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
