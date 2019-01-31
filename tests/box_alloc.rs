use kruvi::{
    Parser, Datum,
    parser::{DatumAllocator, AllocError, DefaultCharClassifier, EmptyOperatorBindings},
    datum::DatumBox,
    text::{TextVec, chunk::PosStr},
};

use kruvi_shared_tests::suites::test_suite0;


fn parser() -> Parser<DefaultCharClassifier,
                      BoxDatumAllocator,
                      EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: BoxDatumAllocator,
        bindings: EmptyOperatorBindings,
    }
}

#[derive(Debug)]
struct BoxDatumAllocator;

impl DatumAllocator for BoxDatumAllocator {
    type TT = TextVec<PosStr<'static>>;
    type ET = ();
    type DR = DatumBox<Self::TT, Self::ET>;

    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumBox::new(from))
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
