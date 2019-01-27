use kruvi::*;
use kruvi_shared_tests::suites::*;


fn parser() -> Parser<DefaultCharClassifier,
                      BoxDatumAllocator,
                      EmptyOperatorBindings<BoxDatumAllocator>>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: BoxDatumAllocator,
        bindings: EmptyOperatorBindings::new(),
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
