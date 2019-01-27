use kruvi::*;
use kruvi_shared_tests::suites::*;


fn parser() -> Parser<DefaultCharClassifier,
                      RcDatumAllocator,
                      EmptyOperatorBindings<RcDatumAllocator>>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: RcDatumAllocator,
        bindings: EmptyOperatorBindings::new(),
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
