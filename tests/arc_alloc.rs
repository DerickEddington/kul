use kruvi::*;
use kruvi_shared_tests::suites::*;


fn parser() -> Parser<DefaultCharClassifier,
                      ArcDatumAllocator,
                      EmptyOperatorBindings<ArcDatumAllocator>>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: ArcDatumAllocator,
        bindings: EmptyOperatorBindings::new(),
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
