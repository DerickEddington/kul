use kruvi::{
    Parser,
    parser::{RcDatumAllocator, DefaultCharClassifier, EmptyOperatorBindings},
    text::{TextVec, chunk::PosStr},
};

use kruvi_shared_tests::suites::test_suite0;


fn parser() -> Parser<DefaultCharClassifier,
                      RcDatumAllocator<TextVec<PosStr<'static>>, ()>,
                      EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: RcDatumAllocator::default(),
        bindings: EmptyOperatorBindings,
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
