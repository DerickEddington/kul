use kruvi::{
    Parser,
    parser::{BoxDatumAllocator, DefaultCharClassifier, EmptyOperatorBindings},
    text::{TextVec, chunk::PosStr},
};

use kruvi_shared_tests::suites::test_suite0;


fn parser() -> Parser<DefaultCharClassifier,
                      BoxDatumAllocator<TextVec<PosStr<'static>>, ()>,
                      EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: BoxDatumAllocator::default(),
        bindings: EmptyOperatorBindings,
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
