use kruvi::{
    Parser,
    parser::{RcDatumAllocator, DefaultCharClassifier},
    text::{TextVec, chunk::PosStr},
};

use kruvi_shared_tests::{
    suites::test_suite0,
    bindings::BasicTestOperatorBindings,
};


type DA = RcDatumAllocator<TextVec<PosStr<'static>>, ()>;

fn parser() -> Parser<DefaultCharClassifier,
                      DA,
                      BasicTestOperatorBindings<DA>>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: RcDatumAllocator::default(),
        bindings: BasicTestOperatorBindings::default(),
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
