use kul::{
    Parser,
    parser::{ArcDatumAllocator, DefaultCharClassifier},
    text::{TextVec, chunk::PosStr},
};

use kul_shared_tests::{
    suites::test_suite0,
    bindings::BasicTestOperatorBindings,
};


type DA = ArcDatumAllocator<TextVec<PosStr<'static>>, ()>;

fn parser() -> Parser<DefaultCharClassifier,
                      DA,
                      BasicTestOperatorBindings<DA>>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: ArcDatumAllocator::default(),
        bindings: BasicTestOperatorBindings::default(),
    }
}

#[test]
fn suite0() {
    test_suite0(parser());
}
