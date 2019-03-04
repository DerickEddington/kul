#![no_std]

use kul_core::{
    Parser, Datum,
    parser::{
        premade::{SliceDatumAllocator, DefaultCharClassifier},
    },
    datum::premade::MutRefDatum,
    text::{premade::TextDatumList, chunk::premade::PosStr},
};

use kul_shared_tests::{
    suites::test_suite0,
    bindings::BasicTestOperatorBindings,
};


type TxtTy<'a> = TextDatumList<'a, PosStr<'static>, ()>;

type DA<'a> = SliceDatumAllocator<'a, TxtTy<'a>, ()>;

fn parser<'a>(arr: &'a mut [MutRefDatum<'a, TxtTy<'a>, ()>])
              -> Parser<DefaultCharClassifier,
                        DA<'a>,
                        BasicTestOperatorBindings<DA<'a>>>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: SliceDatumAllocator::new(arr),
        bindings: BasicTestOperatorBindings::default(),
    }
}

#[test]
fn suite0() {
    use Datum::Extra;

    let mut datum_array: [MutRefDatum<'_, TxtTy<'_>, ()>; 512]
        = [Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(())];

    test_suite0(parser(&mut datum_array));
}
