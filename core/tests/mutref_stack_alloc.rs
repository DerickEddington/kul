#![no_std]

use kruvi_core::{
    Parser, Datum,
    parser::{
        premade::{SliceDatumAllocator, DefaultCharClassifier, EmptyOperatorBindings},
    },
    datum::premade::MutRefDatum,
    text::{premade::TextDatumList, chunk::premade::PosStr},
};

use kruvi_shared_tests::suites::test_suite0;


type TxtTy<'a> = TextDatumList<'a, PosStr<'static>, ()>;

fn parser<'a>(arr: &'a mut [MutRefDatum<'a, TxtTy<'a>, ()>])
              -> Parser<DefaultCharClassifier,
                        SliceDatumAllocator<'a, TxtTy<'a>, ()>,
                        EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: SliceDatumAllocator::new(arr),
        bindings: EmptyOperatorBindings,
    }
}

#[test]
fn suite0() {
    use Datum::Extra;

    let mut datum_array: [MutRefDatum<'_, TxtTy<'_>, ()>; 512]
        = [Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(())];

    test_suite0(parser(&mut datum_array));
}
