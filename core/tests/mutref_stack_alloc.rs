#![no_std]

use kruvi_core::*;
use kruvi_core::Datum::*;
use kruvi_shared_tests::suites::*;


type TxtTy<'a> = TextDatumSeq<'a, PosStr<'static>, ()>;

fn parser<'a>(arr: &'a mut [MutRefDatum<'a, TxtTy<'a>, ()>])
              -> Parser<DefaultCharClassifier,
                        ArrayDatumAllocator,
                        EmptyOperatorBindings>
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: ArrayDatumAllocator{free: Some(arr)},
        bindings: EmptyOperatorBindings,
    }
}

struct ArrayDatumAllocator<'a> {
    free: Option<&'a mut [MutRefDatum<'a, TxtTy<'a>, ()>]>,
}

impl<'a> DatumAllocator for ArrayDatumAllocator<'a> {
    type TT = TxtTy<'a>;
    type ET = ();
    type DR = DatumMutRef<'a, Self::TT, Self::ET>;

    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        let free = self.free.take().unwrap();
        match free.split_first_mut() {
            Some((dr, rest)) => {
                *dr = from;
                self.free = Some(rest);
                Ok(DatumMutRef(dr))
            }
            None => Err(AllocError::AllocExhausted)
        }
    }
}

#[test]
fn suite0() {
    // TODO?: Make this smaller so it's easier to test allocator exhaustion?
    let mut datum_array: [MutRefDatum<'_, TxtTy<'_>, ()>; 256] = [Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(())];

    test_suite0(parser(&mut datum_array));
}
