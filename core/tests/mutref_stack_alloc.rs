#![no_std]

use kruvi_core::*;
use kruvi_core::Datum::*;
use kruvi_shared_tests::suites::*;


#[derive(PartialEq, Eq, Debug)]
struct ParserMutRef<'a> {
    datum_array_free: Option<&'a mut [MutRefDatum<'a, 'static, ()>]>,
}

impl<'a> ParserMutRef<'a> {
    fn new(arr: &'a mut [MutRefDatum<'a, 'static, ()>]) -> Self {
        ParserMutRef {datum_array_free: Some(arr)}
    }
}

impl<'a> Parser<'static> for ParserMutRef<'a> {
    type ET = ();
    type DR = DatumMutRef<'a, 'static, Self::ET>;
    // Note: OR and DR are not actually used for this test case
    type OR = &'a mut OpFn<'static, Self::ET, Self::DR, Self::CE>;
    type AR = &'a mut ApFn<'static, Self::ET, Self::DR, Self::CE>;
    type CE = ();

    fn env_lookup(&mut self, _operator: &Self::DR)
                  -> Option<Combiner<Self::OR, Self::AR>>
    { None } // Not actually used for this test case

    fn new_datum(&mut self, from: Datum<'static, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        let free = self.datum_array_free.take().unwrap();
        match free.split_first_mut() {
            Some((dr, rest)) => {
                *dr = from;
                self.datum_array_free = Some(rest);
                Ok(DatumMutRef(dr))
            }
            None => Err(AllocError::AllocExhausted)
        }
    }
}

#[test]
fn suites() {
    // TODO?: Make this smaller so it's easier to test allocator exhaustion?
    let mut datum_array: [MutRefDatum<'_, 'static, ()>; 256] = [Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(()), Extra(())];

    let mut p = ParserMutRef::new(&mut datum_array);
    test_suite0(&mut p);
}
