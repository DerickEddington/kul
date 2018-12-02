#![no_std]

use core::mem::replace;

use kruvi_core::*;
use kruvi_core::Datum::*;
use kruvi_shared_tests::*;


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
    type AS = &'a mut [MutRefDatum<'a, 'static, Self::ET>];
    type ET = ();
    type DR = DatumMutRef<'a, 'static, Self::ET>;
    // Note: OR and DR are not actually used for this test case
    type OR = &'a mut OpFn<'static, Self::ET, Self::DR, Self::AS>;
    type AR = &'a mut ApFn<'static, Self::ET, Self::DR, Self::AS>;

    fn supply_alloc_state(&mut self) -> Self::AS {
        // For this type, we must temporarily disown our &mut reference
        // to the array, so that &mut references to its elements can be
        // split off without being borrows from our self. (Such borrows
        // from self would prevent any further calling of methods of
        // self.)
        replace(&mut self.datum_array_free, None).unwrap()
    }

    fn receive_alloc_state(&mut self, alst: Self::AS) {
        self.datum_array_free = Some(alst);
    }

    fn env_lookup(&mut self, _operator: &Self::DR)
                  -> Option<Combiner<Self::OR, Self::AR>>
    { None } // Not actually used for this test case

    fn new_datum(&mut self, from: Datum<'static, Self::ET, Self::DR>,
                 alst: Self::AS)
                 -> Result<(Self::DR, Self::AS), Error>
    {
        match alst.split_first_mut() {
            Some((dr, rest)) => {
                *dr = from;
                Ok((DatumMutRef(dr), rest))
            }
            None => Err(Error::AllocExhausted)
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
