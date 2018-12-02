use std::boxed::Box;

use kruvi::*;
use kruvi_shared_tests::*;


#[derive(PartialEq, Eq, Debug)]
struct ParserBox();

impl Parser<'static> for ParserBox {
    type AS = ();
    type ET = ();
    type DR = DatumBox<'static, Self::ET>;
    // Note: OR and DR are not actually used for this test case
    type OR = Box<OpFn<'static, Self::ET, Self::DR, Self::AS>>;
    type AR = Box<ApFn<'static, Self::ET, Self::DR, Self::AS>>;

    fn supply_alloc_state(&mut self) -> Self::AS { () }

    fn receive_alloc_state(&mut self, _: Self::AS) {}

    fn env_lookup(&mut self, _operator: &Self::DR)
                  -> Option<Combiner<Self::OR, Self::AR>>
    { None }

    fn new_datum(&mut self, from: Datum<'static, Self::ET, Self::DR>, _: Self::AS)
                 -> Result<(Self::DR, Self::AS), Error>
    {
        Ok((DatumBox::new(from), ()))
    }
}

#[test]
fn suites() {
    let mut p = ParserBox();
    test_suite0(&mut p);
}
