use std::boxed::Box;

use kruvi::*;
use kruvi_shared_tests::*;


#[derive(PartialEq, Eq, Debug)]
struct ParserArc();

impl Parser<'static> for ParserArc {
    type AS = ();
    type ET = ();
    type DR = DatumArc<'static, Self::ET>;
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
        Ok((DatumArc::new(from), ()))
    }
}

#[test]
fn suites() {
    let mut p = ParserArc();
    test_suite0(&mut p);
}
