//! Used by the integration tests of both the [core](../kruvi_core/index.html)
//! and the [full](../kruvi/index.html) crates.  It provides test suites that
//! can be run against any type of [`Parser`](../kruvi_core/trait.Parser.html),
//! and it uses a couple tricks so that its representation of expected results
//! can be directly compared against a variety of
//! [`Datum`](../kruvi_core/enum.Datum.html) types.

// TODO: Probably get rid of `TestCase` and instead have some `parse` function
// to replace `TestCase::test` and which can be used in assertion statements

// TODO: Probably should enhance to support predicate checking of "extra types"
// so that functionality can be tested globally too.

use std::ops::Deref;
use std::fmt::Debug;

use kruvi_core::*;


#[derive(PartialEq, Eq, Debug)]
struct TestCase<'d> {
    input: &'static str,
    expected: Expected<'d>,
}

impl TestCase<'_> {
    fn test<P>(&self, parser: &mut P)
        where P: Parser<'static>,
              P::DR: Debug,
              P::CE: Debug,
    {
        const RESUME_RETRIES: u32 = 1;

        let mut result = vec![];

        { // New block so that `iter` is dropped at the end, just because this
          // seems cleaner because `ParserIter` does implement `Drop`.  Not
          // necessary for the correctness of this method.

            let mut iter = parser.parse(self.input);

            // Accumulate everything from the parser iterator, in our `Vec` of
            // `Vec` structure, handling the possible rare case where an
            // iterator can be resumed after its `next` method returns `None`
            loop {
                let mut next = None;
                for _ in 0 .. 1 + RESUME_RETRIES {
                    next = iter.next();
                    if next.is_some() { break; }
                }
                match next {
                    Some(next) => {
                        let mut somes = vec![next];
                        while let Some(next) = iter.next() { somes.push(next); }
                        result.push(somes);
                    },
                    None => break
                }
            }
        }

        assert_eq!(self.expected, result)
    }
}

// We use `Vec` of `Vec` so we can model the rare case where an iterator can be
// resumed after its `next` method returns `None`
type Expected<'d> = Vec<Vec<Item<'d>>>;

#[derive(PartialEq, Eq, Debug)]
struct Item<'d> (ParseIterItem<ExpectedDatum<'d>, CeIgnore>);

impl<'d, DR, CE> PartialEq<ParseIterItem<DR, CE>> for Item<'d>
    where DR: Deref,
          ExpectedDatum<'d>: PartialEq<DR::Target>,
          CeIgnore: PartialEq<CE>,
{
    fn eq(&self, other: &ParseIterItem<DR, CE>) -> bool {
        match (&self.0, other) {
            (Ok(d1), Ok(dr2)) => *d1 == **dr2,
            (Err(e1), Err(e2)) => *e1 == *e2,
            _ => false
        }
    }
}

type ExpectedDatum<'d> = MutRefDatum<'d, 'static, EtIgnore>;
#[cfg(test)]
type ExpectedDatumRef<'d> = DatumMutRef<'d, 'static, EtIgnore>;

#[derive(Copy, Clone, Eq, Debug)]
struct EtIgnore;

impl<ET> PartialEq<ET> for EtIgnore {
    fn eq(&self, _other: &ET) -> bool {
        true
    }
}

#[derive(Copy, Clone, Eq, Debug)]
struct CeIgnore;

impl<CE> PartialEq<CE> for CeIgnore {
    fn eq(&self, _other: &CE) -> bool {
        true
    }
}


/// Basic test suite that checks the basic syntax and forms and does not
/// exercise macros/combiners nor extra types.
pub fn test_suite0<P>(p: &mut P)
    where P: Parser<'static>,
          P::DR: Debug,
          P::CE: Debug,
{
    TestCase {
        input: "",
        expected: vec![]
    }
    .test(p);

    // TODO: A lot more
}


// This only tests the internal units of this crate
#[cfg(test)]
mod tests {
    use super::*;
    use kruvi_core::Datum::*;
    use kruvi_core::Error::*;

    #[test]
    fn extratype_ignore_eq() {
        assert_eq!(EtIgnore, ());
        assert_eq!(EtIgnore, 1);
        assert_eq!(EtIgnore, 2.3);
        assert_eq!(EtIgnore, "foo");
    }

    #[test]
    fn combinererror_ignore_eq() {
        assert_eq!(CeIgnore, ());
        assert_eq!(CeIgnore, 1);
        assert_eq!(CeIgnore, 2.3);
        assert_eq!(CeIgnore, "foo");
    }

    #[test]
    fn datum_equality() {
        assert_eq!(Text::<EtIgnore, ExpectedDatumRef>(
                       PosStr{val:"a", src:"a{b}", byte_pos:0, char_pos:0}),
                   Text::<(), DatumMutRef<()>>(
                       PosStr{val:"a", src:"{c{a}b}", byte_pos:3, char_pos:3}));

        assert_eq!(Combination::<EtIgnore, ExpectedDatumRef>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList) },
                   Combination::<i32, DatumMutRef<i32>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyNest::<EtIgnore, ExpectedDatumRef>,
                   EmptyNest::<f64, DatumMutRef<f64>>);

        assert_eq!(List::<EtIgnore, ExpectedDatumRef>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)},
                   List::<&str, DatumMutRef<&str>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyList::<EtIgnore, ExpectedDatumRef>,
                   EmptyList::<bool, DatumMutRef<bool>>);

        assert_eq!(Extra::<EtIgnore, ExpectedDatumRef>(EtIgnore),
                   Extra::<&str, DatumMutRef<&str>>("foo"));
    }

    #[test]
    fn error_equality() {
        assert_eq!(UnbalancedEndChar::<CeIgnore>{byte_pos:2, char_pos:1},
                   UnbalancedEndChar::<()>{byte_pos:2, char_pos:1});

        assert_eq!(MissingEndChar::<CeIgnore>, MissingEndChar::<()>);

        assert_eq!(AllocExhausted::<CeIgnore>, AllocExhausted::<()>);

        assert_eq!(NoAllocState::<CeIgnore>, NoAllocState::<()>);

        assert_eq!(DerefTryMut::<CeIgnore>, DerefTryMut::<()>);

        assert_eq!(FailedCombiner::<CeIgnore>(CeIgnore), FailedCombiner::<i32>(1));
    }

    type WimpyDatum<'d> = Datum<'static, (), DatumMutRef<'d, 'static, ()>>;

    struct WimpyParser<'d> {
        single_datum: Option<&'d mut WimpyDatum<'d>>,
    }

    impl<'d> WimpyParser<'d> {
        fn new(datum: &'d mut WimpyDatum<'d>) -> Self {
            WimpyParser { single_datum: Some(datum) }
        }
    }

    impl<'d> Parser<'static> for WimpyParser<'d> {
        type AS = Option<&'d mut WimpyDatum<'d>>;
        type ET = ();
        type DR = DatumMutRef<'d, 'static, Self::ET>;
        type OR = &'d mut OpFn<'static, Self::ET, Self::DR, Self::CE, Self::AS>;
        type AR = &'d mut ApFn<'static, Self::ET, Self::DR, Self::CE, Self::AS>;
        type CE = ();

        fn supply_alloc_state(&mut self) -> Self::AS {
            core::mem::replace(&mut self.single_datum, None)
        }

        fn receive_alloc_state(&mut self, _alst: Self::AS) { }

        fn env_lookup(&mut self, _operator: &Self::DR)
                      -> Option<Combiner<Self::OR, Self::AR>>
        { None }

        fn new_datum(&mut self, from: Datum<'static, Self::ET, Self::DR>, alst: Self::AS)
                     -> Result<(Self::DR, Self::AS), Error<Self::CE>>
        {
            match alst {
                Some(datum_ref) => {
                    *datum_ref = from;
                    Ok((DatumMutRef(datum_ref), None))
                },
                None => Err(AllocExhausted)
            }
        }
    }

    #[test]
    fn basic_empty() {
        TestCase {
            input: "",
            expected: vec![]
        }
        .test(&mut WimpyParser::new(&mut EmptyNest));
    }

    #[test]
    fn basic_single() {
        TestCase {
            input: "solo",
            expected: vec![vec![Item(Ok(Text(PosStr{val: "solo", src: "solo",
                                                    byte_pos: 0, char_pos: 0})))]]
        }
        .test(&mut WimpyParser::new(&mut EmptyNest));
    }

    #[test]
    fn basic_exhaust() {
        TestCase {
            input: "good {shit}",
            expected: vec![vec![Item(Ok(Text(PosStr{val: "good ", src: "good {shit}",
                                                    byte_pos: 0, char_pos: 0}))),
                                Item(Err(AllocExhausted)),
                                Item(Err(NoAllocState))]]
        }
        .test(&mut WimpyParser::new(&mut EmptyNest));
    }

    // // WimpyParser won't work for most cases of test_suite0
    // #[test]
    // fn suite0() {
    //     test_suite0(&mut WimpyParser::new(&mut EmptyNest));
    // }
}
