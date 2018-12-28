//! Used by the integration tests of both the [core](../kruvi_core/index.html)
//! and the [full](../kruvi/index.html) crates.  It provides test suites that
//! can be run against any type of [`Parser`](../kruvi_core/trait.Parser.html),
//! and it uses tricks so that its representation of expected results can be
//! directly compared against a variety of
//! [`Datum`](../kruvi_core/enum.Datum.html) types.


// TODO: Probably should enhance to support predicate checking of "extra types"
// so that functionality can be tested globally too.

use std::ops::{Deref, DerefMut};
use std::fmt::Debug;

use kruvi_core::*;


pub mod suites;
pub mod utils;


fn parse_all<P>(parser: &mut P, input: &'static str)
                -> Vec<Vec<ParseIterItem<P::DR, P::CE>>>
    where P: Parser<'static>,
{
    const RESUME_RETRIES: u32 = 1;
    let mut result = vec![];
    let mut iter = parser.parse(input);
    // Accumulate everything from the parser iterator, in our `Vec` of `Vec`
    // structure, handling the possible rare case where an iterator can be
    // resumed after its `next` method returns `None`
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
    result
}

fn expect(e: Vec<Result<Datum<'static, EtIgnore, ExpectedDatumRef>,
                        Error<CeIgnore>>>)
          -> Expected
{
    if e.len() == 0 { return vec![]; }
    vec![e.into_iter().map(|r| Item(r.map(dr))).collect()]
}

fn dr(from: Datum<'static, EtIgnore, ExpectedDatumRef>) -> ExpectedDatumRef {
    ExpectedDatumRef(Box::new(ExpectedDatum(from)))
}

/// We use `Vec` of `Vec` so we can model the rare case where an iterator can be
/// resumed after its `next` method returns `None`
type Expected = Vec<Vec<Item>>;

/// Newtype wrapper needed to implement our `PartialEq` trick
#[derive(PartialEq, Eq, Debug)]
struct Item (ParseIterItem<ExpectedDatumRef, CeIgnore>);

/// This allows `Item` to be compared with any type of `ParseIterItem`, and it
/// compares in our special ways
impl<DR, CE> PartialEq<ParseIterItem<DR, CE>> for Item
    where DR: Deref,
          ExpectedDatum: PartialEq<DR::Target>,
          CeIgnore: PartialEq<CE>,
{
    fn eq(&self, other: &ParseIterItem<DR, CE>) -> bool {
        match (&self.0, other) {
            (Ok(dr1), Ok(dr2)) => *dr1.0 == **dr2,
            (Err(e1), Err(e2)) => *e1 == *e2,
            _ => false
        }
    }
}

/// Newtype wrapper needed to implement our `PartialEq` trick
#[derive(Clone, PartialEq, Eq, Debug)]
struct ExpectedDatum (Datum<'static, EtIgnore, ExpectedDatumRef>);
#[derive(Clone, PartialEq, Eq, Debug)]
struct ExpectedDatumRef (Box<ExpectedDatum>);

impl Deref for ExpectedDatumRef {
    type Target = Datum<'static, EtIgnore, ExpectedDatumRef>;

    fn deref(&self) -> &Self::Target {
        &Deref::deref(&self.0).0
    }
}

impl DerefMut for ExpectedDatumRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut DerefMut::deref_mut(&mut self.0).0
    }
}

/// This allows `ExpectedDatumRef` to be used as the `Datum` reference type.
impl DerefTryMut for ExpectedDatumRef {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(DerefMut::deref_mut(this))
    }
}

/// This allows `ExpectedDatum` to be compared with `Datum` types differently
/// than the normal comparison of `Datum` types, so that the `Text` variants'
/// `PosStr` type can be compared completely (as opposed to only by the primary
/// `val` string).
impl<'s, ET, DR> PartialEq<Datum<'s, ET, DR>> for ExpectedDatum
    where DR: DerefTryMut<Target = Datum<'s, ET, DR>>
{
    fn eq(&self, other: &Datum<'s, ET, DR>) -> bool {
        use kruvi_core::Datum::*;

        match (&self.0, other) {
            (Text(ps1), Text(ps2))
                // This is what's significantly different than normal compare
                => ps1 == ps2,
            (Combination{operator: rtr1, operands: rnds1},
             Combination{operator: rtr2, operands: rnds2})
                => *rtr1.0 == **rtr2 && *rnds1.0 == **rnds2,
            (EmptyNest, EmptyNest)
                => true,
            (List{elem: e1, next: n1}, List{elem: e2, next: n2})
                => *e1.0 == **e2 && *n1.0 == **n2,
            (EmptyList, EmptyList)
                => true,
            (Extra(et1), Extra(et2))
                => et1 == et2,
            _
                => false
        }
    }
}

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


/// `Parser` that wraps another `Parser` to make it use custom delimiters
pub struct CustomDelimParser<'p, P>
    where P: Parser<'static>
{
    pub parser: &'p mut P,
    pub nest_start: Option<Vec<char>>,
    pub nest_end: Option<Vec<char>>,
    pub nest_escape: Option<Vec<char>>,
    pub whitespace: Option<Vec<char>>,
}

impl<'p, P> Parser<'static> for CustomDelimParser<'p, P>
    where P: Parser<'static>
{
    type AS = P::AS;
    type ET = P::ET;
    type DR = P::DR;
    type OR = P::OR;
    type AR = P::AR;
    type CE = P::CE;

    fn is_nest_start(&self, c: char) -> bool {
        self.nest_start.as_ref().map_or_else(|| self.parser.is_nest_start(c),
                                             |v| v.contains(&c))
    }
    fn is_nest_end(&self, c: char) -> bool {
        self.nest_end.as_ref().map_or_else(|| self.parser.is_nest_end(c),
                                           |v| v.contains(&c))
    }
    fn is_nest_escape(&self, c: char) -> bool {
        self.nest_escape.as_ref().map_or_else(|| self.parser.is_nest_escape(c),
                                              |v| v.contains(&c))
    }
    fn is_whitespace(&self, c: char) -> bool {
        self.whitespace.as_ref().map_or_else(|| self.parser.is_whitespace(c),
                                             |v| v.contains(&c))
    }
    fn supply_alloc_state(&mut self) -> Self::AS {
        self.parser.supply_alloc_state()
    }
    fn receive_alloc_state(&mut self, alst: Self::AS) {
        self.parser.receive_alloc_state(alst)
    }
    fn env_lookup(&mut self, operator: &Self::DR)
                  -> Option<Combiner<Self::OR, Self::AR>> {
        self.parser.env_lookup(operator)
    }
    fn new_datum(&mut self, from: Datum<'static, Self::ET, Self::DR>,
                 alst: Self::AS)
                 -> Result<(Self::DR, Self::AS), Error<Self::CE>> {
        self.parser.new_datum(from, alst)
    }
}


// This only tests the internal units of this module
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
                       operator: dr(EmptyNest),
                       operands: dr(EmptyList)},
                   Combination::<i32, DatumMutRef<i32>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyNest::<EtIgnore, ExpectedDatumRef>,
                   EmptyNest::<f64, DatumMutRef<f64>>);

        assert_eq!(List::<EtIgnore, ExpectedDatumRef>{
                       elem: dr(EmptyNest),
                       next: dr(EmptyList)},
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
        assert_eq!(expect(vec![]),
                   parse_all(&mut WimpyParser::new(&mut EmptyNest),
                             ""));
    }

    #[test]
    fn basic_single() {
        assert_eq!(expect(vec![Ok(Text(PosStr{val: "solo", src: "solo",
                                              byte_pos: 0, char_pos: 0}))]),
                   parse_all(&mut WimpyParser::new(&mut EmptyNest),
                             "solo"));
    }

    #[test]
    fn basic_exhaust() {
        assert_eq!(expect(vec![Ok(Text(PosStr{val: "good ", src: "good {shit}",
                                              byte_pos: 0, char_pos: 0})),
                               Err(AllocExhausted),
                               Err(NoAllocState)]),
                   parse_all(&mut WimpyParser::new(&mut EmptyNest),
                             "good {shit}"));
    }

    // // WimpyParser won't work for most cases of test_suite0
    // #[test]
    // fn suite0() {
    //     super::suites::test_suite0(&mut WimpyParser::new(&mut EmptyNest));
    // }
}
