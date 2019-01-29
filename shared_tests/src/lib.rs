//! Used by the integration tests of both the [core](../kruvi_core/index.html)
//! and the [full](../kruvi/index.html) crates.  It provides test suites that
//! can be run against many types of [`Parser`](../kruvi_core/struct.Parser.html),
//! and it uses tricks so that its representation of expected results can be
//! directly compared against a variety of
//! [`Datum`](../kruvi_core/enum.Datum.html) types.


// TODO: Support checking of the generic character position
// information. Probably with some new trait for converting a TextPosition to
// one concrete type this crate can deal with.
// Maybe simply `(char_pos, Option<utf8_byte_pos>, Option<utf16_byte_pos>)`
// where `None` values cause those byte positions to not be checked.

// TODO: Probably should enhance to support predicate checking of "extra types"
// so that functionality can be tested globally too.

use std::ops::{Deref, DerefMut};
use std::fmt::Debug;

use kruvi_core::*;
use kruvi::TextVec;


pub mod suites;
pub mod utils;


pub type TestStrText = TextVec<PosStr<'static>>;


// TODO: Should some/all of the below items be public?


fn parse_all<CC, DA, OB, S>(
    parser: &mut Parser<CC, DA, OB>,
    input: S,
)
    -> Vec<ParseIterItem<DA::DR,
                         <DA::TT as TextBase>::Pos,
                         OB::CE>>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
          S: SourceStream<DA::TT, DA>,
          // DA::DR: Debug,
          // DA::TT: Debug,
          // <DA::TT as TextBase>::Pos: Debug,
          // <DA::TT as Text>::Chunk: Debug,
          // OB::CE: Debug,
{
    let mut result = vec![];
    let mut iter = parser.parse(input);
    while let Some(next) = iter.next() {
        result.push(next);
    }
    result
}

fn expect(e: Vec<Result<ExpectedDatum, Error<PosIgnore, CeIgnore>>>) -> Expected {
    e.into_iter().map(|r| Item(r.map(dr))).collect()
}

fn dr(from: ExpectedDatum) -> ExpectedDatumRef {
    ExpectedDatumRef(Box::new(from))
}

type Expected = Vec<Item>;

/// Newtype wrapper needed to implement our `PartialEq` trick
#[derive(PartialEq, Eq, Debug)]
struct Item (ParseIterItem<ExpectedDatumRef, PosIgnore, CeIgnore>);

/// This allows `Item` to be compared with any type of `ParseIterItem`, and it
/// compares in our special ways
impl<DR, POS, CE> PartialEq<ParseIterItem<DR, POS, CE>> for Item
    where DR: Deref,
          ExpectedDatum: PartialEq<DR::Target>,
          PosIgnore: PartialEq<POS>,
          CeIgnore: PartialEq<CE>,
{
    fn eq(&self, other: &ParseIterItem<DR, POS, CE>) -> bool {
        match (&self.0, other) {
            (Ok(dr1), Ok(dr2)) => *dr1.0 == **dr2,
            (Err(e1), Err(e2)) => *e1 == *e2,
            _ => false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct ExpectedText(&'static str);

impl<TT> PartialEq<TT> for ExpectedText
    where TT: Text,
{
    fn eq(&self, other: &TT) -> bool {
        self.0.chars().eq(other.iter().map(|SourceIterItem{ch, ..}| ch))
    }
}

type ExpectedDatum = Datum<ExpectedText, EtIgnore, ExpectedDatumRef>;
#[derive(Clone, PartialEq, Eq, Debug)]
struct ExpectedDatumRef (Box<ExpectedDatum>);

impl Deref for ExpectedDatumRef {
    type Target = ExpectedDatum;
    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

impl DerefMut for ExpectedDatumRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        DerefMut::deref_mut(&mut self.0)
    }
}

/// This allows `ExpectedDatumRef` to be used as the `Datum` reference type.
impl DerefTryMut for ExpectedDatumRef {
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(DerefMut::deref_mut(this))
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
struct PosIgnore;

impl<P> PartialEq<P> for PosIgnore {
    fn eq(&self, _other: &P) -> bool {
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


mod custom_delim {
    use super::*;

    /// `Parser` that converts another `Parser` to make it use custom delimiters
    pub fn parser<CC, DA, OB>(parser: Parser<CC, DA, OB>, spec: Spec)
                              -> Parser<CustomDelimCC, DA, OB> {
        Parser {
            classifier: CustomDelimCC(spec),
            allocator: parser.allocator,
            bindings: parser.bindings,
        }
    }

    pub struct Spec {
        pub nest_start: Vec<char>,
        pub nest_end: Vec<char>,
        pub nest_escape: Vec<char>,
        pub whitespace: Vec<char>,
    }

    pub struct CustomDelimCC(Spec);

    impl CharClassifier for CustomDelimCC {
        fn is_nest_start(&self, c: char) -> bool {
            self.0.nest_start.contains(&c)
        }
        fn is_nest_end(&self, c: char) -> bool {
            self.0.nest_end.contains(&c)
        }
        fn is_nest_escape(&self, c: char) -> bool {
            self.0.nest_escape.contains(&c)
        }
        fn is_whitespace(&self, c: char) -> bool {
            self.0.whitespace.contains(&c)
        }
    }
}

// This only tests the internal units of this module
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extratype_ignore_eq() {
        assert_eq!(EtIgnore, ());
        assert_eq!(EtIgnore, 1);
        assert_eq!(EtIgnore, 2.3);
        assert_eq!(EtIgnore, "foo");
    }

    #[test]
    fn position_ignore_eq() {
        assert_eq!(PosIgnore, ());
        assert_eq!(PosIgnore, 1);
        assert_eq!(PosIgnore, 2.3);
        assert_eq!(PosIgnore, "foo");
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
        use Datum::{Combination, EmptyNest, List, EmptyList, Extra};

        assert_eq!(Datum::Text::<_, EtIgnore, ExpectedDatumRef>(ExpectedText("a")),
                   Datum::Text::<TestStrText, (), DatumMutRef<TestStrText, ()>>(
                       Text::from_str("a")));

        assert_eq!(Combination::<_, EtIgnore, ExpectedDatumRef>{
                       operator: dr(EmptyNest),
                       operands: dr(EmptyList)},
                   Combination::<TestStrText, i32, DatumMutRef<TestStrText, i32>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyNest::<_, EtIgnore, ExpectedDatumRef>,
                   EmptyNest::<TestStrText, f64, DatumMutRef<TestStrText, f64>>);

        assert_eq!(List::<_, EtIgnore, ExpectedDatumRef>{
                       elem: dr(EmptyNest),
                       next: dr(EmptyList)},
                   List::<TestStrText, &str, DatumMutRef<TestStrText, &str>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyList::<_, EtIgnore, ExpectedDatumRef>,
                   EmptyList::<TestStrText, bool, DatumMutRef<TestStrText, bool>>);

        assert_eq!(Extra::<_, EtIgnore, ExpectedDatumRef>(EtIgnore),
                   Extra::<TestStrText, &str, DatumMutRef<TestStrText, &str>>("foo"));
    }

    #[test]
    fn error_equality() {
        use kruvi_core::Error::*;

        assert_eq!(UnbalancedEndChar::<PosIgnore, CeIgnore>(PosIgnore),
                   UnbalancedEndChar::<usize, ()>(7));

        assert_eq!(MissingEndChar::<PosIgnore, CeIgnore>, MissingEndChar::<i32, ()>);

        assert_eq!(FailedAlloc::<PosIgnore, CeIgnore>(AllocError::AllocExhausted),
                   FailedAlloc::<f64, ()>(AllocError::AllocExhausted));

        assert_eq!(FailedDerefTryMut::<PosIgnore, CeIgnore>,
                   FailedDerefTryMut::<bool, ()>);

        assert_eq!(FailedCombiner::<PosIgnore, CeIgnore>(CeIgnore),
                   FailedCombiner::<PosIgnore, i32>(1));
    }

    mod basic_parse_all {
        use super::*;
        use kruvi::{DatumBox};

        fn wimpy_parser() -> Parser<DefaultCharClassifier,
                                    WimpyDatumAllocator,
                                    EmptyOperatorBindings>
        {
            Parser {
                classifier: DefaultCharClassifier,
                allocator: WimpyDatumAllocator{single_datum:
                                               Some(DatumBox::new(Datum::EmptyNest))},
                bindings: EmptyOperatorBindings,
            }
        }

        struct WimpyDatumAllocator {
            single_datum: Option<DatumBox<TestStrText, ()>>,
        }

        impl DatumAllocator for WimpyDatumAllocator {
            type TT = TestStrText;
            type ET = ();
            type DR = DatumBox<Self::TT, Self::ET>;

            fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                         -> Result<Self::DR, AllocError>
            {
                let free = self.single_datum.take();
                match free {
                    Some(mut datum_ref) => {
                        *datum_ref = from;
                        Ok(datum_ref)
                    },
                    None => Err(AllocError::AllocExhausted)
                }
            }
        }

        #[test]
        fn empty() {
            assert_eq!(expect(vec![]),
                       parse_all(&mut wimpy_parser(),
                                 TestStrText::from_str("").iter()));
        }

        #[test]
        fn single() {
            assert_eq!(expect(vec![Ok(Datum::Text(ExpectedText("solo")))]),
                       parse_all(&mut wimpy_parser(),
                                 TestStrText::from_str("solo").iter()));
        }

        #[test]
        fn exhaust() {
            assert_eq!(expect(vec![Ok(Datum::Text(ExpectedText("good "))),
                                   Err(Error::FailedAlloc(AllocError::AllocExhausted)),
                                   Err(Error::UnbalancedEndChar(PosIgnore))]),
                       parse_all(&mut wimpy_parser(),
                                 TestStrText::from_str("good {shit}").iter()));
        }

        // // WimpyParser won't work for most cases of test_suite0. But this can be
        // // useful for testing that compilation of this works.
        // #[test]
        // fn suite0() {
        //     crate::suites::test_suite0(wimpy_parser());
        // }
    }
}
