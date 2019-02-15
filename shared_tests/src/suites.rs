//! Suites of tests applied across multiple crates

use std::fmt::Debug;

use kruvi_core::{Parser, SourceStream, Text, TextBase, Datum, Combiner,
                 Error};
use kruvi_core::parser::{DatumAllocator, OperatorBindings, AllocError,
                         premade::{DefaultCharClassifier, EmptyOperatorBindings}};
use kruvi_core::combiner::{OpFn, ApFn};

use crate::{parse_all, expect, dr, ExpectedText, PosIgnore, custom_delim};


/// Basic interface to test suite #0 that only requires giving a `Parser`.  This
/// will exercise the given `Parser`'s `Text` type as a `SourceStream` as well
/// as in produced `Datum`s.
pub fn test_suite0<DA>(p: Parser<DefaultCharClassifier,
                                 DA,
                                 EmptyOperatorBindings>)
    where DA: DatumAllocator,
          <DA::TT as Text>::Chunk: From<&'static str>,
          DA::TT: Debug,
          DA::ET: Debug,
          DA::DR: Debug,
          <DA::TT as TextBase>::Pos: Debug,
{
    use kruvi_core::SourceIterItem;

    struct DummySourceStream<DA>(DA);

    impl<DA> Iterator for DummySourceStream<DA>
        where DA: DatumAllocator,
    {
        type Item = SourceIterItem<<DA::TT as TextBase>::Pos>;
        fn next(&mut self) -> Option<Self::Item> { unreachable!() }
    }

    impl<DA> SourceStream<DA::TT, DA> for DummySourceStream<DA>
        where DA: DatumAllocator,
    {
        fn peek(&mut self) -> Option<&<Self as Iterator>::Item> { unreachable!() }

        fn next_accum(&mut self, _: &mut DA)
                      -> Result<Option<<Self as Iterator>::Item>, AllocError>
        { unreachable!() }

        fn accum_done(&mut self, _: &mut DA) -> Result<DA::TT, AllocError>
        { unreachable!() }
    }

    type DummyFn<DA> = fn(&'static str) -> DummySourceStream<DA>;

    test_suite0_with(p, None::<DummyFn<DA>>);
}


/// Test suite that checks the basic syntax and forms, which exercises the
/// generic parsing logic, and does not exercise macros/combiners nor extra
/// types.
///
/// The given `Parser` is required to use the default character classifier and
/// the empty operator bindings so we know what we're starting with.  We will
/// later mutate the parser to test non-default character classifiers and to
/// test the parsing logic for non-empty operator bindings.
///
/// If the `str_to_src_strm` argument is `None`, the test-case inputs will be
/// converted to the given `Parser`'s `Text` type's iterator to be used as the
/// `SourceStream` to parse.  This is both more convenient and it exercises the
/// text type's ability to be used as a `SourceStream`.  This is used by
/// `test_suite0` which is used by many integration tests.
///
/// If the `str_to_src_strm` argument is `Some`, the test inputs will be
/// converted by the function given in the argument, and the only requirement is
/// that a compatible `SourceStream` be returned.  This enables supplying
/// various types of `SourceStream`s to parse.  This is used by other
/// integration tests that exercise different types of `SourceStream`s.
///
/// In either case, the given `Parser`'s `Text` type is always exercised to the
/// degree that parsing constructs values of it in the produced `Datum`s which
/// are compared with the expected test-case outputs.
pub fn test_suite0_with<'a, DA, F, S>(mut p: Parser<DefaultCharClassifier,
                                                    DA,
                                                    EmptyOperatorBindings>,
                                      str_to_src_strm: Option<F>)
    where DA: DatumAllocator,
          <DA::TT as Text>::Chunk: From<&'static str>,
          DA::TT: Debug,
          DA::ET: Debug,
          DA::DR: Debug,
          <DA::TT as TextBase>::Pos: Debug,
          F: Fn(&'static str) -> S,
          S: SourceStream<DA::TT, DA>,
{
    use Datum::{Combination, EmptyNest, List, EmptyList};
    use Error::*;

    let text = |val| Datum::Text(ExpectedText(val));

    let comb = |rator, rands|
                   Combination{operator: dr(rator), operands: dr(rands)};

    let list = |elem, next| List{elem: dr(elem), next: dr(next)};
    let list1 = |e1| list(e1, EmptyList);
    let list2 = |e1, e2| list(e1, list1(e2));
    let list3 = |e1, e2, e3| list(e1, list2(e2, e3));
    let list4 = |e1, e2, e3, e4| list(e1, list3(e2, e3, e4));
    let list5 = |e1, e2, e3, e4, e5| list(e1, list4(e2, e3, e4, e5));

    macro_rules! test {
        ($input:expr => [$($expected:expr),*])
            =>
        {test!($input => (assert_eq p) [$($expected),*])};

        ($input:expr =>! [$($expected:expr),*])
            =>
        {test!($input => (assert_ne p) [$($expected),*])};

        ($input:expr => ($parser:expr) [$($expected:expr),*])
            =>
        {test!($input => (assert_eq $parser) [$($expected),*])};

        ($input:expr =>! ($parser:expr) [$($expected:expr),*])
            =>
        {test!($input => (assert_ne $parser) [$($expected),*])};

        ($input:expr => ($ass:ident $parser:expr) [$($expected:expr),*])
            =>
        {//dbg!($input);
         let parser = &mut $parser;
         let input = $input;
         $ass!(expect(vec![$($expected),*]),
               if let Some(str_to_src_strm) = &str_to_src_strm {
                   parse_all(parser, str_to_src_strm(input))
               } else {
                   parse_all(parser, DA::TT::from_str(input).iter())
               });
        };
    }

    // Basics
    test!("" => []);
    test!(" " => [Ok(text(" "))]);
    test!("  " => [Ok(text("  "))]);
    test!("a" => [Ok(text("a"))]);
    test!("a " => [Ok(text("a "))]);
    test!(" a" => [Ok(text(" a"))]);
    test!(" a " => [Ok(text(" a "))]);
    test!("xyz" => [Ok(text("xyz"))]);
    test!("a b" => [Ok(text("a b"))]);
    test!("a  b" => [Ok(text("a  b"))]);
    test!("a b c" => [Ok(text("a b c"))]);
    test!("   a  b c    " => [Ok(text("   a  b c    "))]);

    test!("{b}" => [Ok(comb(text("b"), EmptyList))]);
    test!("{b }" => [Ok(comb(text("b"), EmptyList))]);
    test!("{bob}" => [Ok(comb(text("bob"), EmptyList))]);
    test!("{b o b}" => [Ok(comb(text("b"), list1(text("o b"))))]);
    test!("{ bo b }" => [Ok(comb(text("bo"), list1(text("b "))))]);
    test!(" c  d   { e  f   g    }     hi  j "
          => [Ok(text(" c  d   ")),
              Ok(comb(text("e"), list1(text(" f   g    ")))),
              Ok(text("     hi  j "))]);

    test!("{}" => [Ok(EmptyNest)]);
    test!("{}{}" => [Ok(EmptyNest),
                     Ok(EmptyNest)]);
    test!("{{}}" => [Ok(comb(EmptyNest, EmptyList))]);
    test!("{{}{}}" => [Ok(comb(EmptyNest, list1(EmptyNest)))]);
    test!("{{{}}}" => [Ok(comb(comb(EmptyNest, EmptyList), EmptyList))]);
    test!(" { } " => [Ok(text(" ")),
                      Ok(EmptyNest),
                      Ok(text(" "))]);
    test!("  { {  }   } " => [Ok(text("  ")),
                              Ok(comb(EmptyNest, list1(text("  ")))),
                              Ok(text(" "))]);
    test!("   {    {   {  } }  }  " => [Ok(text("   ")),
                                        Ok(comb(comb(EmptyNest, EmptyList),
                                                list1(text(" ")))),
                                        Ok(text("  "))]);

    test!(r"\\" => [Ok(text(r"\"))]);
    test!(r"\{" => [Ok(text("{"))]);
    test!(r"\}" => [Ok(text("}"))]);
    test!(r"\{\}" => [Ok(text("{}"))]);
    test!(r"\a" => [Ok(text("a"))]);
    test!(r"\a\b" => [Ok(text("ab"))]);
    test!(r"\" => [Ok(text(""))]);
    test!(r"a\" => [Ok(text("a"))]);
    test!(r"a\b\" => [Ok(text("ab"))]);

    test!(r"{b\ o b}" => [Ok(comb(text("b o"), list1(text("b"))))]);
    test!(r"{\ bo b }" => [Ok(comb(text(" bo"), list1(text("b "))))]);
    test!(r"{\ bo\ b }" => [Ok(comb(text(" bo b"), EmptyList))]);
    test!(r"{\ bo\ b  }" => [Ok(comb(text(" bo b"), list1(text(" "))))]);
    test!(r"{\ }" => [Ok(comb(text(" "), EmptyList))]);
    test!(r"{\  }" => [Ok(comb(text(" "), EmptyList))]);
    test!(r"{\   }" => [Ok(comb(text(" "), list1(text(" "))))]);
    test!(r"{\ \ }" => [Ok(comb(text("  "), EmptyList))]);
    test!(r"{yz\ }" => [Ok(comb(text("yz "), EmptyList))]);
    test!(r"{yz\ \ }" => [Ok(comb(text("yz  "), EmptyList))]);
    test!(r"{yz\ \  \ }" => [Ok(comb(text("yz  "), list1(text(" "))))]);
    test!(r"{y\\z}" => [Ok(comb(text(r"y\z"), EmptyList))]);
    test!(r"{yz\}}" => [Ok(comb(text("yz}"), EmptyList))]);
    test!(r"{yz\{}" => [Ok(comb(text("yz{"), EmptyList))]);
    test!(r"{y\{z}" => [Ok(comb(text("y{z"), EmptyList))]);
    test!(r"{\{ yz}" => [Ok(comb(text("{"), list1(text("yz"))))]);
    test!("{\\\n}" => [Ok(comb(text("\n"), EmptyList))]);
    test!("{\\\t}" => [Ok(comb(text("\t"), EmptyList))]);
    test!("{\\\t\\\n}" => [Ok(comb(text("\t\n"), EmptyList))]);

    test!("{" => [Err(MissingEndChar)]);
    test!("}" => [Err(UnbalancedEndChar(PosIgnore))]);
    test!("␛{" => [Ok(text("␛")),
                   Err(MissingEndChar)]);
    test!("␛}" => [Err(UnbalancedEndChar(PosIgnore))]);

    test!("a b{\n{cd}  { }   { {e\re  {\tf}}\t   g  }\t hi \n j \t\t}k\nλ{ m{{}\r\r}o}\n"
          => [Ok(text("a b")),
              Ok(comb(comb(text("cd"), EmptyList),
                      list5(text(" "),
                            EmptyNest,
                            text("   "),
                            comb(comb(text("e"), list2(text("e  "),
                                                       comb(text("f"), EmptyList))),
                                 list1(text("   g  "))),
                            text("\t hi \n j \t\t")))),
              Ok(text("k\nλ")),
              Ok(comb(text("m"), list2(comb(EmptyNest, list1(text("\r"))), text("o")))),
              Ok(text("\n"))]);

    // TODO: A lot more

    // Custom delimiters

    let mut c = custom_delim::parser(p, custom_delim::Spec {
        nest_start: vec!['⟪'],
        nest_end: vec!['⟫'],
        nest_escape: vec!['␛'],
        whitespace: vec!['-'],
    });
    test!("" =>(c) []);
    test!("{}" =>(c) [Ok(text("{}"))]);
    test!("{a}" =>(c) [Ok(text("{a}"))]);
    test!("⟪⟫" =>(c) [Ok(EmptyNest)]);
    test!("⟪ ⟫" =>(c) [Ok(comb(text(" "), EmptyList))]);
    test!("⟪a⟫" =>(c) [Ok(comb(text("a"), EmptyList))]);
    test!("⟪ a ⟫" =>(c) [Ok(comb(text(" a "), EmptyList))]);
    test!("⟪a⟫" =>(c) [Ok(comb(text("a"), EmptyList))]);
    test!("⟪-a⟫" =>(c) [Ok(comb(text("a"), EmptyList))]);
    test!("⟪--a⟫" =>(c) [Ok(comb(text("a"), EmptyList))]);
    test!("⟪a-⟫" =>(c) [Ok(comb(text("a"), EmptyList))]);
    test!("⟪a--⟫" =>(c) [Ok(comb(text("a"), list1(text("-"))))]);
    test!("⟪-a-⟫" =>(c) [Ok(comb(text("a"), EmptyList))]);
    test!("⟪a-b⟫" =>(c) [Ok(comb(text("a"), list1(text("b"))))]);
    test!("⟪--a---b--⟫" =>(c) [Ok(comb(text("a"), list1(text("--b--"))))]);
    test!("a-⟪b-c⟫d-" =>(c) [Ok(text("a-")),
                             Ok(comb(text("b"), list1(text("c")))),
                             Ok(text("d-"))]);
    test!("␛␛" =>(c) [Ok(text("␛"))]);
    test!("␛⟪" =>(c) [Ok(text("⟪"))]);
    test!("␛⟫" =>(c) [Ok(text("⟫"))]);
    test!("␛⟪␛⟫" =>(c) [Ok(text("⟪⟫"))]);
    test!(r"\\" =>(c) [Ok(text(r"\\"))]);
    test!(r"\⟪\⟫" =>(c) [Ok(text(r"\")),
                         Ok(comb(text(r"\"), EmptyList))]);
    test!(r"\⟪" =>(c) [Ok(text(r"\")),
                       Err(MissingEndChar)]);
    test!(r"\⟫" =>(c) [Err(UnbalancedEndChar(PosIgnore))]);

    let mut c = custom_delim::parser(c, custom_delim::Spec {
        nest_start: vec!['⟪', '⟦'],
        nest_end: vec!['⟫', '⟧'],
        nest_escape: vec!['␛', '⃠'],
        whitespace: vec!['.', ':'],
    });
    test!("⟪⟫" =>(c) [Ok(EmptyNest)]);
    test!("⟦⟧" =>(c) [Ok(EmptyNest)]);
    test!("⟪⟧" =>(c) [Ok(EmptyNest)]);
    test!("⟦.:..::⟫" =>(c) [Ok(EmptyNest)]);
    test!("␛⃠" =>(c) [Ok(text("⃠"))]);
    test!("⃠␛" =>(c) [Ok(text("␛"))]);
    test!("⃠⟪␛⟫" =>(c) [Ok(text("⟪⟫"))]);
    test!(r"\⟦" =>(c) [Ok(text(r"\")),
                       Err(MissingEndChar)]);
    test!(r"\⟧" =>(c) [Err(UnbalancedEndChar(PosIgnore))]);

    // Parsing modes for Operatives and Applicatives. (This doesn't really
    // exercise combiners/macros, just does the bare minimum with them to test
    // the core parser's fixed modes for them.)

    struct BasicCombiners {
        o: &'static str, // operative
        a: &'static str, // applicative
        r: &'static str, // remove form
        c: &'static str, // allocate combination form
        f: &'static str, // failing allocate
    };

    impl<DA> OperatorBindings<DA> for BasicCombiners
        where DA: DatumAllocator,
              <DA::TT as Text>::Chunk: From<&'static str>,
    {
        type OR = Box<OpFn<DA, Self::CE>>;
        type AR = Box<ApFn<DA, Self::CE>>;
        type CE = ();

        fn lookup(&mut self, operator: &Datum<DA::TT, DA::ET, DA::DR>)
                  -> Option<Combiner<Self::OR, Self::AR>>
        {
            let just_operands = |_operator, operands, _dalloc: &mut DA| {
                Ok(Some(operands))
            };

            let remove = |_operator, _operands, _dalloc: &mut DA| {
                Ok(None)
            };

            let comb_alloc = |_operator, _operands, dalloc: &mut DA| {
                Ok(Some(Datum::Combination {
                    operator: dalloc.new_datum(Datum::EmptyNest)?,
                    operands: dalloc.new_datum(Datum::EmptyList)?,
                }))
            };

            let fail_alloc = |_operator, _operands, _dalloc: &mut DA| {
                Err(FailedAlloc(AllocError::AllocExhausted))
            };

            if let Datum::Text(text) = operator {
                if text.partial_eq(&DA::TT::from_str(self.o)) {
                    return Some(Combiner::Operative(Box::new(just_operands)))
                } else if text.partial_eq(&DA::TT::from_str(self.a)) {
                    return Some(Combiner::Applicative(Box::new(just_operands)))
                } else if text.partial_eq(&DA::TT::from_str(self.r)) {
                    return Some(Combiner::Applicative(Box::new(remove)))
                } else if text.partial_eq(&DA::TT::from_str(self.c)) {
                    return Some(Combiner::Operative(Box::new(comb_alloc)))
                } else if text.partial_eq(&DA::TT::from_str(self.f)) {
                    return Some(Combiner::Operative(Box::new(fail_alloc)))
                }
            }
            None
        }
    }

    let mut c = Parser {
        classifier: DefaultCharClassifier,
        allocator: c.allocator,
        bindings: BasicCombiners{o: "oo", a: "aa", r: "#", c: "cc", f: "ff"},
    };
    // Operatives get all the text to the end of the nest form unbroken
    // regardless if any of it looks like other nest forms.
    test!("{oo}" =>(c) [Ok(text(""))]);
    test!("{oo }" =>(c) [Ok(text(""))]);
    test!("{oo  }" =>(c) [Ok(text(" "))]);
    test!("{oo{}}" =>(c) [Ok(text("{}"))]);
    test!("{oo zab {zz} yo}" =>(c) [Ok(text("zab {zz} yo"))]);
    test!("{\n oo  {\n zab {{} yo}}}" =>(c) [Ok(text(" {\n zab {{} yo}}"))]);
    test!("{u {oo zab {zz} yo}}" =>(c) [Ok(comb(text("u"),
                                                list1(text("zab {zz} yo"))))]);
    test!("{{oo}}" =>(c) [Ok(comb(text(""), EmptyList))]);
    test!("{{oo zz} yy}" =>(c) [Ok(comb(text("zz"), list1(text("yy"))))]);
    test!("{{oo oo} yy}" =>(c) [Ok(text("yy"))]);
    test!("{{{oo oo} oo} yy}" =>(c) [Ok(text("yy"))]);
    test!("{oo {}" =>(c) [Err(MissingEndChar)]);
    test!("{oo {" =>(c) [Err(MissingEndChar)]);
    test!("{oo}}" =>(c) [Ok(text("")),
                         Err(UnbalancedEndChar(PosIgnore))]);
    // Applicatives get a list of the parsed operands.
    test!("{aa}" =>(c) [Ok(EmptyList)]);
    test!("{aa }" =>(c) [Ok(EmptyList)]);
    test!("{aa  }" =>(c) [Ok(list1(text(" ")))]);
    test!("{aa{}}" =>(c) [Ok(list1(EmptyNest))]);
    test!("{aa zab {zz} yo}" =>(c) [Ok(list3(text("zab "),
                                             comb(text("zz"), EmptyList),
                                             text(" yo")))]);
    test!("{\n aa  {\n zab {{} yo}}}"
          =>(c) [Ok(list2(text(" "), comb(text("zab"),
                                          list1(comb(EmptyNest,
                                                     list1(text("yo")))))))]);
    test!("{u {aa zab {zz} yo}}"
          =>(c) [Ok(comb(text("u"), list1(list3(text("zab "),
                                                comb(text("zz"), EmptyList),
                                                text(" yo")))))]);
    test!("{{aa}}" =>(c) [Ok(comb(EmptyList, EmptyList))]);
    test!("{{aa zz} yy}" =>(c) [Ok(comb(list1(text("zz")), list1(text("yy"))))]);
    test!("{{aa oo} yy}" =>(c) [Ok(comb(list1(text("oo")), list1(text("yy"))))]);
    test!("{{oo aa} yy}" =>(c) [Ok(list1(text("yy")))]);
    test!("{{{aa aa} aa} yy}" =>(c) [Ok(comb(comb(list1(text("aa")), list1(text("aa"))),
                                             list1(text("yy"))))]);
    test!("{{{oo aa} oo} yy}" =>(c) [Ok(comb(list1(text("oo")), list1(text("yy"))))]);
    test!("{aa {}" =>(c) [Err(MissingEndChar)]);
    test!("{aa {" =>(c) [Err(MissingEndChar)]);
    test!("{aa}}" =>(c) [Ok(EmptyList),
                         Err(UnbalancedEndChar(PosIgnore))]);
    // Combiners can indicate that the original form should be entirely removed.
    test!("{#}" =>(c) []);
    test!("{# foo}" =>(c) []);
    test!("{# {oo zab {zz} yo}}" =>(c) []);
    test!("{# {aa zab {zz} yo}}" =>(c) []);
    test!("{# {u oof}}" =>(c) []);
    test!("{oo {#}}" =>(c) [Ok(text("{#}"))]);
    test!("{oo {# {oo zab}}}" =>(c) [Ok(text("{# {oo zab}}"))]);
    test!("{aa {#}}" =>(c) [Ok(EmptyList)]);
    test!("{aa {# {aa bar}}}" =>(c) [Ok(EmptyList)]);
    test!("{aa {#}{##}{#}}" =>(c) [Ok(list1(comb(text("##"), EmptyList)))]);
    test!("{u {#}}" =>(c) [Ok(comb(text("u"), EmptyList))]);
    test!("{u z {#} y}" =>(c) [Ok(comb(text("u"), list2(text("z "), text(" y"))))]);
    test!("{{#}}" =>(c) [Ok(EmptyNest)]);
    test!("{{# aa}oo xyz}" =>(c) [Ok(text("xyz"))]);
    test!("{{# oo}aa xyz}" =>(c) [Ok(list1(text("xyz")))]);
    test!("{{# {oo {#}}} u}" =>(c) [Ok(comb(text("u"), EmptyList))]);
    test!("{ {#}  u}" =>(c) [Ok(comb(text("u"), EmptyList))]);
    test!("{  {#}  {#}  u}" =>(c) [Ok(comb(text("u"), EmptyList))]);
    test!("{  {#}  {#}  u{# u}x{#}\n{# y}{#}{# z}z}"
          =>(c) [Ok(comb(text("u"), list3(text("x"), text("\n"), text("z"))))]);
    test!("x{#}X" =>(c) [Ok(text("x")),
                         Ok(text("X"))]);
    test!(" {# oo}  x {# aa} X  {#}" =>(c) [Ok(text(" ")),
                                            Ok(text("  x ")),
                                            Ok(text(" X  "))]);
    // Combiners can use the `Parser`'s allocator.
    test!("{cc}" =>(c) [Ok(comb(EmptyNest, EmptyList))]);
    test!("{cc} {cc}" =>(c) [Ok(comb(EmptyNest, EmptyList)),
                             Ok(text(" ")),
                             Ok(comb(EmptyNest, EmptyList))]);
    test!("{oo {cc a b}}" =>(c) [Ok(text("{cc a b}"))]);
    test!("{aa {cc 1 2}{cc 3 4 5}}" =>(c) [Ok(list2(comb(EmptyNest, EmptyList),
                                                    comb(EmptyNest, EmptyList)))]);
    test!("{ff}" =>(c) [Err(FailedAlloc(AllocError::AllocExhausted))]);
    test!("{ff} zab" =>(c) [Err(FailedAlloc(AllocError::AllocExhausted)),
                            Ok(text(" zab"))]);
    test!("{oo {ff}}" =>(c) [Ok(text("{ff}"))]);
    test!("{aa x{ff}y}" =>(c) [Err(FailedAlloc(AllocError::AllocExhausted)),
                               Err(UnbalancedEndChar(PosIgnore))]);
}

// TODO: Suite for Parsers that provide character position.

// TODO: Suite for Parsers that provide UTF-8 byte position.
