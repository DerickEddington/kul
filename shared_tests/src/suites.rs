//! Suites of tests applied across multiple crates


use super::*;


/// Basic test suite that checks the basic syntax and forms and does not
/// exercise macros/combiners nor extra types.
pub fn test_suite0<DA>(mut p: Parser<DefaultCharClassifier,
                                     DA,
                                     EmptyOperatorBindings<DA>>)
    where DA: DatumAllocator,
          <DA::TT as Text>::Chunk: From<&'static str>,
          DA::DR: Debug,
          <DA::TT as TextBase>::Pos: Debug,
{
    use Datum::*;
    use Error::*;

    let text = |val| Text(ExpectedText(val));

    let comb = |rator, rands|
                   Combination{operator: dr(rator), operands: dr(rands)};

    let list = |elem, next| List{elem: dr(elem), next: dr(next)};
    let list1 = |elem| list(elem, EmptyList);
    // let list2 = |e1, e2| list(e1, list(e2, EmptyList));

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
        {$ass!(expect(vec![$($expected),*]),
               parse_all(&mut $parser,
                         DA::TT::from_str($input).iter()));};
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
    test!("{bob}" => [Ok(comb(text("bob"), EmptyList))]);
    test!("{b o b}" => [Ok(comb(text("b"), list1(text("o b"))))]);
    test!("{ bo b }" => [Ok(comb(text("bo"), list1(text("b "))))]);
    test!(" c  d   { e  f   g    }     hi  j "
          => [Ok(text(" c  d   ")),
              Ok(comb(text("e"), list1(text(" f   g    ")))),
              Ok(text("     hi  j "))]);

    test!("{}" => [Ok(EmptyNest)]);
    // test!("{{}}" => [Ok(comb(comb(EmptyNest, EmptyList), EmptyList))]);

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
    test!(r"{yz\ }" => [Ok(comb(text("yz "), EmptyList))]);
    test!(r"{yz\ \ }" => [Ok(comb(text("yz  "), EmptyList))]);
    test!(r"{yz\ \  \ }" => [Ok(comb(text("yz  "), list1(text(" "))))]);
    // test!(r"{y\\z}" => [Ok(comb(text(r"y\z"), EmptyList))]);
    // test!(r"{yz\}}" => [Ok(comb(text("yz}"), EmptyList))]);
    // test!(r"{yz\{}" => [Ok(comb(text("yz{"), EmptyList))]);
    // test!(r"{y\{z}" => [Ok(comb(text("y{z"), EmptyList))]);
    // test!(r"{\{ yz}" => [Ok(comb(text("{"), list1(text("yz"))))]);

    test!("{" => [Err(MissingEndChar)]);
    test!("}" => [Err(UnbalancedEndChar(PosIgnore))]);
    test!("␛{" => [Ok(text("␛")),
                   Err(MissingEndChar)]);
    test!("␛}" => [Err(UnbalancedEndChar(PosIgnore))]);

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
}

// TODO: Suite for Parsers that provide character position.

// TODO: Suite for Parsers that provide UTF-8 byte position.
