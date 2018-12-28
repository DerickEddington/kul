//! Suites of tests applied across multiple crates


use super::*;


/// Basic test suite that checks the basic syntax and forms and does not
/// exercise macros/combiners nor extra types.
pub fn test_suite0<P>(p: &mut P)
    where P: Parser<'static>,
          P::DR: Debug,
          P::CE: Debug,
{
    use kruvi_core::Datum::*;

    let current_input = std::cell::Cell::new("");

    let text = |val, byte_pos, char_pos|
                   Text(PosStr{val, src: current_input.get(), byte_pos, char_pos});

    let comb = |rator, rands|
                   Combination{operator: dr(rator), operands: dr(rands)};

    let list = |elem, next| List{elem: dr(elem), next: dr(next)};
    let list1 = |elem| list(elem, EmptyList);

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
        {current_input.set($input);
         $ass!(expect(vec![$($expected),*]),
               parse_all($parser, current_input.get()));};
    }

    // Basics
    test!("" => []);
    test!(" " => [Ok(text(" ", 0, 0))]);
    test!("  " => [Ok(text("  ", 0, 0))]);
    test!("a" => [Ok(text("a", 0, 0))]);
    test!("a " => [Ok(text("a ", 0, 0))]);
    test!(" a" => [Ok(text(" a", 0, 0))]);
    test!(" a " => [Ok(text(" a ", 0, 0))]);
    test!("xyz" => [Ok(text("xyz", 0, 0))]);
    test!("a b" => [Ok(text("a b", 0, 0))]);
    test!("a  b" => [Ok(text("a  b", 0, 0))]);
    test!("a b c" => [Ok(text("a b c", 0, 0))]);
    test!("   a  b c    " => [Ok(text("   a  b c    ", 0, 0))]);

    test!("{b}" => [Ok(comb(text("b", 1, 1), EmptyList))]);
    test!("{bob}" => [Ok(comb(text("bob", 1, 1), EmptyList))]);
    test!("{b o b}" => [Ok(comb(text("b", 1, 1), list1(text("o b", 3, 3))))]);
    test!("{ bo b }" => [Ok(comb(text("bo", 2, 2), list1(text("b ", 5, 5))))]);
    test!(" c  d   { e  f   g    }     hi  j "
          => [Ok(text(" c  d   ", 0, 0)),
              Ok(comb(text("e", 10, 10), list1(text(" f   g    ", 12, 12)))),
              Ok(text("     hi  j ", 23, 23))]);
    test!(r"\\" => [Ok(text(r"\\", 0, 0))]);
    test!(r"\{" => [Ok(text(r"\{", 0, 0))]);
    test!(r"\}" => [Ok(text(r"\}", 0, 0))]);
    test!(r"\{\}" => [Ok(text(r"\{\}", 0, 0))]);

    // Custom delimiters
    {
        let c = &mut CustomDelimParser {
            parser: p,
            nest_start: Some(vec!['⟪']),
            nest_end: Some(vec!['⟫']),
            nest_escape: Some(vec!['␛']),
            whitespace: Some(vec!['-']),
        };
        test!("" =>(c) []);
        test!("{}" =>(c) [Ok(text("{}", 0, 0))]);
        test!("{a}" =>(c) [Ok(text("{a}", 0, 0))]);
        test!("⟪⟫" =>(c) [Ok(EmptyNest)]);
        test!("⟪ ⟫" =>(c) [Ok(comb(text(" ", 3, 1), EmptyList))]);
        test!("⟪a⟫" =>(c) [Ok(comb(text("a", 3, 1), EmptyList))]);
        test!("⟪ a ⟫" =>(c) [Ok(comb(text(" a ", 3, 1), EmptyList))]);
        test!("⟪a⟫" =>(c) [Ok(comb(text("a", 3, 1), EmptyList))]);
        test!("⟪-a⟫" =>(c) [Ok(comb(text("a", 4, 2), EmptyList))]);
        test!("⟪-a-⟫" =>(c) [Ok(comb(text("a", 4, 2), EmptyList))]);
        test!("⟪a-b⟫" =>(c) [Ok(comb(text("a", 3, 1), list1(text("b", 5, 3))))]);
        test!("⟪--a---b--⟫"
              =>(c) [Ok(comb(text("a", 5, 3), list1(text("--b--", 7, 5))))]);
        test!("␛␛" =>(c) [Ok(text("␛␛", 0, 0))]);
        test!("␛⟪" =>(c) [Ok(text("␛⟪", 0, 0))]);
        test!("␛⟫" =>(c) [Ok(text("␛⟫", 0, 0))]);
        test!("␛⟪␛⟫" =>(c) [Ok(text("␛⟪␛⟫", 0, 0))]);
        test!("a-⟪b-c⟫d-" =>(c) [Ok(text("a-", 0, 0)),
                                 Ok(comb(text("b", 5, 3), list1(text("c", 7, 5)))),
                                 Ok(text("d-", 11, 7))]);
    }
    {
        let c = &mut CustomDelimParser {
            parser: p,
            nest_start: Some(vec!['[']),
            nest_end: Some(vec![']']),
            nest_escape: None,
            whitespace: None,
        };
        test!("{}" =>(c) [Ok(text("{}", 0, 0))]);
        test!("[]" =>(c) [Ok(EmptyNest)]);
        test!("[ ]" =>(c) [Ok(EmptyNest)]);
        test!("[  a   b  ]"
              =>(c) [Ok(comb(text("a", 3, 3), list1(text("  b  ", 5, 5))))]);
        test!(r"\\" =>(c) [Ok(text(r"\\", 0, 0))]);
        test!(r"\[" =>(c) [Ok(text(r"\[", 0, 0))]);
        test!(r"\]" =>(c) [Ok(text(r"\]", 0, 0))]);
        test!(r"\[\]" =>(c) [Ok(text(r"\[\]", 0, 0))]);
    }
    {
        let c = &mut CustomDelimParser {
            parser: p,
            nest_start: Some(vec!['⟪', '⟦']),
            nest_end: Some(vec!['⟫', '⟧']),
            nest_escape: Some(vec!['␛', '⃠']),
            whitespace: Some(vec!['.', ':']),
        };
        test!("⟪⟫" =>(c) [Ok(EmptyNest)]);
        test!("⟦⟧" =>(c) [Ok(EmptyNest)]);
        test!("⟪⟧" =>(c) [Ok(EmptyNest)]);
        test!("⟦.:..::⟫" =>(c) [Ok(EmptyNest)]);
        test!("⃠⟪␛⟫" =>(c) [Ok(text("⃠⟪␛⟫", 0, 0))]);
    }

    // TODO: A lot more
}
