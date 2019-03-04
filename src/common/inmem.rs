//! A `Parser` for parsing inputs that are single in-memory strings.

use std::hash::Hash;

use crate::{
    Parser as ParserStruct, ParseIterItem, Text as _,
    parser::{BoxDatumAllocator, HashMapOperatorBindings},
    text::{TextVec, chunk::PosStr},
    combiner::{OpFn, ApFn},
};

use super::helper::collect_up_to_first_err;


#[doc(no_inline)]
pub use crate::parser::DefaultCharClassifier as CharClassifier;


/// Chosen so that the referenced parts of an input string are zero-copy, and so
/// that `char` and byte positions are tracked (relative to the input `&str`),
/// and so that, when chunks are broken around escape characters, a simple `Vec`
/// is used to logically concatenate them.
///
/// This `Text` type is a `TextVec` of `PosStr` chunks.
pub type Text<'input> = TextVec<PosStr<'input>>;

/// Chosen so that the [`Datum`]s in the AST values returned from parsing are
/// simply allocated in heap `Box`es without limit.
///
/// This `DatumAllocator` type is a `BoxDatumAllocator` of our `Text` type and
/// of the given `Extra` type.
///
/// The `Extra` type parameter determines the type used in the [`Datum::Extra`]
/// variant of our `Datum` type, and it defaults to `()`.
///
/// [`Datum`]: ../../../kul_core/enum.Datum.html
/// [`Datum::Extra`]: ../../../kul_core/enum.Datum.html#variant.Extra
pub type DatumAllocator<'input, Extra = ()> = BoxDatumAllocator<Text<'input>, Extra>;

/// Chosen so that you may establish bindings simply using the `std` [`HashMap`]
/// and with flexible trait objects for the function types.
///
/// This `OperatorBindings` type is a `HashMapOperatorBindings` that binds
/// operator sub-forms, as our [`Datum`] type, to [`Combiner`] macro
/// functions/closures, as `Box`ed `dyn` trait objects.
///
/// The `Extra` type parameter determines the type used in the [`Datum::Extra`]
/// variant of our `Datum` type.  It defaults to `()`.
///
/// The `CombinerError` type parameter determines the type used in the
/// [`Error::FailedCombiner`] variant of the crate's error type which your
/// `Combiner` functions may return.  It defaults to `()`.
///
/// [`Datum`]: ../../../kul_core/enum.Datum.html
/// [`Combiner`]: ../../../kul_core/enum.Combiner.html
/// [`HashMap`]: http://doc.rust-lang.org/std/collections/struct.HashMap.html
/// [`Datum::Extra`]: ../../../kul_core/enum.Datum.html#variant.Extra
/// [`Error::FailedCombiner`]: ../../../kul_core/enum.Error.html#variant.FailedCombiner
pub type OperatorBindings<'input, Extra = (), CombinerError = ()>
    = HashMapOperatorBindings<DatumAllocator<'input, Extra>,
                              Box<OpFn<DatumAllocator<'input, Extra>,
                                       CombinerError>>,
                              Box<ApFn<DatumAllocator<'input, Extra>,
                                       CombinerError>>,
                              CombinerError>;

/// A [`Parser`] for parsing inputs that are single in-memory strings, as
/// `&str`s, that uses: the default characters as delimiters, `Box`es to
/// allocate the [`Datum`]s in ASTs returned from parsing, and [`HashMap`]s to
/// bind operator sub-forms to [`Combiner`] macro functions.
///
/// The AST values returned from parsing are zero-copy with regard to the input
/// parts they refer to, achieved by borrowing sub-slices of the input slice.
///
/// You must decide which types you want to use in the [`Datum::Extra`] variant
/// and for any errors returned by your macro/"combiner" functions in the
/// [`Error::FailedCombiner`] variant.  If unsure, the `()` type is suitable for
/// either or both, and this is their default.
///
/// [`Parser`]: ../../../kul_core/struct.Parser.html
/// [`Datum`]: ../../../kul_core/enum.Datum.html
/// [`HashMap`]: http://doc.rust-lang.org/std/collections/struct.HashMap.html
/// [`Combiner`]: ../../../kul_core/enum.Combiner.html
/// [`Datum::Extra`]: ../../../kul_core/enum.Datum.html#variant.Extra
/// [`Error::FailedCombiner`]: ../../../kul_core/enum.Error.html#variant.FailedCombiner
pub type Parser<'input, Extra = (), CombinerError = ()>
    = ParserStruct<CharClassifier,
                   DatumAllocator<'input, Extra>,
                   OperatorBindings<'input, Extra, CombinerError>>;

/// The `Result` of parsing a top-level form, and the type of elements returned
/// by [`parse_str`] and [`parse_str_with`].
///
/// An `Ok` value contains a [`Datum`] value that is the AST (abstract syntax
/// tree) of the parsed top-level form.
///
/// An `Err` value contains an [`Error`] value variant that describes the error
/// caused by either a syntax error in the input or by one of your [`Combiner`]
/// functions returning a value of your chosen `CombinerError` type parameter.
///
/// [`parse_str`]: fn.parse_str.html
/// [`parse_str_with`]: fn.parse_str_with.html
/// [`Datum`]: ../../../kul_core/enum.Datum.html
/// [`Error`]: ../../../kul_core/enum.Error.html
/// [`Combiner`]: ../../../kul_core/enum.Combiner.html
pub type TopFormResult<'input, Extra = (), CombinerError = ()>
    = ParseIterItem<DatumAllocator<'input, Extra>,
                    OperatorBindings<'input, Extra, CombinerError>>;


/// Make a new `Parser` that uses the given `OperatorBindings` value and that
/// uses the types chosen by this module.
///
/// You may call the returned `Parser`'s [`parse`] method and use the
/// [`ParseIter`] values however you can.
///
/// [`parse`]: ../../../kul_core/struct.Parser.html#method.parse
/// [`ParseIter`]: ../../../kul_core/struct.ParseIter.html
#[inline]
pub fn parser<Extra, CombinerError>(
    bindings: OperatorBindings<'_, Extra, CombinerError>
) -> Parser<'_, Extra, CombinerError>
    where Extra: Hash + Eq,
{
    ParserStruct {
        classifier: CharClassifier,
        allocator: DatumAllocator::default(),
        bindings,
    }
}

/// Parse the given string slice using a [`Parser`] that uses the given
/// `OperatorBindings` value, and return a vector of the results as `Datum` ASTs
/// for each successfully-parsed top-level form and/or an `Error`.
///
/// This enables you to give bindings for parsing certain nest forms in your
/// custom ways and enables you to [choose](type.OperatorBindings.html) what to
/// use for the `Extra` and `CombinerError` type parameters.
///
/// If an error is returned by the parser, parsing will be immediately aborted
/// and the last element of the returned vector will be that error.  If instead
/// you want to continue trying to parse after an error occurred, use the
/// [`parser` function] to directly work with a `Parser`.
///
/// [`Parser`]: type.Parser.html
/// [`parser` function]: fn.parser.html
pub fn parse_str_with<'i, Extra, CombinerError>(
    input: &'i str,
    bindings: OperatorBindings<'i, Extra, CombinerError>
) -> Vec<TopFormResult<'i, Extra, CombinerError>>
    where Extra: Hash + Eq,
{
    let input_text = Text::from_str(input);
    let input_source_stream = input_text.iter();
    collect_up_to_first_err(parser(bindings).parse(input_source_stream))
}

/// Parse the given string slice using a [`Parser`] with no bindings of
/// operators, and return a vector of the results as `Datum` ASTs for each
/// successfully-parsed top-level form and/or an `Error`.
///
/// This will parse all nest forms recursively in the same way and produce the
/// [`Datum::Combination`] variant to represent each in the returned ASTs.  This
/// produces a simplistic kind of S-expression structure that is useful when you
/// don't want any special parsing of nested text, either because you want to
/// prevent that but still be able to analyze the basic structure, or because
/// your application is so simple that it only uses the format/language/syntax
/// to this extent.
///
/// This uses `()` for the types in the `Datum::Extra` and
/// `Error::FailedCombiner` variants, because these variants cannot be produced
/// when there are no bindings.
///
/// See [`parse_str_with`], which this uses, for the description of how errors
/// are returned.
///
/// [`Parser`]: type.Parser.html
/// [`Datum::Combination`]: ../../../kul_core/enum.Datum.html#variant.Combination
/// [`parse_str_with`]: fn.parse_str_with.html
#[inline]
pub fn parse_str(input: &str) -> Vec<TopFormResult<'_>> {
    let empty_bindings = OperatorBindings::default();
    parse_str_with(input, empty_bindings)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Datum, Combiner, Error,
        datum::DatumBox,
        text::chunk::StrPos,
    };
    use std::{collections::HashMap, iter::FromIterator};

    #[test]
    fn parse_str() {
        assert_eq!(super::parse_str(""), []);
        assert_eq!(super::parse_str("a"), [Ok(Datum::Text(TextVec::from_str("a")))]);
        assert_eq!(super::parse_str("{}"), [Ok(Datum::EmptyNest)]);
        assert_eq!(super::parse_str("{b}"),
                   [Ok(Datum::Combination {
                       operator: DatumBox::new(Datum::Text(TextVec::from_str("b"))),
                       operands: DatumBox::new(Datum::EmptyList),
                   })]);
        assert_eq!(super::parse_str("c{} d"),
                   [Ok(Datum::Text(TextVec::from_str("c"))),
                    Ok(Datum::EmptyNest),
                    Ok(Datum::Text(TextVec::from_str(" d")))]);
        assert_eq!(super::parse_str("e {f {}"),
                   [Ok(Datum::Text(TextVec::from_str("e "))),
                    Err(Error::MissingEndChar)]);
        assert_eq!(super::parse_str("λ} h"),
                   [Err(Error::UnbalancedEndChar(
                       StrPos{src: "λ} h", byte_pos: 2, char_pos: 1}))]);
    }

    #[test]
    fn parse_str_with() {
        fn bindings() -> OperatorBindings<'static, u16, bool> {
            let pairs: Vec<(_, Combiner<Box<OpFn<_, _>>, Box<ApFn<_, _>>>)> = vec![
                (Datum::Text(TextVec::from_str("op")),
                 Combiner::Operative(Box::new(|_, _, _| Ok(Some(Datum::Extra(1)))))),
                (Datum::Text(TextVec::from_str("e")),
                 Combiner::Applicative(Box::new(|_, _, _|
                                                Err(Error::FailedCombiner(true))))),
            ];
            OperatorBindings::new(HashMap::from_iter(pairs.into_iter()))
        }
        assert_eq!(super::parse_str_with("", bindings()), []);
        assert_eq!(super::parse_str_with("a", bindings()),
                   [Ok(Datum::Text(TextVec::from_str("a")))]);
        assert_eq!(super::parse_str_with("{}", bindings()),
                   [Ok(Datum::EmptyNest)]);
        assert_eq!(super::parse_str_with("{op ignored}", bindings()),
                   [Ok(Datum::Extra(1))]);
        assert_eq!(super::parse_str_with("{zz }", bindings()),
                   [Ok(Datum::Combination {
                           operator: DatumBox::new(Datum::Text(TextVec::from_str("zz"))),
                           operands: DatumBox::new(Datum::EmptyList),
                       })]);
        assert_eq!(super::parse_str_with("{e}aborted", bindings()),
                   [Err(Error::FailedCombiner(true))]);
    }
}


// Note: Tests of the `parser` function are in the integration test, because
// that is required for the impl of TestOperatorBindings for
// HashMapOperatorBindings to work, for using `test_suite0`.  (That impl doesn't
// work with unit tests because the test harness compilation actually doesn't
// use the same type that it's impl'ed for.)
