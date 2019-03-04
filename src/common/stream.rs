//! A `Parser` for parsing inputs that are chunks from streaming sources.

use std::{rc::Rc, hash::Hash};

use crate::{
    Parser as ParserStruct, ParseIterItem,
    parser::{BoxDatumAllocator, HashMapOperatorBindings},
    text::{TextVec, chunk::PosStrish},
    combiner::{OpFn, ApFn},
    source_stream::StrishIterSourceStream,
};

use super::helper::collect_up_to_first_err;


#[doc(no_inline)]
pub use crate::parser::DefaultCharClassifier as CharClassifier;


/// Chosen so that the application can choose whether or not to use all the
/// capacity of an input stream's `String` chunks, and so that referenced parts
/// of chunks are zero-copy, and so that `char` positions are tracked (relative
/// to the input stream), and so that, when chunks are broken around escape
/// characters, a simple `Vec` is used to logically concatenate them.
///
/// This `Text` type is a `TextVec` of `PosStrish<Rc<String>>` chunks.
pub type Text = TextVec<PosStrish<Rc<String>>>;

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
pub type DatumAllocator<Extra = ()> = BoxDatumAllocator<Text, Extra>;

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
pub type OperatorBindings<Extra = (), CombinerError = ()>
    = HashMapOperatorBindings<DatumAllocator<Extra>,
                              Box<OpFn<DatumAllocator<Extra>,
                                       CombinerError>>,
                              Box<ApFn<DatumAllocator<Extra>,
                                       CombinerError>>,
                              CombinerError>;

/// A [`Parser`] for parsing inputs that are chunks from a streaming source, as
/// a sequence of `String` chunks yielded by an `Iterator`, that uses: the
/// default characters as delimiters, `Box`es to allocate the [`Datum`]s in ASTs
/// returned from parsing, and [`HashMap`]s to bind operator sub-forms to
/// [`Combiner`] macro functions.
///
/// The AST values returned from parsing are zero-copy with regard to the input
/// parts they refer to, achieved by shared ownership, i.e. `Rc`-wrapping, of
/// the input `String` chunks.
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
pub type Parser<Extra = (), CombinerError = ()>
    = ParserStruct<CharClassifier,
                   DatumAllocator<Extra>,
                   OperatorBindings<Extra, CombinerError>>;

/// The `Result` of parsing a top-level form, and the type of elements returned
/// by [`parse_stream`] and [`parse_stream_with`].
///
/// An `Ok` value contains a [`Datum`] value that is the AST (abstract syntax
/// tree) of the parsed top-level form.
///
/// An `Err` value contains an [`Error`] value variant that describes the error
/// caused by either a syntax error in the input or by one of your [`Combiner`]
/// functions returning a value of your chosen `CombinerError` type parameter.
///
/// [`parse_stream`]: fn.parse_stream.html
/// [`parse_stream_with`]: fn.parse_stream_with.html
/// [`Datum`]: ../../../kul_core/enum.Datum.html
/// [`Error`]: ../../../kul_core/enum.Error.html
/// [`Combiner`]: ../../../kul_core/enum.Combiner.html
pub type TopFormResult<Extra = (), CombinerError = ()>
    = ParseIterItem<DatumAllocator<Extra>,
                    OperatorBindings<Extra, CombinerError>>;


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
    bindings: OperatorBindings<Extra, CombinerError>
) -> Parser<Extra, CombinerError>
    where Extra: Hash + Eq,
{
    ParserStruct {
        classifier: CharClassifier,
        allocator: DatumAllocator::default(),
        bindings,
    }
}

/// Parse the given stream, which is an `Iterator` of `String` chunks, using a
/// [`Parser`] that uses the given `OperatorBindings` value, and return a vector
/// of the results as `Datum` ASTs for each successfully-parsed top-level form
/// and/or an `Error`.
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
pub fn parse_stream_with<I, Extra, CombinerError>(
    input: I,
    bindings: OperatorBindings<Extra, CombinerError>
)
    -> Vec<TopFormResult<Extra, CombinerError>>
    where I: Iterator<Item = String>,
          Extra: Hash + Eq,
{
    let input_source_stream = StrishIterSourceStream::new(input.map(Rc::new));
    collect_up_to_first_err(parser(bindings).parse(input_source_stream))
}

/// Parse the given stream, which is an `Iterator` of `String` chunks, using a
/// [`Parser`] with no bindings of operators, and return a vector of the results
/// as `Datum` ASTs for each successfully-parsed top-level form and/or an
/// `Error`.
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
/// See [`parse_stream_with`], which this uses, for the description of how
/// errors are returned.
///
/// [`Parser`]: type.Parser.html
/// [`Datum::Combination`]: ../../../kul_core/enum.Datum.html#variant.Combination
/// [`parse_stream_with`]: fn.parse_stream_with.html
#[inline]
#[allow(clippy::module_name_repetitions)]
pub fn parse_stream<I>(input: I) -> Vec<TopFormResult>
    where I: Iterator<Item = String>,
{
    let empty_bindings = OperatorBindings::default();
    parse_stream_with(input, empty_bindings)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Datum, Combiner, Text as _, Error,
        datum::DatumBox,
        text::chunk::CharPos,
    };
    use std::{collections::HashMap, iter::FromIterator};

    fn read_into(dest: &mut [u8], from: &[u8]) {
        dest.copy_from_slice(from);
    }

    fn buffer_stream_chunk(from: &str) -> String {
        let buf_size = from.len();
        let mut b = vec![0; buf_size];
        read_into(&mut b, from.as_bytes());
        String::from_utf8(b).unwrap()
    }

    /// Mock stream source
    fn stream<'a>(from: &'a [&'static str]) -> impl Iterator<Item = String> + 'a {
        from.iter().map(|&s| buffer_stream_chunk(s))
    }

    #[test]
    fn parse_stream() {
        assert_eq!(super::parse_stream(stream(&[])), []);
        assert_eq!(super::parse_stream(stream(&[""])), []);
        assert_eq!(super::parse_stream(stream(&["a"])),
                   [Ok(Datum::Text(TextVec::from_str("a")))]);
        assert_eq!(super::parse_stream(stream(&["{", "}"])),
                   [Ok(Datum::EmptyNest)]);
        assert_eq!(super::parse_stream(stream(&["{b", "", "oo}"])),
                   [Ok(Datum::Combination {
                       operator: DatumBox::new(Datum::Text(TextVec::from_str("boo"))),
                       operands: DatumBox::new(Datum::EmptyList),
                   })]);
        assert_eq!(super::parse_stream(stream(&["c{} d", ""])),
                   [Ok(Datum::Text(TextVec::from_str("c"))),
                    Ok(Datum::EmptyNest),
                    Ok(Datum::Text(TextVec::from_str(" d")))]);
        assert_eq!(super::parse_stream(stream(&["e ", "{", "f {", "}"])),
                   [Ok(Datum::Text(TextVec::from_str("e "))),
                    Err(Error::MissingEndChar)]);
        assert_eq!(super::parse_stream(stream(&["", "λ} h"])),
                   [Err(Error::UnbalancedEndChar(CharPos(1)))]);
    }

    #[test]
    fn parse_stream_with() {
        fn bindings() -> OperatorBindings<char, f32> {
            let pairs: Vec<(_, Combiner<Box<OpFn<_, _>>, Box<ApFn<_, _>>>)> = vec![
                (Datum::Text(TextVec::from_str("op")),
                 Combiner::Operative(Box::new(|_, _, _| Ok(Some(Datum::Extra('λ')))))),
                (Datum::Text(TextVec::from_str("e")),
                 Combiner::Applicative(Box::new(|_, _, _|
                                                Err(Error::FailedCombiner(1.5))))),
            ];
            OperatorBindings::new(HashMap::from_iter(pairs.into_iter()))
        }
        assert_eq!(super::parse_stream_with(stream(&[""]), bindings()), []);
        assert_eq!(super::parse_stream_with(stream(&["a"]), bindings()),
                   [Ok(Datum::Text(TextVec::from_str("a")))]);
        assert_eq!(super::parse_stream_with(stream(&["{}"]), bindings()),
                   [Ok(Datum::EmptyNest)]);
        assert_eq!(super::parse_stream_with(stream(&["{o", "p ignored}"]), bindings()),
                   [Ok(Datum::Extra('λ'))]);
        assert_eq!(super::parse_stream_with(stream(&["{z", "z ", "", "}"]), bindings()),
                   [Ok(Datum::Combination {
                           operator: DatumBox::new(Datum::Text(TextVec::from_str("zz"))),
                           operands: DatumBox::new(Datum::EmptyList),
                       })]);
        assert_eq!(super::parse_stream_with(stream(&["", "{", "e}aborted"]), bindings()),
                   [Err(Error::FailedCombiner(1.5))]);
    }
}


// Note: Tests of the `parser` function are in the integration test, because
// that is required for the impl of TestOperatorBindings for
// HashMapOperatorBindings to work, for using `test_suite0`.  (That impl doesn't
// work with unit tests because the test harness compilation actually doesn't
// use the same type that it's impl'ed for.)
