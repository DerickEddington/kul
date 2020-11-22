//! A `Parser` for parsing inputs that are in-memory strings, for no-heap
//! constrained applications.

use core::iter::TakeWhile;

use crate::{
    Parser as ParserStruct, ParseIter, ParseIterItem, Combiner, Error, Text as _,
    parser::premade::{SliceDatumAllocator, PairOperatorBindings},
    text::{self, premade::TextDatumList, chunk::premade::{PosStr, StrPos}},
    datum::premade::{MutRefDatum, DatumMutRef},
    combiner::{OpFn, ApFn},
};


#[doc(no_inline)]
pub use crate::parser::premade::DefaultCharClassifier as CharClassifier;


/// Chosen so that the referenced parts of an input string are zero-copy, and so
/// that `char` and byte positions are tracked (relative to the input `&str`),
/// and so that, when chunks are broken around escape characters, a `Datum` list
/// is used to logically concatenate them.
///
/// This `Text` type is a `TextDatumList` of `PosStr` chunks.
pub type Text<'input, 'alloc, Extra = ()>
    = TextDatumList<'alloc, PosStr<'input>, Extra>;

/// Chosen so that the [`Datum`]s in the AST values returned from parsing are
/// allocated from a contiguous slice of them, which itself must be
/// pre-allocated by you, e.g. as an array on the stack.
///
/// This `DatumAllocator` type is a `SliceDatumAllocator` of our `Text` type and
/// of the given `Extra` type.
///
/// The `Extra` type parameter determines the type used in the [`Datum::Extra`]
/// variant of our `Datum` type, and it defaults to `()`.
///
/// [`Datum`]: ../../enum.Datum.html
/// [`Datum::Extra`]: ../../enum.Datum.html#variant.Extra
pub type DatumAllocator<'input, 'alloc, Extra = ()>
    = SliceDatumAllocator<'alloc, Text<'input, 'alloc, Extra>, Extra>;

/// A contiguous slice of our `Datum` type.  Used by our [`DatumAllocator`] via
/// our [`parser`] constructor.
///
/// Useful when defining a borrowed array of `Datum`s to allocate from.
///
/// [`DatumAllocator`]: type.DatumAllocator.html
/// [`parser`]: fn.parser.html
pub type DatumSlice<'input, 'alloc, Extra = ()>
    = &'alloc mut [DatumType<'input, 'alloc, Extra>];

/// Our `Datum` type.  Contains mutable borrows of other `Datum`s of our type,
/// wrapped in our [`DatumRef`] type.
///
/// [`DatumRef`]: type.DatumRef.html
pub type DatumType<'input, 'alloc, Extra = ()>
    = MutRefDatum<'alloc, Text<'input, 'alloc, Extra>, Extra>;

/// Our `Datum` reference type.  Wraps mutable borrows of other `Datum`s of our
/// type.
pub type DatumRef<'input, 'alloc, Extra = ()>
    = DatumMutRef<'alloc, Text<'input, 'alloc, Extra>, Extra>;

/// Chosen so that you may establish bindings using an immutable slice of pairs
/// (2-tuples) and with flexible trait objects for the function types.
///
/// This `OperatorBindings` type is a `PairOperatorBindings` that binds operator
/// sub-forms, as our [`Datum`] type, to [`Combiner`] macro functions/closures,
/// as `&dyn` trait objects.
///
/// The `Extra` type parameter determines the type used in the [`Datum::Extra`]
/// variant of our `Datum` type.  It defaults to `()`.
///
/// The `CombinerError` type parameter determines the type used in the
/// [`Error::FailedCombiner`] variant of the crate's error type which your
/// `Combiner` functions may return.  It defaults to `()`.
///
/// [`Datum`]: ../../enum.Datum.html
/// [`Combiner`]: ../../enum.Combiner.html
/// [`Datum::Extra`]: ../../enum.Datum.html#variant.Extra
/// [`Error::FailedCombiner`]: ../../enum.Error.html#variant.FailedCombiner
pub type OperatorBindings<'input, 'alloc, 'funs, 'bind, Extra = (), CombinerError = ()>
    = PairOperatorBindings<
          BindingsSlice<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>,
          DatumAllocator<'input, 'alloc, Extra>,
          OperativeRef<'input, 'alloc, 'funs, Extra, CombinerError>,
          ApplicativeRef<'input, 'alloc, 'funs, Extra, CombinerError>,
          CombinerError
      >;

/// A slice of pairs (2-tuples) that associate operator sub-forms with macro
/// functions, to bind them.  Used by our [`OperatorBindings`] via our
/// [`parser`] constructor.
///
/// Useful when defining a borrowed array of bindings.
///
/// [`OperatorBindings`]: type.OperatorBindings.html
/// [`parser`]: fn.parser.html
pub type BindingsSlice<'input, 'alloc, 'funs, 'bind, Extra = (), CombinerError = ()>
    = &'bind [BindingsPair<'input, 'alloc, 'funs, Extra, CombinerError>];

/// A pair (2-tuple) that associates an operator sub-form with a macro function,
/// to bind them.
pub type BindingsPair<'input, 'alloc, 'funs, Extra = (), CombinerError = ()>
    = (
        // The operator sub-form
        DatumType<'input, 'alloc, Extra>,
        // The macro function bound to the operator
        Combiner<OperativeRef<'input, 'alloc, 'funs, Extra, CombinerError>,
                 ApplicativeRef<'input, 'alloc, 'funs, Extra, CombinerError>>
    );

/// Our [`Operative`] reference type.  A `&dyn` reference to a function as a
/// trait object.
///
/// [`Operative`]: ../../combiner/enum.Combiner.html#variant.Operative
pub type OperativeRef<'input, 'alloc, 'funs, Extra = (), CombinerError = ()>
    = &'funs OpFn<DatumAllocator<'input, 'alloc, Extra>,
                  CombinerError>;

/// Our [`Applicative`] reference type.  A `&dyn` reference to a function as a
/// trait object.
///
/// [`Applicative`]: ../../combiner/enum.Combiner.html#variant.Applicative
pub type ApplicativeRef<'input, 'alloc, 'funs, Extra = (), CombinerError = ()>
    = &'funs ApFn<DatumAllocator<'input, 'alloc, Extra>,
                  CombinerError>;

/// A [`Parser`] for parsing inputs that are in-memory strings, as `&str`s
/// wrapped by this module's [`Text`] type, that is suitable for no-heap
/// constrained applications that only use the stack, and that uses: the default
/// characters as delimiters, slices to allocate the [`Datum`]s in ASTs returned
/// from parsing, and slices of pairs (2-tuples) to bind operator sub-forms to
/// [`Combiner`] macro functions.
///
/// The AST values returned from parsing are zero-copy with regard to the input
/// parts they refer to, achieved by borrowing sub-slices of the input slice.
///
/// You must decide which types you want to use in the [`Datum::Extra`] variant
/// and for any errors returned by your macro/"combiner" functions in the
/// [`Error::FailedCombiner`] variant.  If unsure, the `()` type is suitable for
/// either or both, and this is their default.
///
/// [`Parser`]: ../../struct.Parser.html
/// [`Text`]: type.Text.html
/// [`Datum`]: ../../enum.Datum.html
/// [`Combiner`]: ../../enum.Combiner.html
/// [`Datum::Extra`]: ../../enum.Datum.html#variant.Extra
/// [`Error::FailedCombiner`]: ../../enum.Error.html#variant.FailedCombiner
pub type Parser<'input, 'alloc, 'funs, 'bind, Extra = (), CombinerError = ()>
    = ParserStruct<
          CharClassifier,
          DatumAllocator<'input, 'alloc, Extra>,
          OperatorBindings<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>
      >;

/// The `Result` of parsing a top-level form, and the type of items yielded by
/// [`parse_text_with`].
///
/// An `Ok` value contains a [`Datum`] value that is the AST (abstract syntax
/// tree) of the parsed top-level form.
///
/// An `Err` value contains an [`Error`] value variant that describes the error
/// caused by either a syntax error in the input or by one of your [`Combiner`]
/// functions returning a value of your chosen `CombinerError` type parameter.
///
/// [`parse_text_with`]: fn.parse_text_with.html
/// [`Datum`]: ../../enum.Datum.html
/// [`Error`]: ../../enum.Error.html
/// [`Combiner`]: ../../enum.Combiner.html
pub type TopFormResult<'input, 'alloc, 'funs, 'bind, Extra = (), CombinerError = ()>
    = ParseIterItem<
          DatumAllocator<'input, 'alloc, Extra>,
          OperatorBindings<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>
      >;

/// Make a new `Parser` that uses the given `DatumSlice` and `BindingsSlice`
/// values for the `DatumAllocator` and `OperatorBindings` and that uses the
/// types chosen by this module.
///
/// You may call the returned `Parser`'s [`parse`] method and use the
/// [`ParseIter`] values however you can.
///
/// [`parse`]: ../../struct.Parser.html#method.parse
/// [`ParseIter`]: ../../struct.ParseIter.html
pub fn parser<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>(
    alloc_from: DatumSlice<'input, 'alloc, Extra>,
    bind_pairs: BindingsSlice<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>
)
    -> Parser<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>
where
    Extra: Eq,
{
    ParserStruct {
        classifier: CharClassifier,
        allocator: DatumAllocator::new(alloc_from),
        bindings: OperatorBindings::new(bind_pairs),
    }
}

/// Make a new `Parser` that uses the given `DatumSlice` value for the
/// `DatumAllocator`, that has no bindings of operators, and that uses the types
/// chosen by this module.
///
/// See the [`parser`] function, which this simply calls with an empty slice for
/// the bindings.
///
/// The returned `Parser` will parse all nest forms recursively in the same way
/// and produce the [`Datum::Combination`] variant to represent each in the
/// returned ASTs.  This produces a simplistic kind of S-expression structure
/// that is useful when you don't want any special parsing of nested text,
/// either because you want to prevent that but still be able to analyze the
/// basic structure, or because your application is so simple that it only uses
/// the format/language/syntax to this extent.
///
/// This uses `()` for the types in the `Datum::Extra` and
/// `Error::FailedCombiner` variants, because these variants cannot be produced
/// when there are no bindings.
///
/// [`parser`]: fn.parser.html
/// [`Datum::Combination`]: ../../enum.Datum.html#variant.Combination
pub fn parser_no_bind<'input, 'alloc, 'funs, 'bind>(
    alloc_from: DatumSlice<'input, 'alloc>,
)
    -> Parser<'input, 'alloc, 'funs, 'bind>
{
    parser(alloc_from, &[])
}

/// Parse the given `Text`, that contains the input string, using the given
/// `Parser`, and return an `Iterator` of the results as `Datum` ASTs for each
/// successfully-parsed top-level form and/or an `Error`.
///
/// This enables you to give bindings for parsing certain nest forms in your
/// custom ways and enables you to [choose][`parser` function] what to use for
/// the `Extra` and `CombinerError` type parameters.
///
/// If an error is returned by the parser, parsing will be immediately aborted
/// and the last item of the returned iterator will be that error.  If instead
/// you want to continue trying to parse after an error occurred, use the
/// [`parser` function] to directly work with a `Parser`.
///
/// The reason this function takes borrows of `Text` and `Parser` values is
/// because the returned iterator borrows them itself and this is the only way
/// the lifetimes can work.
///
/// [`Parser`]: type.Parser.html
/// [`parser` function]: fn.parser.html
pub fn parse_text_with<'t, 'p, 'input, 'alloc, 'funs, 'bind, Extra, CombinerError>(
    input: &'t Text<'input, 'alloc, Extra>,
    parser: &'p mut Parser<'input, 'alloc, 'funs, 'bind, Extra, CombinerError>,
)
    // Note: Couldn't figure out how to make this work:
    // -> impl Iterator<Item = TopFormResult<'input, 'alloc, 'funs, 'bind,
    //                                       Extra, CombinerError>>
    -> ParseTextWith<'t, 'p, 'input, 'alloc, 'funs, 'bind, Extra, CombinerError,
                     impl FnMut(&Result<DatumType<'input, 'alloc, Extra>,
                                        Error<StrPos<'input>, CombinerError>>)
                                -> bool>
where
    Extra: Eq,
{
    let input_source_stream = input.iter();
    let mut already_errored = false;
    parser.parse(input_source_stream)
          .take_while(move |r|
                      if already_errored {
                          false
                      } else {
                          if r.is_err() {
                              already_errored = true;
                          }
                          true
                      })
}

type ParseTextWith<'text, 'parser, 'input, 'alloc, 'funs, 'bind,
                   Extra, CombinerError,
                   Predicate>
    = TakeWhile<ParseIter<'parser,
                          Parser<'input, 'alloc, 'funs, 'bind,
                                 Extra, CombinerError>,
                          text::iter::Iter<'text, Text<'input, 'alloc, Extra>>>,
                Predicate>;


#[cfg(test)]
mod tests {
    #![allow(clippy::many_single_char_names)]
    use super::*;
    use crate::{Datum, datum::premade::DatumMutRef,
                parser::DatumAllocator as _, parser::OperatorBindings as _};

    #[test]
    fn datum_allocator() {
        let a: DatumSlice<'_, '_> = &mut [Datum::Extra(())];
        let mut da = DatumAllocator::new(a);
        let r = da.new_datum(Datum::EmptyNest);
        assert_eq!(*r.unwrap(),
                   Datum::<Text<'_, '_>, (), DatumMutRef<'_, _, _>>::EmptyNest);
    }

    #[test]
    fn operator_bindings() {
        let f: OperativeRef<'_, '_, '_> = &(|_, _, _| Ok(None));
        let p: BindingsSlice<'_, '_, '_, '_>
            = &[(Datum::EmptyNest, Combiner::Operative(f))];
        let ob = OperatorBindings::new(p);
        let l = ob.lookup(&Datum::EmptyNest);
        assert_eq!(l.map(|_| true), Some(true));
        assert_eq!(ob.lookup(&Datum::EmptyList).map(|_| true), None);
    }

    #[test]
    fn parser_no_bind() {
        let a: DatumSlice<'_, '_>
            = &mut [Datum::Extra(()), Datum::Extra(()), Datum::Extra(()),
                    Datum::Extra(()), Datum::Extra(()), Datum::Extra(())];
        let mut p = super::parser_no_bind(a);

        assert_eq!(super::parse_text_with(&Text::from_str(""), &mut p)
                       .next(),
                   None);

        let mut ptw = |s| {
            let t = Text::from_str(s);
            let mut it = super::parse_text_with(&t, &mut p);
            let r = it.next().unwrap();
            assert!(it.next().is_none());
            r
        };

        assert_eq!(ptw("a"), Ok(Datum::Text(Text::from_str("a"))));
        assert_eq!(ptw("{}"), Ok(Datum::EmptyNest));

        let mut dtb = Datum::Text(Text::from_str("b"));
        let mut dtc = Datum::Text(Text::from_str("c"));
        let mut del1 = Datum::EmptyList;
        let mut del2 = Datum::EmptyList;
        let mut dco = Datum::Combination {
            operator: DatumMutRef(&mut dtc),
            operands: DatumMutRef(&mut del1)
        };
        let mut dl = Datum::List {
            elem: DatumMutRef(&mut dco),
            next: DatumMutRef(&mut del2),
        };
        assert_eq!(ptw("{b {c}}"),
                   Ok(Datum::Combination {
                          operator: DatumMutRef(&mut dtb),
                          operands: DatumMutRef(&mut dl),
                   }));

        assert_eq!(ptw("λ} h"),
                   Err(Error::UnbalancedEndChar(
                           StrPos{src: "λ} h", byte_pos: 2, char_pos: 1})));
    }

    #[test]
    fn parse_text_with() {
        let a: DatumSlice<'_, '_, i8> = &mut [Datum::Extra(0), Datum::Extra(0)];
        let b: BindingsSlice<'_, '_, '_, '_, i8, bool> = &[
            (Datum::Text(Text::from_str("op")),
             Combiner::Operative(&(|_, _, _| Ok(Some(Datum::Extra(1)))))),
            (Datum::Text(Text::from_str("e")),
             Combiner::Applicative(&(|_, _, _| Err(Error::FailedCombiner(true))))),
        ];
        let mut p = parser(a, b);

        assert_eq!(super::parse_text_with(&Text::from_str(""), &mut p)
                       .next(),
                   None);

        let mut ptw = |s| {
            let t = Text::from_str(s);
            let mut it = super::parse_text_with(&t, &mut p);
            let r = it.next().unwrap();
            assert!(it.next().is_none());
            r
        };

        assert_eq!(ptw("a"), Ok(Datum::Text(Text::from_str("a"))));
        assert_eq!(ptw("{}"), Ok(Datum::EmptyNest));
        assert_eq!(ptw("{op ignored}"), Ok(Datum::Extra(1)));
        let mut d1 = Datum::Text(Text::from_str("zz"));
        let mut d2 = Datum::EmptyList;
        assert_eq!(ptw("{zz }"),
                   Ok(Datum::Combination {
                          operator: DatumMutRef(&mut d1),
                          operands: DatumMutRef(&mut d2),
                      }));
        assert_eq!(ptw("{e}aborted"), Err(Error::FailedCombiner(true)));
    }
}
