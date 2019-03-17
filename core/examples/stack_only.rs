//! This is modified from the `common_basic.rs` example (which uses the full
//! `kul` crate), to contrast how similar usage is achieved when using only the
//! `kul_core` crate with only values allocated on the stack.

use core::str::FromStr;

use kul_core::{
    premade::inmem::{
        parser, parser_no_bind, parse_text_with,
        DatumType, DatumSlice, Text, BindingsSlice, DatumAllocator,
    },
    datum::premade::DatumMutRef,
    Combiner, Datum, Error, Text as _,
};

/// Parse without any bound operators.  This shows that the common base syntax
/// can always be parsed without knowing about possible extensions.
fn no_extension() {
    let input = r"Surrounding {▷ e\\sc} text.";

    // Helper to initialize the elements of the below array.  Any `Datum`
    // variant would work but `EmptyNest` is the simplest.  The elements'
    // initial values are never actually used and are always overwritten when
    // the elements are used by the parsing.  Something like this closure
    // function, or some macro, is needed to help with making the new values for
    // each element, or the value constructor expression could just be repeated
    // directly, because the concrete `Datum` type cannot be `Copy` because it
    // contains mutable borrows.
    let e = || Datum::EmptyNest;

    // The `Datum`s that constitute the AST returned by parsing are allocated
    // from this array that is allocated on the stack.  Real programs usually
    // need larger arrays than this one.  (Alternatively, the slice could be
    // allocated anywhere else you could arrange; e.g. in some non-`std` heap or
    // in some prepared memory region.)
    let alloc_from: DatumSlice<'_, '_> = &mut [e(), e(), e(), e(), e()];

    // Make a `Parser` with no bindings that allocates from `alloc_from`.
    let mut parser = parser_no_bind(alloc_from);

    // The string slices to parse must be wrapped in the `Text` type (because
    // the `ParseIter` must borrow that).  This does not copy the string.
    let input = Text::from_str(input);

    // Make the iterator that will parse the input using the parser.
    let parse_iter = parse_text_with(&input, &mut parser);

    // Parse each top-level form. (Three in this case.)
    for result in parse_iter {
        // Print each AST so you can see it. (Omit when #![no_std])
        dbg!(&result);
        // Do what you want with the constructed ASTs or error.
        match result {
            Ok(datum) =>
                // An AST of a top-level form as a `Datum` enum.  Variants that
                // are branching nodes reference other `Datum`s via mutable
                // borrows of elements of the `alloc_from` array.
                match datum { _ => () },
            Err(error) =>
                // Either a syntax error or allocator exhaustion, in this case.
                match error { _ => () }
        }
    }
}

/// Parse with some bound operators.  This shows that the syntax and semantics
/// of particular forms can be extended in custom ways.
fn with_extensions()
{
    /// Extends the types that may occur in the returned ASTs.
    #[derive(Eq, PartialEq, Debug)]
    enum MyDatumVariants {
        Thing,
        Integer(i128),
    }

    /// Extends the types that may occur in errors returned by our custom form
    /// processing.
    #[derive(Debug)]
    enum MyCombinerError<'input, 'alloc> {
        Oops,
        Darnit(MyDatum<'input, 'alloc>),
        TooLong,
    }

    // Helpful type aliases.

    type MyText<'input, 'alloc> = Text<'input, 'alloc, MyDatumVariants>;

    type MyBindingsSlice<'input, 'alloc, 'funs, 'bind> =
        BindingsSlice<'input, 'alloc, 'funs, 'bind,
                      MyDatumVariants, MyCombinerError<'input, 'alloc>>;

    type MyDatum<'input, 'alloc> = DatumType<'input, 'alloc, MyDatumVariants>;

    type MyDatumAllocator<'input, 'alloc> =
        DatumAllocator<'input, 'alloc, MyDatumVariants>;

    type MyDatumSlice<'input, 'alloc> =
        DatumSlice<'input, 'alloc, MyDatumVariants>;

    type AllocArg<'a, 'alloc> = &'a mut MyDatumAllocator<'static, 'alloc>;

    // The functions that process our custom forms.  Using closures can be nicer
    // because at least some type inference of the argument and return types can
    // be gained.  The operator and allocator arguments are always ignored for
    // this example, as they often are in real programs.

    let comment = |_operator, _operands, _: AllocArg<'_, '_>| {
        Ok(None)
    };

    let pass_thru = |_operator, operands, _: AllocArg<'_, '_>| {
        Ok(Some(operands))
    };

    let thing = |_operator, operands, _: AllocArg<'_, '_>| {
        if let Datum::EmptyList = operands {
            Ok(Some(Datum::Extra(MyDatumVariants::Thing)))
        } else {
            Err(Error::FailedCombiner(MyCombinerError::Darnit(operands)))
        }
    };

    let int = |_operator, operands: MyText<'_, '_>, _: AllocArg<'_, '_>| {
        // Must convert the operands text into a `&str`, to be able to use other
        // parsing functions/libraries that take string slices.  Without `std`
        // heap types, we use a buffer on the stack.  (When the other parsing
        // functionality can instead take `Iterator`s of `char`s, this
        // conversion is unneeded.)
        let mut buf = [0; 39];
        let s = operands.encode_utf8(&mut buf).map_err(
            |_| Error::FailedCombiner(MyCombinerError::TooLong))?;
        let i = i128::from_str(s).map_err(
            |_| Error::FailedCombiner(MyCombinerError::Oops))?;
        Ok(Some(Datum::Extra(MyDatumVariants::Integer(i))))
    };

    // Establish bindings of particular operator sub-forms to our processing
    // functions.  Other more declarative and concise ways of doing this are
    // possible, but, for this example, this shows the basic nature that other
    // ways could build on.

    // We make one bound operator be a compound form.  To be the correct type
    // and work, its child nodes must be defined here as variables.
    let mut cof_rator = Datum::Text(Text::from_str("compound"));
    let mut cof_rands = Datum::EmptyList;
    let compound_operator_form =
        Datum::Combination {
            operator: DatumMutRef(&mut cof_rator),
            operands: DatumMutRef(&mut cof_rands),
        };

    // Bind the operators to the functions, using pairs (2-tuples) to associate
    // them.
    let bindings: MyBindingsSlice<'_, '_, '_, '_> = &[
        (Datum::Text(Text::from_str("#")),
         Combiner::Operative(&comment)),
        (Datum::Text(Text::from_str("thing")),
         Combiner::Applicative(&thing)),
        (Datum::Text(Text::from_str("int")),
         Combiner::Operative(&int)),
        (compound_operator_form,
         Combiner::Applicative(&pass_thru)),
    ];

    // Prepare the slice from which to allocate the `Datum`s.  See above
    // `no_extension` function for comments about this.
    let e = || Datum::EmptyNest;
    let alloc_from: MyDatumSlice<'_, '_> =
        &mut [e(), e(), e(), e(), e(), e(), e(), e(), e(), e(), e(), e(),
              e(), e(), e(), e(), e(), e()];

    // Make a `Parser` that uses our `bindings` and allocates from `alloc_from`.
    let mut parser = parser(alloc_from, bindings);

    let input = "{{compound} {thing} {# removed} {unbound form} {int -42}}";
    // Must be wrapped in `Text`.  Does not copy the string.  See above.
    let input = Text::from_str(input);

    // Make the iterator that will parse the input using the parser.
    let parse_iter = parse_text_with(&input, &mut parser);
    // Parse each top-level form.
    for result in parse_iter {
        // Print each AST so you can see it. (Omit when #![no_std])
        dbg!(&result);
        // Do what you want with the constructed ASTs or error.  See above
        // `no_extension` function for comments about this.
    }
}

fn main() {
    no_extension();
    with_extensions();
}
