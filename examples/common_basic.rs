use std::{time::SystemTime, str::FromStr, iter::FromIterator};

use kul::{
    common::inmem::{
        parse_str, parse_str_with,
        Text, OperatorBindings, DatumAllocator,
    },
    Combiner, Datum, datum::{BoxDatum, DatumBox}, Error, Text as _,
};

/// Parse without any bound operators and print results.  This shows that the
/// common base syntax can always be parsed without knowing about possible
/// extensions.
fn no_extension() {
    dbg!(parse_str("λ"));
    dbg!(parse_str(r"es\{c\}ap\\es"));
    dbg!(parse_str("{}"));
    dbg!(parse_str("{▷}"));
    dbg!(parse_str("Surrounding {{▷} λ {}} text."));
}

/// Parse with some bound operators and print results.  This shows that the
/// syntax and semantics of particular forms can be extended in custom ways.
fn with_extensions()
{
    /// Extends the types that may occur in the returned ASTs.
    #[derive(Hash, Eq, PartialEq, Debug)]
    enum MyDatumVariants {
        Time(SystemTime),
        Integer(i128),
    }

    /// Extends the types that may occur in errors returned by our custom form
    /// processing.
    #[derive(Debug)]
    enum MyCombinerError<'input> {
        Oops,
        Darnit(MyDatum<'input>),
    }

    // Convenient type aliases.

    type MyDatum<'input> = BoxDatum<Text<'input>, MyDatumVariants>;

    type MyOperatorBindings<'input> =
        OperatorBindings<'input, MyDatumVariants, MyCombinerError<'input>>;

    type MyDatumAllocator<'input> = DatumAllocator<'input, MyDatumVariants>;

    type AllocArg<'a> = &'a mut MyDatumAllocator<'static>;

    // The functions that process our custom forms.  Using closures can be nicer
    // because at least some type inference of the argument and return types can
    // be gained.  The operator and allocator arguments are always ignored for
    // this example, as they often are in real programs.

    let comment = |_operator, _operands, _: AllocArg<'_>| {
        Ok(None)
    };

    let pass_thru = |_operator, operands, _: AllocArg<'_>| {
        Ok(Some(operands))
    };

    let current_time = |_operator, operands, _: AllocArg<'_>| {
        if let Datum::EmptyList = operands {
            Ok(Some(Datum::Extra(MyDatumVariants::Time(SystemTime::now()))))
        } else {
            Err(Error::FailedCombiner(MyCombinerError::Darnit(operands)))
        }
    };

    let int = |_operator, operands: Text<'_>, _: AllocArg<'_>| {
        // Must convert the operands text into a `&str`, to be able to use other
        // parsing functions/libraries that take string slices.  (When the other
        // parsing functionality can instead take `Iterator`s of `char`s, this
        // conversion is unneeded.)
        let i = i128::from_str(&String::from_iter(operands.chars()))
                    .map_err(|_| Error::FailedCombiner(MyCombinerError::Oops))?;
        Ok(Some(Datum::Extra(MyDatumVariants::Integer(i))))
    };

    // Establish bindings of particular operator sub-forms to our processing
    // functions.  Other more declarative and concise ways of doing this are
    // possible, but, for this example, this shows the basic nature that other
    // ways could build on.

    let mut bindings = MyOperatorBindings::default();
    bindings.hashmap.insert(Datum::Text(Text::from_str("#")),
                            Combiner::Operative(Box::new(comment)));
    bindings.hashmap.insert(Datum::Text(Text::from_str("current-time")),
                            Combiner::Applicative(Box::new(current_time)));
    bindings.hashmap.insert(Datum::Text(Text::from_str("int")),
                            Combiner::Operative(Box::new(int)));
    let compound_operator_form =
        Datum::Combination {
            operator: DatumBox::new(Datum::Text(Text::from_str("compound"))),
            operands: DatumBox::new(Datum::EmptyList),
        };
    bindings.hashmap.insert(compound_operator_form,
                            Combiner::Applicative(Box::new(pass_thru)));

    // Parse a string that uses all of the above and print results.

    dbg!(parse_str_with(
        "{{compound} {current-time} {# removed} {unbound form} {int -42}}",
        bindings)
    );
}

fn main() {
    no_extension();
    with_extensions();
}
