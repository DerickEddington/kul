# Kul

A unique textual notation that can be used as both a data format and a markup
language and that has powerful extensibility of both syntax and semantics, and a
Rust library for parsing it.  It is inspired by the little-known [Curl
language].

The syntax is very minimal and exists only to delimit forms of nested text that
you sometimes define your own parsing and semantics of by binding *operator*
sub-forms to *combiner* functions which are similar to macros.  As such, by
itself, it is not a complete format for most applications and they will define
their own different extensions.  However, it enables having a common base syntax
that can still be parsed, without any extensions, into the basic structure which
can be used across applications that do not need to fully handle the inner
syntax of all of the nested text forms.  Furthermore, shared or standardized
extensions could be created in the future.

Some of what the library offers:

- Zero-copy parsing of both entirely-in-memory inputs and of streamed inputs (if
you arrange buffering into chunks appropriately), including zero-copy exclusion
of the escape character.  All achieved via a generic design of chunked text.
- No `unsafe` code.
- No external dependencies.
- A `no_std` crate usable on its own (including with only stack allocation).
- A `std`-using crate with more convenient `Vec`s, `String`s, `Box`es, etc.
- Very generically parameterized in most aspects to allow maximal reuse for
  diverse applications.

This is also my first Rust project and an excuse to learn and explore both it
and the idea I've had for a while for a format like this.

[Curl language]: http://people.csail.mit.edu/ward/curl-ijwet.pdf

## Examples

```
As markup of free-form text for embedding structured data types, such as a
{date 2019-03-05 15:15 PST}, which might be rendered as widgets; or for {italic
{bold styling} the text}.  Only the \{, \}, and \\ characters require escaping.
```

```
{config {# As a data structure. (This form is a comment.)}
    {logging
        {err = yes}
        {warn = {maybe, if 1 + x = 3}}
    }
    {greeting: We can have text in structures without ugly quoting!}
    {knobs this := {that}; other := {}}
    {{compound-operator with arguments} 7/2 > 𝜋}
}
```

```
Using non-default delimiters:

⟪⟪source-code Rust⟫
    use kul::common::inmem::parse_str;

    fn main() {
        let input = "Escaped the {bold non-default} delimiters: ␛⟪, ␛⟫, ␛␛";
        dbg!(parse_str(input));
    }
⟫
```

## Status

Version `0.1.0`: experimental and unstable.  Builds fine and passes all tests
and lints.

## Rust version

At least `1.33` required.

## Usage

It would require some length to describe the full range of possible usages,
given how very generically parameterized the library is.

But for common basic applications, it can be used like:

```rust
use std::{time::SystemTime, str::FromStr, iter::FromIterator};

use kul::{
    common::inmem::{
        parse_str, parse_str_with,
        Text, OperatorBindings, DatumAllocator,
    },
    Combiner, Datum, datum::{BoxDatum, DatumBox}, Error, Text as _,
};

/// Parse without any bound operators (and print results).  This shows that the
/// common base syntax can always be parsed without knowing about possible
/// extensions.
fn no_extension() {
    dbg!(parse_str("λ"));
    dbg!(parse_str(r"es\{c\}ap\\es"));
    dbg!(parse_str("{}"));
    dbg!(parse_str("{▷}"));
    dbg!(parse_str("Surrounding {{▷} λ {}} text."));
}

/// Parse with some bound operators (and print results).  This shows that the
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

    type AllocArg<'alloc> = &'alloc mut MyDatumAllocator<'static>;

    // The functions that process our custom forms.  Using closures can be nicer
    // because at least some type inference of the argument and return types can
    // be gained.  The operator and allocator arguments are always ignored for
    // this example, as they often are in real programs.

    let comment = |_operator, _operands, _: AllocArg<'_>| {
        Ok(None)
    };

    let current_time = |_operator, operands, _: AllocArg<'_>| {
        if let Datum::EmptyList = operands {
            Ok(Some(Datum::Extra(MyDatumVariants::Time(SystemTime::now()))))
        } else {
            Err(Error::FailedCombiner(MyCombinerError::Darnit(operands)))
        }
    };

    let int = |_operator, operands: Text<'_>, _: AllocArg<'_>| {
        let i = i128::from_str(&String::from_iter(operands.chars()))
                    .map_err(|_| Error::FailedCombiner(MyCombinerError::Oops))?;
        Ok(Some(Datum::Extra(MyDatumVariants::Integer(i))))
    };

    let pass_thru = |_operator, operands, _: AllocArg<'_>| {
        Ok(Some(operands))
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

    // Parse a string that uses all of the above (and print results).

    dbg!(parse_str_with(
        "{{compound} {current-time} {# removed} {unbound form} {int -42}}",
        bindings)
    );
}

fn main() {
    no_extension();
    with_extensions();
}
```

The above example can be run by doing:

```
cargo run --example common_basic
```

## Documentation

The source-code has many doc comments, which can be rendered as the API
documentation in your WWW browser by doing:

```
cargo doc --open
```

See the documentation at the top of the `kul_core` crate for some further
overview of the Kul language and design.

(`TODO`: Some of the less-important doc comments' inter-links are broken
currently due to refactorings changing their relative directories and names, and
some just need links to be made in the first place.  I'm hoping that the
[`intra_rustdoc_links` feature (RFC 1946)][intra_rustdoc_links] will become
stable soon to make fixing these be much easier and much more maintainable.)

[intra_rustdoc_links]: https://github.com/rust-lang/rfcs/blob/master/text/1946-intra-rustdoc-links.md

## Tests

The crates have many unit tests and integration tests, which can be run by doing:

```
cargo test --all
```

That uses around 2.1G of memory running them with `--test-threads=4` on my
64-bit 4-core computer.  If you want it to use less, you can do:

```
cargo test --all -- "" tree-size=$((2**N))  # where N < 21
```

2<sup>21</sup> is the default for the size of the trees used to test the `Drop`
and `PartialEq` implementations that prevent stack overflows for deep trees
(e.g. long lists and deep nests) of `kul::Datum`.  Below some size, it will no
longer be properly testing this because overflows would not happen without our
implementations anyway.  Alternatively, you may increase `N > 21` as much as you
can, to test very-deep trees.  To see overflows happen, comment-out the `Drop`
`impl`s in `src/drop.rs`.

## Unresolved

The following aspects are unresolved:

- Some of the terminology chosen might be improvable.

- The `Datum` enum type, used for the returned AST nodes and for the arguments
  given to extension functions, is designed to be multi-purpose so constrained
  applications, that do not use heap allocation and use only the core `no_std`
  crate, only need to provide an allocator of this one simple type, which
  enables just using a basic array of them on the stack.  But this results in
  some of the API being non-ideal because `Datum` is used where only a subset of
  its variants are possible, requiring matching to destructure with a branch
  that has to be made `unreachable!()`.  I couldn't figure out a way to improve
  this while keeping support for constrained allocation from basic arrays of a
  single type.  It is unresolved whether this can be improved without causing
  difficulties for no-heap constrained applications.

- The `Text` trait, used to represent logical concatenation of chunks of
  sequences of `char`s, is designed to be multi-purpose so it can be used for
  AST output, and for input of streamed or in-memory sources, and for
  efficiently excluding escape characters, and used generically with various
  underlying representations.  But this results in having to use iteration of
  its logical `char` sequence for all operations that want to know its content,
  which doesn't fit perfectly with applications that want to parse its content
  by using other libraries that take string slices (`&str`) as their inputs to
  parse (e.g. the `std` library's `FromStr` implementations).  Creating
  temporary `String`s from `Text` content is a reasonable way to use such
  libraries, but this doesn't feel great given the project's goal of supporting
  zero-copy as much as possible, and `String` (nor any dynamic string types) is
  not available to no-heap constrained applications.  It is unresolved whether
  this can be improved while keeping all the desired uses and qualities of the
  `Text` trait.  (The Kul library itself preserves zero-copy while using
  iterators of `char`s by using its own special iterator and related traits and
  types.)

- The name Kul was chosen after abandoning the name Kruvi.  Kruvi is the Lojban
  word for curve, in homage to Curl, but it sounds too similar to the name
  Groovy (an existing software project) and is too long to use as a filename
  extension.  The name Kul seems to be unclaimed as a project name and `.kul`
  unclaimed as a filename extension.  I'm imagining that applications might use
  filename extensions like `.whatever.kul` to indicate their particular
  extensions of the format as well as indicate that it can also always be parsed
  as the basic `.kul` structure.  I'm not very attached to any name and am open
  to changing it but do like that Kul is cool.
