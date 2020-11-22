//! The core of a parser for a unique textual notation that can be used as both
//! a data format and a markup language and that has powerful
//! extensibility of both syntax and semantics.  It is inspired by the
//! little-known [Curl programming
//! language](http://en.wikipedia.org/wiki/Curl_(programming_language)).  It is
//! very parameterized to allow maximal reuse for different applications.  It is
//! capable of zero-copy operation (depending on how you concretize it),
//! including for its generic designs of chunked text representations and
//! omitting escape characters.
//!
//! # Overview
//!
//! The notation is similar to Lisp S-expressions in that there are nested forms
//! delimited by brackets and in that the first sub-form in a nest (the "head")
//! can be interpreted as an operator (which can also be thought of as a
//! constructor).  Unlike S-expressions, but like Curl, the parsing and meaning
//! of nested text and nested forms can be extended by two types of macros
//! (somewhat like "reader macros" of Lisp).  Also unlike S-expressions, all
//! text outside nested forms is preserved exactly, as is the text inside some
//! nested forms, and so the notation is also a markup language.  Head forms can
//! be bound to macros, which is what causes them to be interpreted as
//! operators, but they can also be unbound which leaves a nested form
//! uninterpreted.
//!
//! The macros are implemented as functions, termed "combiner"s.  One of the
//! types of combiner, termed "operative", takes nested text unparsed and can
//! parse it however it wants.  The other type of combiner, termed
//! "applicative", takes a list of forms produced by recursively parsing nested
//! text.  For both combiner types, whatever is returned is substituted for the
//! nested form in the abstract syntax tree (AST) returned by the parser.  (The
//! terms "combiner", "operative", and "applicative" come from the [Kernel
//! programming language](http://web.cs.wpi.edu/~jshutt/kernel.html) and its
//! F-expressions, which are somewhat analogous.)
//!
//! The parser is intended to be extended, by binding combiners, for each
//! application, but it can be used without extension, i.e. without any macros,
//! as a simplistic kind of S-expression language where the basic AST is used as
//! your data structure.
//!
//! This core crate is `no_std` and so can be used in constrained environments
//! without heap allocation.  The crate is generically parameterized over what
//! allocates the "datums" used as nodes in the constructed ASTs.  Allocation
//! can be done from fixed-size, pre-established, stack arrays.  Or, allocation
//! can be done from a heap, e.g. using the standard `Box` type, or from
//! whatever kind of allocator you can arrange.
//!
//! This core crate's purpose mostly is to define the generic types, traits, and
//! logic that other crates depend on to create their own concrete
//! implementations to use for their actual parsing.  But some basic premade
//! implementations, that fit with `no_std` use, are provided by this core crate
//! in sub-modules named `premade`, and these might be sufficient by themselves
//! for some limited applications.
//!
//! # Unicode
//!
//! Parsing is done based on Rust's `char` type (which is a Unicode scalar
//! value).  The configurable delimiters are single `char`s, and so they cannot
//! be general grapheme clusters (because those can be sequences of multiple
//! `char`s).  It seems very unlikely that anyone would seriously want to use
//! grapheme clusters as the delimiters because the few delimiters only have
//! bracket and escape semantics.  For a parsed input text, all non-delimiter
//! `char`s are preserved exactly (except whitespace around head forms), and so
//! grapheme clusters are always preserved where it makes sense for our format.


#![no_std]


kul_lints::declare_lints_on_top_module!();
