//! The core of a parser for a unique textual notation that can be used as both
//! a data format and/or a markup language and that allows powerful
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

// TODO?: Should the Kernel terms be dropped in favor of terms like "text
// macro", "form macro", and "constructor" instead?  Those terms are probably
// more familiar for a language focused on textual syntax, vs. the Kernel terms
// which require an analogy between expression-form evaluation and
// extensible-parsing to really understand.  I like the Kernel terms and
// analogy because parsing can be viewed as evaluation of text and the AST as
// the denoted values, which seems fitting for this crate which kind of blends
// both into a hybrid and where there are two complementary ways of processing
// forms like in Kernel.

// TODO: Impl `Text` (and so a SourceStream too) for:
// - &[char] (which will also work for Vec<char> outside this crate with `std`)

// TODO: Exercise combiners in some test suite

// FUTURE: When/if the `generic_associated_types` feature of Rust becomes
// stable, use it so all the text chunk and char iterators can be generic and
// defined by the implementors and have the needed access to the lifetimes of
// the method calls' borrows of their `self`, instead of the current design that
// has the concrete iterator types and the odd state borrowing and transforming
// (which was a workaround done to have access to the needed lifetimes).


#![no_std]

#![forbid(unsafe_code)]
#![warn(clippy::all)]


use core::ops::DerefMut;

use parser::{CharClassifier, DatumAllocator, AllocError, OperatorBindings};


mod error;
#[doc(inline)]
pub use error::Error;

pub mod datum;
#[doc(inline)]
pub use datum::{Datum, DerefTryMut};

pub mod text;
#[doc(inline)]
pub use text::{Text, TextBase, TextConcat, TextChunk};

pub mod combiner;
#[doc(inline)]
pub use combiner::Combiner;

pub mod parser;


/// Implementations provided for ready use.
// In the future, this might have more and need to be public.
mod premade {
    /// Useful when omitting the positional information is desired/required.
    impl super::SourcePosition for () {
        #[inline] fn empty() -> Self { }
    }
}


/// Positional information of a character or text chunk relative to the original
/// source it is from.
// TODO: Should it be bound by: Display?, Debug?
pub trait SourcePosition
    where Self: Clone,
{
    fn empty() -> Self;
}

/// Item produced by a [`SourceStream`](trait.SourceStream.html) iterator that
/// represents its next character, possibly with positional information.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct SourceIterItem<SourcePosition> {
    /// A character produced by a source.
    pub ch: char,
    /// Positional information of the character relative to the original source
    /// it is from.
    pub pos: SourcePosition,
}

/// A stream of characters that might know its characters' positions in the
/// source it is from.
///
/// It may be used with streaming sources that might consume or destroy the
/// source and so can be iterated only once.  Or it may be used with sources
/// that can be iterated more than once by each time constructing new iterators
/// that implement this trait.
///
/// It is able to accumulate its iterated items, when its `next_accum` method is
/// called, until its `accum_done` method is called, and this may be done
/// multiple times.  The supertrait `next` method will be called instead when
/// the next item must not be accumulated, as determined by first using the
/// `peek` method to check, which is used to exclude escape characters from the
/// results of this crate's parsing.
///
/// After the `next_accum` method has been called and returned some item, the
/// `next` method should not be called before the `accum_done` method is called,
/// to avoid interfering with a pending accumulation.  If `next` is called in
/// this case, the pending accumulation will be silently dropped.
pub trait SourceStream<TT, DA>: Iterator<Item = SourceIterItem<TT::Pos>>
    where TT: TextConcat<DA>,
          DA: DatumAllocator<TT = TT> + ?Sized,
{
    /// TODO
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item>;

    /// TODO
    fn next_accum(&mut self, dalloc: &mut DA)
                  -> Result<Option<<Self as Iterator>::Item>,
                            AllocError>;

    /// TODO ... this is the primary constructor of the text types for the
    /// parsing
    fn accum_done(&mut self, dalloc: &mut DA) -> Result<TT, AllocError>;
}


/// Represents: the ability to parse a string; the characters used to delimit
/// the nesting form; the method of allocating the `Datum`s; and the environment
/// of bindings of macros.
#[derive(Debug)]
pub struct Parser<CC, DA, OB> {
    pub classifier: CC,
    pub allocator: DA,
    pub bindings: OB,
}

impl<CC, DA, OB> Parser<CC, DA, OB>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
{
    /// The primary method.  Parse the given text source, according to the
    /// specific parameterization of our `Self`, and return an iterator that
    /// yields each top-level form as a `Datum` AST.
    pub fn parse<S>(&mut self, source: S) -> ParseIter<'_, Self, S>
        where S: SourceStream<DA::TT, DA>,
    {
        ParseIter::new(self, source)
    }
}


/// An [`Iterator`](http://doc.rust-lang.org/std/iter/trait.Iterator.html) that
/// parses its input text one top-level form at a time per each call to
/// [`next`](http://doc.rust-lang.org/std/iter/trait.Iterator.html#tymethod.next),
/// and yields a [`Datum`](enum.Datum.html) AST for each or an
/// [`Error`](enum.Error.html), according to the given
/// [`Parser`](struct.Parser.html)'s parameterization.
#[derive(Debug)]
pub struct ParseIter<'p, Prsr, SrcStrm>
    where Prsr: ?Sized + 'p,
{
    parser: &'p mut Prsr,
    src_strm: SrcStrm,
    nest_depth: usize,
}

impl<'p, CC, DA, OB, S>
    Iterator
    for ParseIter<'p, Parser<CC, DA, OB>, S>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
          Parser<CC, DA, OB>: 'p,
          S: SourceStream<DA::TT, DA>,
{
    type Item = ParseIterItem<DA, OB>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.do_next() {
            Ok(Some(d)) => Some(Ok(d)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}

/// The type of values given by the parser iterator
pub type ParseIterItem<DA, OB> = ParseResult<DA, OB>;

type ParseDatum<DA> = Datum<<DA as DatumAllocator>::TT,
                            <DA as DatumAllocator>::ET,
                            <DA as DatumAllocator>::DR>;

type ParseError<DA, OB> = Error<<<DA as DatumAllocator>::TT as TextBase>::Pos,
                                <OB as OperatorBindings<DA>>::CE>;

type ParseResult<DA, OB> = Result<ParseDatum<DA>, ParseError<DA, OB>>;

type ParseResultOption<DA, OB> = Result<Option<ParseDatum<DA>>, ParseError<DA, OB>>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum ParseTextMode {
    Base,
    Operator,
    Operands,
}

impl<'p, CC, DA, OB, S>
    ParseIter<'p, Parser<CC, DA, OB>, S>
    where CC: CharClassifier,
          DA: DatumAllocator,
          OB: OperatorBindings<DA>,
          Parser<CC, DA, OB>: 'p,
          S: SourceStream<DA::TT, DA>,
{
    #[inline]
    fn new(parser: &'p mut Parser<CC, DA, OB>, src_strm: S) -> Self {
        Self {
            parser,
            src_strm,
            nest_depth: 0,
        }
    }

    #[inline]
    fn do_next(&mut self) -> ParseResultOption<DA, OB> {
        self.parse_next(ParseTextMode::Base)
    }

    fn parse_next(&mut self, mode: ParseTextMode) -> ParseResultOption<DA, OB> {
        loop {
            if mode == ParseTextMode::Operator {
                // Skip any leading whitespace before head form.
                self.skip_whitespace();
            }
            // Peek some next char for below, or abort appropriately if none.
            let ch = match self.src_strm.peek() {
                Some(&SourceIterItem{ch, ..}) => ch,
                None =>
                    return if self.nest_depth == 0 {
                        Ok(None)
                    } else {
                        Err(Error::MissingEndChar)
                    }
            };
            // Start of a nest, either a combination or an empty nest. Parse it
            // to its end and return it if its combiner didn't remove it.
            if self.parser.classifier.is_nest_start(ch) {
                self.incr_nest_depth();
                let result = self.parse_nested();
                self.decr_nest_depth();
                // If a combiner indicated to remove the nest form, continue our
                // loop to parse the next form, effectively removing the current
                // nest form.  Else, return the result whatever it is.
                if let Ok(None) = &result {
                    continue
                } else {
                    return result
                }
            }
            // End of a nest, or error. Don't parse nor return an item, only
            // check validity.
            else if self.parser.classifier.is_nest_end(ch) {
                return self.check_end_char().map(|_| None)
            }
            // Start of a text. Parse it to its end and return it.
            else {
                return self.parse_text(mode).map(Some)
            }
       }
     }

    fn parse_text(&mut self, mode: ParseTextMode) -> ParseResult<DA, OB>
    {
        #[inline]
        fn is_end_char<CC>(ch: char, chclass: &CC, mode: ParseTextMode) -> bool
            where CC: CharClassifier,
        {
            match mode {
                ParseTextMode::Base
                    => chclass.is_nest_start(ch),
                ParseTextMode::Operator
                    => chclass.is_whitespace(ch)
                    || chclass.is_nest_start(ch)
                    || chclass.is_nest_end(ch),
                ParseTextMode::Operands
                    => chclass.is_nest_end(ch),
            }
        }

        // Giving our allocator to the accumulation calls below enables them to
        // have the option of using new `Datum`s to achieve text-chunking for
        // the breaking around, and excluding of, escape characters.  While most
        // implementations will ignore the allocator (e.g. to instead use heap
        // allocation), this unusual support is essential for `TextDatumList`
        // (or similar) which is intended for use in constrained environments
        // without heap allocation where reusing our `Datum` allocation ability
        // (e.g. from a stack array) is desired.

        let mut text = DA::TT::empty();
        macro_rules! concat_accum {
            () => {
                let accum = self.src_strm.accum_done(&mut self.parser.allocator)?;
                text = text.concat(accum, &mut self.parser.allocator)?;
            }
        }

        let mut nest_level: usize = 0;

        while let Some(&SourceIterItem{ch, ..}) = self.src_strm.peek() {
            // Reached end. Do not consume peeked end char
            if nest_level == 0 && is_end_char(ch, &self.parser.classifier, mode) {
                break;
            }
            // Accumulate escaped char whatever it might be, but not the escape
            // char
            else if self.parser.classifier.is_nest_escape(ch) {
                concat_accum!(); // Break chunk before escape char
                self.src_strm.next(); // Skip peeked escape char first
                self.src_strm.next_accum(&mut self.parser.allocator)?;
            }
            // Start of nest. Track nesting depth
            else if self.parser.classifier.is_nest_start(ch) {
                // Accumulate peeked
                self.src_strm.next_accum(&mut self.parser.allocator)?;
                nest_level += 1;
            }
            // End of nest. Check balanced nesting
            else if self.parser.classifier.is_nest_end(ch) {
                if nest_level > 0 {
                    // Accumulate peeked
                    self.src_strm.next_accum(&mut self.parser.allocator)?;
                    nest_level -= 1;
                } else {
                    self.check_end_char()?;
                    break;
                }
            }
            // Accumulate peeked
            else {
                self.src_strm.next_accum(&mut self.parser.allocator)?;
            }
        }
        // Done. Return what we accumulated. Or error if unbalanced nesting.
        if nest_level == 0 {
            concat_accum!();
            Ok(Datum::Text(text))
        } else {
            Err(Error::MissingEndChar)
        }
    }

    fn parse_nested(&mut self) -> ParseResultOption<DA, OB>
    {
        let end = |slf: &mut Self| {
            // Consume our nest's end char. A missing end char is possible, but
            // an erroneous non-end char shouldn't be.
            if let Some(SourceIterItem{ch, ..}) = slf.src_strm.next() {
                debug_assert!(slf.parser.classifier.is_nest_end(ch));
                Ok(())
            } else {
                Err(Error::MissingEndChar)
            }
        };

        // Advance past nest start char.
        let start = self.src_strm.next();
        debug_assert_eq!(start.map(|SourceIterItem{ch, ..}|
                                   self.parser.classifier.is_nest_start(ch)),
                         Some(true));
        // Parse form in operator position, or empty.
        let operator = self.parse_next(ParseTextMode::Operator)?;
        // If operator delimited by following whitespace, advance past first
        // whitespace char.
        if let Some(&SourceIterItem{ch, ..}) = self.src_strm.peek() {
            if self.parser.classifier.is_whitespace(ch) { self.src_strm.next(); }
        }
        // Determine the result.
        let result = match operator {
            // Parse the operands according to the operator.
            Some(operator) => match self.parser.bindings.lookup(&operator) {
                // Operator is bound to a combiner macro which will process the
                // operands and determine the return value.
                Some(combiner) => match combiner {
                    // Operatives are given the operands text unparsed to do
                    // whatever they want with it.
                    Combiner::Operative(mut opr) => {
                        let operands = self.parse_text(ParseTextMode::Operands)?;
                        end(self)?;
                        opr.deref_mut()(operator, operands, &mut self.parser.allocator)?
                    },
                    // Applicatives are given the recursive parse of the
                    // operands text as a list of "arguments".
                    Combiner::Applicative(mut apl) => {
                        let arguments = self.parse_all(ParseTextMode::Base)?;
                        end(self)?;
                        apl.deref_mut()(operator, arguments, &mut self.parser.allocator)?
                    }
                },
                // Not bound, so simply recursively parse operands and return a
                // value representing the "combination" of operator and operands
                // forms.
                None => {
                    let operands = self.parse_all(ParseTextMode::Base)?;
                    end(self)?;
                    Some(Datum::Combination {
                        operator: self.parser.allocator.new_datum(operator)?,
                        operands: self.parser.allocator.new_datum(operands)?,
                    })
                },
            }
            // No operator nor operands. Empty nest form.
            None => {
                end(self)?;
                Some(Datum::EmptyNest)
            }
        };
        // Return result
        Ok(result)
    }

    fn parse_all(&mut self, mode: ParseTextMode) -> ParseResult<DA, OB> {
        let mut head = Datum::EmptyList;
        let mut tail = &mut head;
        loop {
            let it = self.parse_next(mode)?;
            if let Some(next_it) = it {
                *tail = Datum::List {
                    elem: self.parser.allocator.new_datum(next_it)?,
                    next: self.parser.allocator.new_datum(Datum::EmptyList)?,
                };
                if let Datum::List{ref mut next, ..} = tail {
                    if let Some(next) = DerefTryMut::get_mut(next) {
                        tail = next;
                    } else {
                        return Err(Error::FailedDerefTryMut);
                    }
                } else {
                    unreachable!()
                }
            } else {
                break;
            }
        }
        Ok(head)
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        let chclass = &self.parser.classifier;
        while self.src_strm.peek()
                           .map(|&SourceIterItem{ch, ..}| chclass.is_whitespace(ch))
                           .unwrap_or(false)
        {
            self.src_strm.next(); // Skip peeked whitespace char
        }
    }

    #[inline]
    fn check_end_char(&mut self) -> Result<(), ParseError<DA, OB>> {
        {
            let chclass = &self.parser.classifier;
            debug_assert_eq!(self.src_strm.peek().map(|&SourceIterItem{ch, ..}|
                                                      chclass.is_nest_end(ch)),
                             Some(true));
        }
        if self.nest_depth > 0 {
            // Valid end of nest. Do not consume peeked char.
            Ok(())
        } else {
            // Invalid unbalanced nest end character. Consume peeked char, to
            // allow the possibility that this iterator could be resumed
            // again. Also, use its `pos` in the error. This `unwrap` will never
            // fail because we already did `peek` and know there is a next.
            let n = self.src_strm.next().unwrap();
            Err(Error::UnbalancedEndChar(n.pos))
        }
    }

    #[inline]
    fn incr_nest_depth(&mut self) {
        self.nest_depth += 1;
    }

    #[inline]
    fn decr_nest_depth(&mut self) {
        self.nest_depth -= 1;
    }
}
