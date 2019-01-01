//! The core of a parser for a unique textual notation that can be used as both
//! a data format and/or a markup language and that allows powerful
//! extensibility of both syntax and semantics.  It is inspired by the
//! little-known [Curl programming
//! language](http://en.wikipedia.org/wiki/Curl_(programming_language)).
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
//! can be done from a heap, e.g. using the standard `Box` type.
//!
//! TODO?: Should the Kernel terms be dropped in favor of terms like "text
//! macro", "form macro", and "constructor" instead?  Those terms are probably
//! more familiar for a language focused on textual syntax, vs. the Kernel terms
//! which require an analogy between expression-form evaluation and
//! extensible-parsing to really understand.  I like the Kernel terms and
//! analogy because parsing can be viewed as evaluation of text and the AST as
//! the denoted values, which seems fitting for this crate which kind of blends
//! both into a hybrid and where there are two complementary ways of processing
//! forms like in Kernel.

// TODO?: Use mod modules to organize better?

// TODO: Review what's pub and not

#![no_std]

use core::mem::replace;
use core::ops::{Deref, DerefMut};
use core::str::CharIndices;
use core::iter::{Peekable, Enumerate};

use self::Datum::*;
use self::Combiner::*;
use self::Error::*;


/// The possible errors that might be returned by parsing.
/// It is extensible by the `CombinerError` type parameter.
#[derive(Copy, Clone, Eq, Debug)]
pub enum Error<CombinerError> {
    /// Close-bracket without matching open-bracket
    UnbalancedEndChar {byte_pos: usize, char_pos: usize},
    /// End-of-stream reached inside nest form
    MissingEndChar,
    /// `Datum` allocator error
    FailedAlloc(AllocError),
    /// [`DerefTryMut::get_mut`](trait.DerefTryMut.html#tymethod.get_mut) failed
    FailedDerefTryMut,
    /// Extensibility that custom macros/combiners may utilize to add additional
    /// error variants
    FailedCombiner(CombinerError),
}

/// The possible errors that might be returned by a parser's `Datum` allocator.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum AllocError {
    /// No more free
    AllocExhausted,
}

impl<CE> From<AllocError> for Error<CE> {
    fn from(ae: AllocError) -> Self {
        Error::FailedAlloc(ae)
    }
}

/// This allows different concrete [`Error`](enum.Error.html) types to be
/// compared with each other for equality if their [combiner error
/// types](enum.Error.html#variant.FailedCombiner) can be.
impl<CE1, CE2> PartialEq<Error<CE2>> for Error<CE1>
    where CE1: PartialEq<CE2>,
{
    fn eq(&self, other: &Error<CE2>) -> bool {
        match (self, other) {
            (UnbalancedEndChar{byte_pos: bp1, char_pos: cp1},
             UnbalancedEndChar{byte_pos: bp2, char_pos: cp2})
                => *bp1 == *bp2 && *cp1 == *cp2,
            (MissingEndChar, MissingEndChar)
                => true,
            (FailedAlloc(ae1), FailedAlloc(ae2))
                => *ae1 == *ae2,
            (FailedDerefTryMut, FailedDerefTryMut)
                => true,
            (FailedCombiner(ce1), FailedCombiner(ce2))
                => *ce1 == *ce2,
            _
                => false
        }
    }
}


/// A string representation that knows what source string it is in and at what
/// position.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PosStr<'s> {
    /// The represented string
    pub val: &'s str,
    /// Entire source string that `val` is in. Might equal `val`.
    pub src: &'s str,
    /// Byte position of start of `val`, relative to `src`
    pub byte_pos: usize,
    /// Character position of start of `val`, relative to `src`
    pub char_pos: usize,
}

impl Deref for PosStr<'_> {
    type Target = str;
    fn deref(&self) -> &str {
        self.val
    }
}

// TODO?:
// impl Borrow for PosStr<'_>
// impl AsRef for PosStr<'_>


/// The abstract syntax tree (AST) type returned by parsing.  It is extensible
/// by the `ExtraType` parameter, and it is parameterized over the `DatumRef`
/// type used to refer to the other `Datum`s in an AST.  It can also be used for
/// DAGs.
#[derive(Copy, Clone, Eq, Debug)]
pub enum Datum<'s, ExtraType, DatumRef>
    where DatumRef: DerefTryMut<Target = Datum<'s, ExtraType, DatumRef>>,
{
    /// An unbroken span of text. (Only nest forms break text.)
    Text(PosStr<'s>),
    /// A nest form that is not empty and so has a non-empty "operator"/"head"
    /// sub-form, as a `Datum`, and has a possibly-empty "operands" sub-form(s),
    /// as a `List` (or `EmptyList`) or a `Text`.
    Combination{operator: DatumRef, operands: DatumRef},
    /// An empty nest form
    EmptyNest,
    /// A list of other `Datum`s. Used to represent parsed operands.
    List{elem: DatumRef, next: DatumRef},
    /// An empty list
    EmptyList,
    /// Extensibility that custom macros/combiners may utilize to add additional
    /// variants
    Extra(ExtraType),
}

/// This allows different concrete [`Datum`](enum.Datum.html) types to be
/// compared with each other for equality if their ["extra"
/// types](enum.Datum.html#variant.Extra) can be.  This also avoids stack
/// overflows for long lists and deep nests (but can still overflow on other
/// deep tree shapes, but those are rare).
impl<'s1, 's2, ET1, ET2, DR1, DR2>
    PartialEq<Datum<'s2, ET2, DR2>>
    for Datum<'s1, ET1, DR1>
    where DR1: DerefTryMut<Target = Datum<'s1, ET1, DR1>>,
          DR2: DerefTryMut<Target = Datum<'s2, ET2, DR2>>,
          ET1: PartialEq<ET2>,
{
    fn eq(&self, other: &Datum<'s2, ET2, DR2>) -> bool {
        let (mut slf, mut oth) = (self, other);
        loop {
            match (slf, oth) {
                (Text(ps1), Text(ps2))
                    => break ps1.val == ps2.val,
                (Combination{operator: rtr1, operands: rnds1},
                 Combination{operator: rtr2, operands: rnds2})
                    => if **rnds1 == **rnds2 {
                        slf = &**rtr1;
                        oth = &**rtr2;
                    } else { break false },
                (EmptyNest, EmptyNest)
                    => break true,
                (List{elem: e1, next: n1}, List{elem: e2, next: n2})
                    => if **e1 == **e2 {
                        slf = &**n1;
                        oth = &**n2;
                    } else { break false },
                (EmptyList, EmptyList)
                    => break true,
                (Extra(et1), Extra(et2))
                    => break et1 == et2,
                _
                    => break false
            }
        }
    }
}

/// Exists to be used similarly to but differently than
/// [`DerefMut`](http://doc.rust-lang.org/std/ops/trait.DerefMut.html) so that
/// types like [`Rc`](http://doc.rust-lang.org/std/rc/struct.Rc.html) and its
/// [`get_mut`](http://doc.rust-lang.org/std/rc/struct.Rc.html#method.get_mut)
/// method can be used to hold [`Datum`s](enum.Datum.html).  `DerefMut` must
/// never fail, so it can't be used.  We want mutability of `Datum`s so that we
/// can construct [lists of them](enum.Datum.html#variant.List) using only the
/// space of the values allocated by
/// [`Parser::new_datum`](trait.Parser.html#tymethod.new_datum), since this
/// crate is intended to be usable in `no_std` environments which don't provide
/// heap allocation.
///
/// The alternative of using tree-recursion to acheive temporary stack
/// allocation for constructing the lists without mutation is not good because
/// the stack is too small for large lists and it could realistically be
/// exceeded (causing a crash).  While `no_std` environments might also have
/// restricted limits to the amounts of `Datum`s that could be allocated, with
/// our approach they can control the limit and can receive an
/// [`Error`](enum.Error.html#variant.AllocExhausted) value under their control
/// if the limit is exceeded (instead of a crash).
pub trait DerefTryMut: Deref
    where <Self as Deref>::Target: Sized,
{
    /// Returns a mutable reference to the inner `Datum` if it can.  Otherwise,
    /// returns `None`.  Some implementations may never return `None` (e.g. for
    /// types that also implement `DerefMut`).
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target>;
}

/// This assists in basic direct mutable borrow references being used as the
/// `Datum` reference type.
pub type MutRefDatum<'d, 's, ET> = Datum<'s, ET, DatumMutRef<'d, 's, ET>>;

/// This wrapper allows the needed recursive type definition for basic direct
/// mutable borrow references to be used as the `Datum` reference type.
#[derive(PartialEq, Eq, Debug)]
pub struct DatumMutRef<'d, 's, ET>(pub &'d mut MutRefDatum<'d, 's, ET>)
    where 's: 'd;

impl<'d, 's, ET> Deref for DatumMutRef<'d, 's, ET>
    where 's: 'd
{
    type Target = MutRefDatum<'d, 's, ET>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'d, 's, ET> DerefMut for DatumMutRef<'d, 's, ET>
    where 's: 'd
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// This allows basic direct mutable borrow references to be used as the `Datum`
/// reference type.
impl<'d, 's, ET> DerefTryMut for DatumMutRef<'d, 's, ET>
    where 's: 'd
{
    fn get_mut(this: &mut Self) -> Option<&mut Self::Target> {
        Some(DerefMut::deref_mut(this))
    }
}


/// This module is needed so that these are public, as required, but not
/// exported from the crate.
mod source_iter {
    use super::*;

    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct SourceIterItem {
        pub ch: char,
        pub byte_pos: usize,
        pub char_pos: usize,
    }

    /// An iterator of the characters of a string that also yields the byte and
    /// character positions and that offsets the positions according to the
    /// string's offset relative to the original input string.
    #[derive(Clone, Debug)]
    pub struct SourceIter<'s> {
        pub ei_iter: Enumerate<CharIndices<'s>>,
        pub byte_offset: usize,
        pub char_offset: usize,
    }

    impl<'s> SourceIter<'s> {
        pub fn new(PosStr{val, byte_pos: byte_offset, char_pos: char_offset, ..
                         }: PosStr<'s>)
               -> Self
        {
            SourceIter {
                ei_iter: val.char_indices().enumerate(),
                byte_offset,
                char_offset,
            }
        }
    }

    impl Iterator for SourceIter<'_> {
        type Item = SourceIterItem;

        fn next(&mut self) -> Option<Self::Item> {
            self.ei_iter.next().map(|(char_pos, (byte_pos, ch))|
                                    SourceIterItem{ch,
                                                   byte_pos: self.byte_offset
                                                             + byte_pos,
                                                   char_pos: self.char_offset
                                                             + char_pos})
        }
    }
}

use self::source_iter::*;


// TODO: Exercise combiners in the test suite

/// This module is needed so that the traits are public, as required by
/// `Combiner`, but not exported from the crate.
mod combiner {
    pub trait OperativeTrait { }
    pub trait ApplicativeTrait { }
}

use self::combiner::*;

/// A macro function, bound to an operator sub-form, which is called with the
/// operands sub-form(s) to determine what should be substituted for the whole
/// form.  The `OperativeRef` and `ApplicativeRef` type parameters determine the
/// types used to refer to the functions.
///
/// While these parameters as defined here can allow a broad range of types
/// (including possibly inconsistent ones), further bounds on these are required
/// by the [`Parser`](trait.Parser.html) definition which ensures that only
/// consistent ones can be used with it, which is the only intended use of this
/// type.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Combiner<OperativeRef, ApplicativeRef>
    where OperativeRef: DerefMut,
          OperativeRef::Target: OperativeTrait,
          ApplicativeRef: DerefMut,
          ApplicativeRef::Target: ApplicativeTrait,
{
    Operative(OperativeRef),
    Applicative(ApplicativeRef),
}

impl<'s, ET, DR, CE> OperativeTrait for OpFn<'s, ET, DR, CE>
    where DR: DerefTryMut<Target = Datum<'s, ET, DR>>,
{ }

impl<'s, ET, DR, CE> ApplicativeTrait for ApFn<'s, ET, DR, CE>
    where DR: DerefTryMut<Target = Datum<'s, ET, DR>>,
{ }

/// The type of "operative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-form as
/// a `Datum::Text` containing the unparsed operands text; and the third
/// argument is the allocator state which can be used if constructing new
/// `Datum`s is needed for the return value.  See
/// [`CombinerResult`](type.CombinerResult.html) for the description of the
/// return value.
pub type OpFn<'s, ET, DR, CE> = dyn FnMut(DR, DR) -> CombinerResult<'s, ET, DR, CE>;

/// The type of "applicative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-forms
/// as a `Datum::List` containing the recursively parsed operands as separate
/// `Datum`s, or it is a `Datum::EmptyList` if the operands text was empty; and
/// the third argument is the allocator state which can be used if constructing
/// new `Datum`s is needed for the return value.  See
/// [`CombinerResult`](type.CombinerResult.html) for the description of the
/// return value.
pub type ApFn<'s, ET, DR, CE> = dyn FnMut(DR, DR) -> CombinerResult<'s, ET, DR, CE>;

/// The type returned by "operative" and "applicative" functions.  For a
/// successful return, the returned `Datum` is substituted for the original form
/// by the parser in the AST it yields, and the returned allocator state is the
/// possibly-updated state passed into the combiner function.  An
/// [`Error`](enum.Error.html) is returned if the combiner fails for any reason.
pub type CombinerResult<'s, ET, DR, CE> = Result<(Datum<'s, ET, DR>), Error<CE>>;


/// Represents: the ability to parse a string; the characters used to delimit
/// the nesting form; the method of allocating the `Datum`s; and the environment
/// of bindings of macros.
pub trait Parser<'s> {
    /// The ["extra" type](enum.Datum.html#variant.Extra) for our `Datum` type.
    type ET;
    /// The type of references to [`Datum`s](enum.Datum.html) yielded by our
    /// parsing.
    type DR: DerefTryMut<Target = Datum<'s, Self::ET, Self::DR>>;
    /// The type of references to
    /// [`Operative`](enum.Combiner.html#variant.Operative) macro functions.
    type OR: DerefMut<Target = OpFn<'s, Self::ET, Self::DR, Self::CE>>;
    /// The type of references to
    /// [`Applicative`](enum.Combiner.html#variant.Applicative) macro functions.
    type AR: DerefMut<Target = ApFn<'s, Self::ET, Self::DR, Self::CE>>;
    /// The [combiner error extension](enum.Error.html#variant.FailedCombiner)
    /// type.
    type CE;

    /// Predicate that determines the character(s) used to delimit the start of
    /// our nesting form.  The default implementation uses the common `{`
    /// character.
    #[inline]
    fn is_nest_start(&self, c: char) -> bool {
        '{' == c
    }

    /// Predicate that determines the character(s) used to delimit the end of
    /// our nesting form.  The default implementation uses the common `}`
    /// character.
    #[inline]
    fn is_nest_end(&self, c: char) -> bool {
        '}' == c
    }

    /// Predicate that determines the character(s) used to escape the delimiter
    /// characters of our nesting form.  The default implementation uses the
    /// common `\` character.
    #[inline]
    fn is_nest_escape(&self, c: char) -> bool {
        '\\' == c
    }

    /// Predicate that determines the character(s) considered to be whitespace,
    /// which affects the delimiting of operator and operands in our nesting
    /// form.  The default implementation uses the common Unicode property.
    #[inline]
    fn is_whitespace(&self, c: char) -> bool {
        c.is_whitespace()
    }

    /// The primary method.  Parse the given string's text, according to the
    /// specific parameterization of our `Self`, and return an iterator that
    /// yields each top-level form as a `Datum` AST.  The default implementation
    /// constructs a new iterator and supplies our allocation state to it (which
    /// might be ignored).
    fn parse<'p>(&'p mut self, source: &'s str) -> ParseIter<'p, 's, Self> {
        ParseIter::new(self, PosStr{val: source, src: source,
                                    byte_pos: 0, char_pos: 0})
    }

    /// Look-up any binding we might have associated with the given datum,
    /// referenced by the `operator` argument, which was found in operator
    /// (first, "head") position of a nested form.  If we do have a binding for
    /// it, return the "combiner" function that determines the semantics of the
    /// entire form and further parses and processes it in possibly arbitrary
    /// ways.  Else if we do not have a binding, return `None` to indicate that
    /// the form should not be handled according to the operator and that the
    /// operands should simply be recursively parsed.
    fn env_lookup(&mut self, operator: &Self::DR)
                  -> Option<Combiner<Self::OR, Self::AR>>;

    /// Allocate a fresh [`Datum`](enum.Datum.html), in whatever way the
    /// particular implementation wants, and set its initial value to that of
    /// the `from` argument.  Note that the `alst` (allocation state) argument
    /// may be ignored.  An [`Error`](enum.Error.html) is returned if allocation
    /// fails for any reason.
    fn new_datum(&mut self, from: Datum<'s, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>;
}


/// The type of values given by the parser iterator
pub type ParseIterItem<DR, CE> = Result<DR, Error<CE>>;

/// An [`Iterator`](http://doc.rust-lang.org/std/iter/trait.Iterator.html) that
/// parses its input text one top-level form at a time per each call to
/// [`next`](http://doc.rust-lang.org/std/iter/trait.Iterator.html#tymethod.next),
/// and yields a [`Datum`](enum.Datum.html) AST for each or an
/// [`Error`](enum.Error.html), according to the given
/// [`Parser`](trait.Parser.html)'s parameterization.
pub struct ParseIter<'p, 's, P>
    where P: 'p + Parser<'s> + ?Sized,
          's: 'p,
{
    parser: &'p mut P,
    src_str: PosStr<'s>,
    src_iter: Peekable<SourceIter<'s>>,
}

impl<'p, 's, P> Iterator for ParseIter<'p, 's, P>
    where P: 'p + Parser<'s> + ?Sized,
          's: 'p,
{
    type Item = ParseIterItem<<P as Parser<'s>>::DR, <P as Parser<'s>>::CE>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.do_next() {
            Ok(Some(dr)) => Some(Ok(dr)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}

impl<'p, 's, P> ParseIter<'p, 's, P>
    where P: 'p + Parser<'s> + ?Sized,
          's: 'p,
{
    // TODO: Can `type = ...` be used here for shorter aliases of long ones below?

    fn new(parser: &'p mut P, src_str: PosStr<'s>) -> Self {
        ParseIter{
            parser,
            src_str,
            src_iter: SourceIter::new(src_str).peekable(),
        }
    }

    fn read_to_end<F: Fn(&P, char) -> bool>
        (&mut self, init_ws_sig: bool, is_end_char: F)
         -> Result<PosStr<'s>, Error<<P as Parser<'s>>::CE>>
    {
        let mut nest_level: usize = 0;
        let (mut _prev_byte_pos, mut prev_char_pos): (usize, usize) = (0, 0);
        let (mut start_byte_pos, mut start_char_pos): (usize, usize) = (0, 0);
        let end_byte_pos: usize;
        let mut _end_char_pos: usize = 0;
        let mut first = true;

        loop {
            match self.src_iter.peek() {
                Some(&SourceIterItem{ch, byte_pos, char_pos}) => {
                    if first {
                        // Skip leading whitespace if indicated
                        if ! init_ws_sig && self.parser.is_whitespace(ch) {
                            self.src_iter.next(); // Consume peeked
                            continue;
                        }
                        // Record first char position
                        else {
                            start_byte_pos = byte_pos;
                            start_char_pos = char_pos;
                            first = false;
                        }
                    }

                    // Reached end. Do not consume peeked end char
                    if nest_level == 0 && is_end_char(self.parser, ch) {
                        end_byte_pos = byte_pos;
                        _end_char_pos = char_pos;
                        break;
                    }
                    // Consume escaped char whatever it might be
                    else if self.parser.is_nest_escape(ch) {
                        self.src_iter.next(); // Consume peeked first
                        self.src_iter.next();
                    }
                    // Start of nest. Track nesting depth
                    else if self.parser.is_nest_start(ch) {
                        self.src_iter.next(); // Consume peeked
                        nest_level += 1;
                    }
                    // End of nest. Check balanced nesting
                    else if self.parser.is_nest_end(ch) {
                        self.src_iter.next(); // Consume peeked
                        if nest_level > 0 {
                            nest_level -= 1;
                        } else {
                            return Err(Error::UnbalancedEndChar{byte_pos, char_pos});
                        }
                    }
                    // Accumulate this char
                    else {
                        self.src_iter.next(); // Consume peeked
                    }

                    _prev_byte_pos = byte_pos;
                    prev_char_pos = char_pos;
                },
                None => {
                    let vlen = self.src_str.val.len();
                    let bend = self.src_str.byte_pos + vlen;
                    let cend;
                    if first {
                        // Computing clen is linear here, but this case is rare (right?)
                        let clen = self.src_str.val.chars().count();
                        cend = self.src_str.char_pos + clen;
                        start_byte_pos = bend;
                        start_char_pos = cend;
                        // first = false; // compiler complains about "never read"
                    } else {
                        cend = prev_char_pos + 1;
                    }
                    end_byte_pos = bend;
                    _end_char_pos = cend;
                    break;
                }
            }
        }
        // Now that we have the start and end identified, return a string from
        // that which also records the position relative to the input text.
        let val = &self.src_str.src[start_byte_pos .. end_byte_pos];
        Ok(PosStr{val, src: self.src_str.src,
                  byte_pos: start_byte_pos, char_pos: start_char_pos})
    }

    fn parse_next_text(&mut self) -> Result<<P as Parser<'s>>::DR,
                                            Error<<P as Parser<'s>>::CE>>
    {
        let text = Text(self.read_to_end(true, <P as Parser<'s>>::is_nest_start)?);
        Ok(self.parser.new_datum(text)?)
    }

    fn parse_nested(&mut self) -> Result<Option<(<P as Parser<'s>>::DR,
                                                 <P as Parser<'s>>::DR)>,
                                         Error<<P as Parser<'s>>::CE>>
    {
        // Head text is delimited by whitespace or nest chars
        let head = self.read_to_end(false, |parser, c| parser.is_whitespace(c)
                                                       || parser.is_nest_start(c)
                                                       || parser.is_nest_end(c))?;
        // Empty string, or all whitespace, is empty nest form
        if head.len() == 0 {
            return Ok(None);
        }
        // If head delimited by following whitespace, advance passed first
        // whitespace char
        if let Some(&SourceIterItem{ch, ..}) = self.src_iter.peek() {
            if self.parser.is_whitespace(ch) { self.src_iter.next(); }
        }
        // Head text is interpreted as an operator form to recursively
        // parse. Note that, because we know head can only be a single form at
        // this point, we know we only need the first (next) datum returned by
        // the recursive parse iterator in order to cover it completely.
        let head = self.recur_on_str_once(head)?;
        // Rest text is delimited by end of our nest, and is interpreted here as
        // unparsed text
        let rest = Text(self.read_to_end(true, <P as Parser<'s>>::is_nest_end)?);
        let rest = self.parser.new_datum(rest)?;
        Ok(Some((head, rest)))
    }

    fn recur_on_str_once(&mut self, s: PosStr<'s>)
                         -> Result<<P as Parser<'s>>::DR,
                                   Error<<P as Parser<'s>>::CE>>
    {
        // Note: We use unwrap on purpose because None is impossible
        self.recur(s, |slf| slf.do_next().map(Option::unwrap))
    }

    fn recur<F, T>(&mut self, s: PosStr<'s>, f: F)
                   -> Result<T, Error<<P as Parser<'s>>::CE>>
        where F: FnOnce(&mut Self) -> Result<T, Error<<P as Parser<'s>>::CE>>
    {
        let save_src_str = replace(&mut self.src_str, s);
        let save_src_iter = replace(&mut self.src_iter, SourceIter::new(s).peekable());
        let r = f(self);
        self.src_str = save_src_str;
        self.src_iter = save_src_iter;
        r
    }

    fn collect_datumlist(&mut self)
                         -> Result<<P as Parser<'s>>::DR,
                                   Error<<P as Parser<'s>>::CE>>
    {
        let mut head = self.parser.new_datum(EmptyList)?;
        let mut tail = &mut head;
        loop {
            let it = self.do_next()?;
            if let Some(next_it) = it {
                if let Some(d) = DerefTryMut::get_mut(tail) {
                    let rest = self.parser.new_datum(EmptyList)?;
                    *d = List{elem: next_it, next: rest};
                    match d {
                        List{next, ..} => tail = next,
                        _ => unreachable!()
                    }
                } else {
                    return Err(Error::FailedDerefTryMut);
                }
            } else {
                break;
            }
        }
        Ok(head)
    }

    fn recur_on_str_all(&mut self, s: PosStr<'s>)
                        -> Result<<P as Parser<'s>>::DR,
                                  Error<<P as Parser<'s>>::CE>>
    {
        self.recur(s, Self::collect_datumlist)
    }

    fn do_next(&mut self) -> Result<Option<<P as Parser<'s>>::DR>,
                                    Error<<P as Parser<'s>>::CE>>
        // TODO?: Error variant that holds AS for errors where it is ok to return
        // the alloc-state?
    {
        let (ch, byte_pos, char_pos) = match self.src_iter.peek() {
            Some(&SourceIterItem{ch, byte_pos, char_pos}) => (ch, byte_pos, char_pos),
            None => return Ok(None)
        };

        // Start of a nest, either a combination or an empty nest
        if self.parser.is_nest_start(ch) {
            // Advance past peeked char
            self.src_iter.next();
            // Parse to the end of our nest level.
            let nested = self.parse_nested()?;
            // Consume our nest's end char, whatever it is.  If erroneous, by
            // consuming it, we could allow the possibility that this iterator
            // could be resumed again, but, in the current design, an erroneous
            // char shouldn't be possible.  It is possible that our nest is
            // missing its end char and end-of-stream occurred, which is the
            // `else` case.
            if let Some(SourceIterItem{ch: ec, ..}) = self.src_iter.next() {
                debug_assert!(self.parser.is_nest_end(ec));
            } else { return Err(Error::MissingEndChar); }
            // Process the nested datums accordingly.
            let next_datum = match nested {
                Some((operator, operands)) => {
                    let rands_str = || -> PosStr<'s> {
                        match &*operands {
                            &Text(val) => val,
                            _ => unreachable!()
                        }
                    };
                    match self.parser.env_lookup(&operator) {
                        // Form in operator position is bound, so delegate the
                        // processing of the operands and the choice of return
                        // value.
                        Some(combiner) => match combiner {
                            Operative(mut opr) =>
                                // Is given the text unparsed so it can do
                                // whatever it wants with it
                                opr.deref_mut()(operator, operands)?,
                            Applicative(mut apl) => {
                                let rands_list = self.recur_on_str_all(rands_str())?;
                                // Is given the parse of the text
                                apl.deref_mut()(operator, rands_list)?
                            }
                        },
                        // Not bound, so simply recursively parse operands and
                        // return a value representing the "combination" of
                        // operator and operands.
                        None => {
                            let rands_list = self.recur_on_str_all(rands_str())?;
                            Combination{operator, operands: rands_list}
                        },
                    }
                },
                None => EmptyNest,
            };
            return Ok(Some(self.parser.new_datum(next_datum)?));
        }

        // Invalid unbalanced nest end character
        else if self.parser.is_nest_end(ch) {
            // Advance past peeked char. By consuming it, we allow the
            // possibility that this iterator could be resumed again.
            self.src_iter.next();
            return Err(Error::UnbalancedEndChar{byte_pos, char_pos});
        }

        // All other characters are simply retained unparsed, with our
        // minimal escape char handling
        else {
            // Note: We did not consume our peeked char, so it will be seen again.
            // Note: parse_next_text already does parser.new_datum
            return self.parse_next_text().map(Some);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn datum_equality_same() {
        use super::Datum::*;

        assert_eq!(Text::<(), DatumMutRef<()>>(
                       PosStr{val:"", src:"", byte_pos:0, char_pos:0}),
                   Text::<(), DatumMutRef<()>>(
                       PosStr{val:"", src:"", byte_pos:0, char_pos:0}));

        assert_eq!(Text::<(), DatumMutRef<()>>(
                       PosStr{val:"z", src:"z", byte_pos:0, char_pos:0}),
                   Text::<(), DatumMutRef<()>>(
                       PosStr{val:"z", src:"{z}", byte_pos:1, char_pos:1}));

        assert_eq!(Combination::<(), DatumMutRef<()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)},
                   Combination::<(), DatumMutRef<()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyNest::<(), DatumMutRef<()>>,
                   EmptyNest::<(), DatumMutRef<()>>);

        assert_eq!(List::<(), DatumMutRef<()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)},
                   List::<(), DatumMutRef<()>>{
                       elem: DatumMutRef(&mut EmptyNest),
                       next: DatumMutRef(&mut EmptyList)});

        assert_eq!(EmptyList::<(), DatumMutRef<()>>,
                   EmptyList::<(), DatumMutRef<()>>);

        assert_eq!(Extra::<(), DatumMutRef<()>>(()),
                   Extra::<(), DatumMutRef<()>>(()));

        // TODO: More cases, including !=
    }

    mod datumref {
        use super::*;

        #[derive(Copy, Clone, Debug)]
        pub struct DatumRef<'d, 's, ET>(pub &'d Datum<'s, ET, DatumRef<'d, 's, ET>>);

        impl<'d, 's, ET> Deref for DatumRef<'d, 's, ET>
            where 's: 'd
        {
            type Target = Datum<'s, ET, DatumRef<'d, 's, ET>>;

            fn deref(&self) -> &Self::Target {
                self.0
            }
        }

        impl<'d, 's, ET> DerefTryMut for DatumRef<'d, 's, ET>
            where 's: 'd
        {
            fn get_mut(_this: &mut Self) -> Option<&mut Self::Target> {
                None
            }
        }
    }

    #[test]
    fn datum_equality_diff_ref() {
        use super::Datum::*;
        use self::datumref::DatumRef;

        assert_eq!(Text::<(), DatumMutRef<()>>(
                       PosStr{val:"", src:"", byte_pos:0, char_pos:0}),
                   Text::<(), DatumRef<()>>(
                       PosStr{val:"", src:"", byte_pos:0, char_pos:0}));

        assert_eq!(Combination::<(), DatumMutRef<()>>{
                       operator: DatumMutRef(&mut EmptyNest),
                       operands: DatumMutRef(&mut EmptyList)},
                   Combination::<(), DatumRef<()>>{
                       operator: DatumRef(&EmptyNest),
                       operands: DatumRef(&EmptyList)});

        assert_eq!(EmptyNest::<(), DatumMutRef<()>>,
                   EmptyNest::<(), DatumRef<()>>);

        assert_eq!(List::<(), DatumMutRef<()>>{elem: DatumMutRef(&mut EmptyNest),
                                               next: DatumMutRef(&mut EmptyList)},
                   List::<(), DatumRef<()>>{elem: DatumRef(&EmptyNest),
                                            next: DatumRef(&EmptyList)});

        assert_eq!(EmptyList::<(), DatumMutRef<()>>,
                   EmptyList::<(), DatumRef<()>>);

        assert!(Extra::<(), DatumMutRef<()>>(())
                == Extra::<(), DatumRef<()>>(()));
    }

    #[test]
    fn datum_copy_clone() {
        use super::Datum::*;
        use self::datumref::DatumRef;

        let a = List::<(), DatumRef<()>>{
            elem: DatumRef(&EmptyNest::<(), DatumRef<()>>),
            next: DatumRef(&EmptyList::<(), DatumRef<()>>)};
        let b = a;
        assert_eq!(a, b);

        let c = List::<(), DatumRef<()>>{
            elem: DatumRef(&EmptyNest::<(), DatumRef<()>>),
            next: DatumRef(&EmptyList::<(), DatumRef<()>>)};
        let d = c.clone();
        assert_eq!(c, d);
    }

    #[test]
    fn error_equality() {
        use super::Error::*;

        assert_eq!(UnbalancedEndChar::<()>{byte_pos:2, char_pos:1},
                   UnbalancedEndChar::<()>{byte_pos:2, char_pos:1});

        assert_eq!(MissingEndChar::<()>, MissingEndChar::<()>);

        assert_eq!(FailedAlloc::<()>(AllocError::AllocExhausted),
                   FailedAlloc::<()>(AllocError::AllocExhausted));

        assert_eq!(FailedDerefTryMut::<()>, FailedDerefTryMut::<()>);

        assert_eq!(FailedCombiner::<i32>(1), FailedCombiner::<i32>(1));
    }
}
