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

// TODO: Remove RefMut support after initial commit

// TODO?: Use mod modules to organize better?

// TODO: Review what's pub and not

#![no_std]

use core::mem::replace;
use core::ops::{Deref, DerefMut};
use core::str::CharIndices;
use core::iter::{Peekable, Enumerate};
use core::cell::RefMut;

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
    /// `Datum` allocator has no more free
    AllocExhausted,
    /// Prior error destroyed allocator state
    NoAllocState,
    /// [`DerefTryMut::get_mut`](trait.DerefTryMut.html#tymethod.get_mut) failed
    DerefTryMut,
    /// Extensibility that custom macros/combiners may utilize to add additional
    /// error variants
    FailedCombiner(CombinerError),
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
            (AllocExhausted, AllocExhausted)
                => true,
            (NoAllocState, NoAllocState)
                => true,
            (DerefTryMut, DerefTryMut)
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
/// type used to refer to the other `Datum`s in an AST DAG (directed acyclic
/// graph).
#[derive(Copy, Clone, Eq, Debug)]
pub enum Datum<'s, ExtraType, DatumRef>
    where DatumRef: Deref<Target = Datum<'s, ExtraType, DatumRef>>,
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
/// types](enum.Datum.html#variant.Extra) can be.
impl<'s1, 's2, ET1, ET2, DR1, DR2>
    PartialEq<Datum<'s2, ET2, DR2>>
    for Datum<'s1, ET1, DR1>
    where DR1: Deref<Target = Datum<'s1, ET1, DR1>>,
          DR2: Deref<Target = Datum<'s2, ET2, DR2>>,
          ET1: PartialEq<ET2>,
{
    fn eq(&self, other: &Datum<'s2, ET2, DR2>) -> bool {
        match (self, other) {
            (Text(ps1), Text(ps2))
                => ps1.val == ps2.val,
            (Combination{operator: rtr1, operands: rnds1},
             Combination{operator: rtr2, operands: rnds2})
                => **rtr1 == **rtr2 && **rnds1 == **rnds2,
            (EmptyNest, EmptyNest)
                => true,
            (List{elem: e1, next: n1}, List{elem: e2, next: n2})
                // TODO: Instead of tree-recursion using limited stack space,
                // use a loop to compare the `next` tails, so that very long
                // lists don't blow the stack.  In langs like Scheme, this would
                // already automatically be tail-recursive, but alas...
                => **e1 == **e2 && **n1 == **n2,
            (EmptyList, EmptyList)
                => true,
            (Extra(et1), Extra(et2))
                => et1 == et2,
            _
                => false
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
/// heap allocation.  The alternative of using tree-recursion to acheive
/// temporary stack allocation for constructing the lists without mutation is
/// not good because the stack is too small for large lists and it could
/// realistically be exceeded (causing a crash).  While `no_std` environments
/// might also have restricted limits to the amounts of `Datum`s that could be
/// allocated, with our approach they can control the limit and can receive an
/// [`Error`](enum.Error.html#variant.AllocExhausted) value under their control
/// if the limit is exceeded (instead of a crash).
pub trait DerefTryMut<CE>: Deref
    where <Self as Deref>::Target: Sized,
{
    /// Must be set to `<Self as Deref>::Target` (by the implementor).  This is
    /// needed, instead of trying to use `Target`, because the `nightly`
    /// compiler does not see our `Target` as constrained to `Sized`, even
    /// though it is for this trait, it only sees the definition of `Target` in
    /// `Deref` which is `?Sized`.  The `1.31.0-beta` compiler doesn't seem to
    /// have this problem and can use `Target`, but I assume this should be made
    /// ready for a change coming in `nightly`?
    type SizedTarget;  // Automatically defaults to Sized

    /// Returns a mutable reference to the inner `Datum` if it can.  Otherwise,
    /// returns [`Error::DerefTryMut`](enum.Error.html#variant.DerefTryMut).
    /// Some implementations may never fail (e.g. for types that also implement
    /// `DerefMut`).
    fn get_mut(this: &mut Self) -> Result<&mut Self::SizedTarget, Error<CE>>;

    /// Mutates the inner `Datum` to be set to the given `value` argument, if
    /// [`get_mut`](#tymethod.get_mut) can get the mutable reference.
    /// Otherwise, returns the same error as `get_mut`.
    fn try_set(this: &mut Self, value: Self::SizedTarget) -> Result<(), Error<CE>>
    {
        *(DerefTryMut::get_mut(this)?) = value;
        Ok(())
    }
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
impl<'d, 's, ET, CE> DerefTryMut<CE> for DatumMutRef<'d, 's, ET>
    where 's: 'd
{
    type SizedTarget = Self::Target;

    fn get_mut(this: &mut Self) -> Result<&mut Self::SizedTarget, Error<CE>> {
        Ok(DerefMut::deref_mut(this))
    }
}

// TODO: Does providing support for `RefMut`s make enough sense?  Why would
// anyone ever want to use RefMuts instead of plain &mut which are already
// supported?  Further, using RefMuts has an inherent issue in that RefMut
// implements Drop and our recursive wrapper type must give the same lifetime to
// both a RefMutDatum and the referents of the DatumRefMuts it contains, which
// the compiler rejects due to the lifetime of the referents not strictly
// outliving the containing RefMut's lifetime.  This means that RefMutDatum
// cannot be used without resorting to using `ManuallyDrop`.

/// This assists in `RefMut`s of `RefCell`s being used as the `Datum` reference
/// type.
pub type RefMutDatum<'d, 's, ET> = Datum<'s, ET, DatumRefMut<'d, 's, ET>>;

/// This wrapper allows the needed recursive type definition for `RefMut`s of
/// `RefCell`s to be used as the `Datum` reference type.
#[derive(Debug)]
pub struct DatumRefMut<'d, 's, ET>(pub RefMut<'d, RefMutDatum<'d, 's, ET>>);

impl<'d, 's, ET> Deref for DatumRefMut<'d, 's, ET>
    where 's: 'd
{
    type Target = RefMutDatum<'d, 's, ET>;

    fn deref(&self) -> &Self::Target {
        Deref::deref(&self.0)
    }
}

impl<'d, 's, ET> DerefMut for DatumRefMut<'d, 's, ET>
    where 's: 'd
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        DerefMut::deref_mut(&mut self.0)
    }
}

// Crazy hackaround idea that didn't work
// pub type RefMutDatum<'f, 'c, 's, ET> = Datum<'s, ET, DatumRefMut<'f, 'c, 's, ET>>;
//
// pub struct DatumRefMut<'f, 'c, 's, ET>(
//     &'f dyn (for<'r> Fn(&'r RefCell<RefMutDatum<'f, 'c, 's, ET>>)
//                         -> &'r mut RefMut<'r, RefMutDatum<'f, 'c, 's, ET>>),
//     &'c RefCell<RefMutDatum<'f, 'c, 's, ET>>,
// );
//
// impl<'f, 'c, 's, ET> Deref for DatumRefMut<'f, 'c, 's, ET> {
//     type Target = RefMutDatum<'f, 'c, 's, ET>;
//
//     fn deref(&self) -> &Self::Target {
//         Deref::deref((self.0)(self.1))
//     }
// }
//
// impl<'f, 'c, 's, ET> DerefMut for DatumRefMut<'f, 'c, 's, ET> {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         DerefMut::deref_mut((self.0)(self.1))
//     }
// }

/// This allows `RefMut`s of `RefCell`s to be used as the `Datum` reference type.
impl<'d, 's, ET, CE> DerefTryMut<CE> for DatumRefMut<'d, 's, ET>
    where 's: 'd,
{
    type SizedTarget = Self::Target;

    fn get_mut(this: &mut Self) -> Result<&mut Self::SizedTarget, Error<CE>> {
        Ok(DerefMut::deref_mut(this))
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


// TODO: Our OperativeTrait and ApplicativeTrait usage allows a Combiner to be
// parameterized with types that do not have the same <'s, ET, DR, AS>, but this
// should not be allowed.  I did it like this so that Combiner can avoid having
// 's, ET, DR, AS in its parameterization, because I had some other problem with
// that I can't quite remember from when the rest of this crate was different.
// Maybe it can go back to having those and be bounded like:
// DerefMut<Target = OpFn<'s, ET, DR, AS>>

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

impl<'s, ET, DR, CE, AS> OperativeTrait for OpFn<'s, ET, DR, CE, AS>
    where DR: Deref<Target = Datum<'s, ET, DR>>,
{ }

impl<'s, ET, DR, CE, AS> ApplicativeTrait for ApFn<'s, ET, DR, CE, AS>
    where DR: Deref<Target = Datum<'s, ET, DR>>,
{ }

/// The type of "operative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-form as
/// a `Datum::Text` containing the unparsed operands text; and the third
/// argument is the allocator state which can be used if constructing new
/// `Datum`s is needed for the return value.  See
/// [`CombinerResult`](type.CombinerResult.html) for the description of the
/// return value.
pub type OpFn<'s, ET, DR, CE, AS> = dyn FnMut(DR, DR, AS)
                                              -> CombinerResult<'s, ET, DR, CE, AS>;

/// The type of "applicative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-forms
/// as a `Datum::List` containing the recursively parsed operands as separate
/// `Datum`s, or it is a `Datum::EmptyList` if the operands text was empty; and
/// the third argument is the allocator state which can be used if constructing
/// new `Datum`s is needed for the return value.  See
/// [`CombinerResult`](type.CombinerResult.html) for the description of the
/// return value.
pub type ApFn<'s, ET, DR, CE, AS> = dyn FnMut(DR, DR, AS)
                                              -> CombinerResult<'s, ET, DR, CE, AS>;

/// The type returned by "operative" and "applicative" functions.  For a
/// successful return, the returned `Datum` is substituted for the original form
/// by the parser in the AST it yields, and the returned allocator state is the
/// possibly-updated state passed into the combiner function.  An
/// [`Error`](enum.Error.html) is returned if the combiner fails for any reason.
pub type CombinerResult<'s, ET, DR, CE, AS> = Result<(Datum<'s, ET, DR>, AS), Error<CE>>;


/// This parameterizes the characters used to delimit the nesting form.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ParserConfig {
    pub nest_start: char,
    pub nest_end: char,
    pub nest_escape: char,
}

/// Represents: the characters used to delimit the nesting form; and the method
/// of allocating the `Datum`s; and the environment of bindings of macros.
pub trait Parser<'s> {
    /// The `Datum` allocator state type.
    type AS;
    /// The ["extra" type](enum.Datum.html#variant.Extra) for our `Datum` type.
    type ET;
    /// The type of references to [`Datum`s](enum.Datum.html) yielded by our
    /// parsing.
    type DR: DerefTryMut<Self::CE,
                         Target = Datum<'s, Self::ET, Self::DR>,
                         SizedTarget = Datum<'s, Self::ET, Self::DR>>;
    /// The type of references to
    /// [`Operative`](enum.Combiner.html#variant.Operative) macro functions.
    type OR: DerefMut<Target = OpFn<'s, Self::ET, Self::DR, Self::CE, Self::AS>>;
    /// The type of references to
    /// [`Applicative`](enum.Combiner.html#variant.Applicative) macro functions.
    type AR: DerefMut<Target = ApFn<'s, Self::ET, Self::DR, Self::CE, Self::AS>>;
    /// The [combiner error extension](enum.Error.html#variant.FailedCombiner)
    /// type.
    type CE;

    /// Returns the characters used to delimit the start and end of our one
    /// nesting form and to escape those two delimiters.  This allows this crate
    /// to be parameterized over these, and allows the particular implementation
    /// to determine what it wants these to be.  Different delimiters are useful
    /// for different applications which want text to be able to contain many
    /// bracket characters without those being parsed as our nesting form
    /// delimiters.  (It would be nice if Rust's "associated constants" could be
    /// used instead with this trait, but that is an unstable feature.)  The
    /// default implementation uses the common `{`, `}`, and `\` characters.
    fn get_config(&self) -> ParserConfig {
        ParserConfig {
            nest_start: '{',
            nest_end: '}',
            nest_escape: '\\',
        }
    }

    /// This allows full generality in the type of state used to allocate the
    /// `Datum`s.  E.g. the allocation state (`Self::AS`) might be ignored, or
    /// it might always be independent of `Self`, or it might be owned or
    /// borrowed by `Self` and need to be temporarily disowned (so that it is
    /// not a borrow from `self` which would prevent any further calling of our
    /// methods).  The result of this method is passed around through functions
    /// so they can pass it to our [`new_datum`](#tymethod.new_datum) method,
    /// and it (possibly mutated), or a value derived from it, is finally passed
    /// back to ourself via our
    /// [`receive_alloc_state`](#tymethod.receive_alloc_state) method when
    /// parsing is finished.
    fn supply_alloc_state(&mut self) -> Self::AS;

    /// This allows the allocation state returned by our
    /// [`supply_alloc_state`](#tymethod.supply_alloc_state) method to be
    /// reassociated with `self` (or, in general, the particular
    /// implementation's facilities), if necessary, when parsing is finished.
    fn receive_alloc_state(&mut self, alst: Self::AS);

    /// The primary method.  Parse the given string's text, according to the
    /// specific parameterization of our `Self`, and return an iterator that
    /// yields each top-level form as a `Datum` AST.  The default implementation
    /// constructs a new iterator and supplies our allocation state to it (which
    /// might be ignored).
    fn parse<'p>(&'p mut self, source: &'s str) -> ParseIter<'p, 's, Self> {
        let alst = self.supply_alloc_state();
        ParseIter::new(self, alst, PosStr{val: source, src: source,
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
    fn new_datum(&mut self, from: Datum<'s, Self::ET, Self::DR>, alst: Self::AS)
                 -> Result<(Self::DR, Self::AS), Error<Self::CE>>;
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
    config: ParserConfig,
    src_str: PosStr<'s>,
    src_iter: Peekable<SourceIter<'s>>,
    alloc_state: Option<<P as Parser<'s>>::AS>,
    none_alst: bool,
}

impl<'p, 's, P> Iterator for ParseIter<'p, 's, P>
    where P: 'p + Parser<'s> + ?Sized,
          's: 'p,
{
    type Item = ParseIterItem<<P as Parser<'s>>::DR, <P as Parser<'s>>::CE>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.disown_alloc_state() {
            Some(alst) => {
                let (result, alst) = match self.do_next(alst) {
                    Ok((it, alst)) => (it.map(|dr| Ok(dr)), Some(alst)),
                    Err(e) => match e {
                        // TODO: for cases where alloc state was returned inside
                        // the error value, set alst to that so it's set back
                        // into self
                        _ => (Some(Err(e)), None)
                    }
                };
                self.own_alloc_state(alst);
                result
            },
            // If the allocation state is ever None, it's because there was a
            // prior error and that error value didn't include the allocation
            // state, and the iterator is now in a degraded state and can no
            // longer parse.  For the first time, we return an error to indicate
            // this, but any subsequent attempts will always indicate that
            // iteration is done.
            None => if ! self.none_alst {
                self.none_alst = true;
                Some(Err(Error::NoAllocState))
            } else { None }
        }
    }
}

/// Give the allocation state back to the `Parser` since we're done with it.
impl<'p, 's, P> Drop for ParseIter<'p, 's, P>
    where P: 'p + Parser<'s> + ?Sized,
          's: 'p,
{
    fn drop(&mut self) {
        if let Some(alst) = replace(&mut self.alloc_state, None) {
            self.parser.receive_alloc_state(alst);
        }
    }
}

impl<'p, 's, P> ParseIter<'p, 's, P>
    where P: 'p + Parser<'s> + ?Sized,
          's: 'p,
{
    // TODO: Can `type = ...` be used here for shorter aliases of long ones below?

    fn new(parser: &'p mut P,
           alloc_state: <P as Parser<'s>>::AS,
           src_str: PosStr<'s>)
           -> Self
    {
        let config = parser.get_config();
        ParseIter{
            parser,
            config,
            src_str,
            src_iter: SourceIter::new(src_str).peekable(),
            alloc_state: Some(alloc_state),
            none_alst: false,
        }
    }

    fn disown_alloc_state(&mut self) -> Option<<P as Parser<'s>>::AS> {
        replace(&mut self.alloc_state, None)
    }

    fn own_alloc_state(&mut self, alst: Option<<P as Parser<'s>>::AS>) {
        // Note: We want to panic if it's Some because that should be impossible
        assert!(self.alloc_state.is_none());
        self.alloc_state = alst;
    }

    fn read_to_end<F: Fn(char) -> bool>
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
                        if ! init_ws_sig && ch.is_whitespace() {
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
                    if nest_level == 0 && is_end_char(ch) {
                        end_byte_pos = byte_pos;
                        _end_char_pos = char_pos;
                        break;
                    }
                    // Consume escaped char whatever it might be
                    else if self.config.nest_escape == ch {
                        self.src_iter.next(); // Consume peeked first
                        self.src_iter.next();
                    }
                    // Start of nest. Track nesting depth
                    else if self.config.nest_start == ch {
                        self.src_iter.next(); // Consume peeked
                        nest_level += 1;
                    }
                    // End of nest. Check balanced nesting
                    else if self.config.nest_end == ch {
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

    fn parse_next_text(&mut self, alst: <P as Parser<'s>>::AS)
                       -> Result<(<P as Parser<'s>>::DR,
                                  <P as Parser<'s>>::AS),
                                 Error<<P as Parser<'s>>::CE>>
    {
        let nest_start = self.config.nest_start;
        let text = Text(self.read_to_end(true, |c| nest_start == c)?);
        self.parser.new_datum(text, alst)
    }

    fn parse_nested(&mut self, alst: <P as Parser<'s>>::AS)
                    -> Result<(Option<(<P as Parser<'s>>::DR,
                                       <P as Parser<'s>>::DR)>,
                               <P as Parser<'s>>::AS),
                              Error<<P as Parser<'s>>::CE>>
    {
        // Must copy these values, since if the closure borrowed, it'd conflict
        let nest_start = self.config.nest_start;
        let nest_end = self.config.nest_end;
        // Head text is delimited by whitespace or nest chars
        let head = self.read_to_end(false, |c| c.is_whitespace()
                                               || nest_start == c
                                               || nest_end == c)?;
        // Empty string, or all whitespace, is empty nest form
        if head.len() == 0 {
            return Ok((None, alst));
        }
        // If head delimited by following whitespace, advance passed first
        // whitespace char
        if let Some(&SourceIterItem{ch, ..}) = self.src_iter.peek() {
            if ch.is_whitespace() { self.src_iter.next(); }
        }
        // Head text is interpreted as an operator form to recursively
        // parse. Note that, because we know head can only be a single form at
        // this point, we know we only need the first (next) datum returned by
        // the recursive parse iterator in order to cover it completely.
        let (head, alst) = self.recur_on_str_once(head, alst)?;
        // Rest text is delimited by end of our nest, and is interpreted here as
        // unparsed text
        let rest = Text(self.read_to_end(true, |c| nest_end == c)?);
        let (rest, alst) = self.parser.new_datum(rest, alst)?;
        Ok((Some((head, rest)), alst))
    }

    fn recur_on_str_once(&mut self, s: PosStr<'s>, alst: <P as Parser<'s>>::AS)
                         -> Result<(<P as Parser<'s>>::DR,
                                    <P as Parser<'s>>::AS),
                                   Error<<P as Parser<'s>>::CE>>
    {
        // Note: We use unwrap on purpose because None is impossible
        self.recur(s,
                   |slf, alst| slf.do_next(alst).map(|(it, alst)|
                                                     (it.unwrap(), alst)),
                   alst)
    }

    fn recur<F, T>(&mut self, s: PosStr<'s>, f: F, alst: <P as Parser<'s>>::AS)
                   -> Result<(T, <P as Parser<'s>>::AS),
                             Error<<P as Parser<'s>>::CE>>
        where F: FnOnce(&mut Self, <P as Parser<'s>>::AS)
                        -> Result<(T, <P as Parser<'s>>::AS),
                                  Error<<P as Parser<'s>>::CE>>
    {
        let save_src_str = replace(&mut self.src_str, s);
        let save_src_iter = replace(&mut self.src_iter, SourceIter::new(s).peekable());
        let r = f(self, alst);
        self.src_str = save_src_str;
        self.src_iter = save_src_iter;
        r
    }

    fn collect_datumlist(&mut self, alst: <P as Parser<'s>>::AS)
                         -> Result<(<P as Parser<'s>>::DR,
                                    <P as Parser<'s>>::AS),
                                   Error<<P as Parser<'s>>::CE>>
    {
        let (mut head, mut alst) = self.parser.new_datum(EmptyList, alst)?;
        let mut tail = &mut head;
        loop {
            let (it, st) = self.do_next(alst)?;
            if let Some(next_it) = it {
                let (rest, st) = self.parser.new_datum(EmptyList, st)?;
                DerefTryMut::try_set(tail, List{elem: next_it, next: rest})?;
                match DerefTryMut::get_mut(tail)? {
                    List{next, ..} => tail = next,
                    _ => unreachable!()
                }
                alst = st;
            } else {
                alst = st;
                break;
            }
        }
        Ok((head, alst))
    }

    fn recur_on_str_all(&mut self, s: PosStr<'s>, alst: <P as Parser<'s>>::AS)
                        -> Result<(<P as Parser<'s>>::DR,
                                   <P as Parser<'s>>::AS),
                                  Error<<P as Parser<'s>>::CE>>
    {
        self.recur(s, Self::collect_datumlist, alst)
    }

    fn do_next(&mut self, alst: <P as Parser<'s>>::AS)
               -> Result<(Option<<P as Parser<'s>>::DR>,
                          <P as Parser<'s>>::AS),
                         Error<<P as Parser<'s>>::CE>>
        // TODO?: Error variant that holds AS for errors where it is ok to return
        // the alloc-state?
    {
        let (ch, byte_pos, char_pos) = match self.src_iter.peek() {
            Some(&SourceIterItem{ch, byte_pos, char_pos}) => (ch, byte_pos, char_pos),
            None => return Ok((None, alst))
        };

        // Start of a nest, either a combination or an empty nest
        if self.config.nest_start == ch {
            // Advance past peeked char
            self.src_iter.next();
            // Parse to the end of our nest level.
            let (nested, alst) = self.parse_nested(alst)?;
            // Consume our nest's end char, whatever it is.  If erroneous, by
            // consuming it, we could allow the possibility that this iterator
            // could be resumed again, but, in the current design, an erroneous
            // char shouldn't be possible.  It is possible that our nest is
            // missing its end char and end-of-stream occurred, which is the
            // `else` case.
            if let Some(SourceIterItem{ch: ec, ..}) = self.src_iter.next() {
                debug_assert!(ec == self.config.nest_end);
            } else { return Err(Error::MissingEndChar); }
            // Process the nested datums accordingly.
            let (next_datum, alst) = match nested {
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
                                opr.deref_mut()(operator, operands, alst)?,
                            Applicative(mut apl) => {
                                let (rands_list, alst)
                                    = self.recur_on_str_all(rands_str(), alst)?;
                                // Is given the parse of the text
                                apl.deref_mut()(operator, rands_list, alst)?
                            }
                        },
                        // Not bound, so simply recursively parse operands and
                        // return a value representing the "combination" of
                        // operator and operands.
                        None => {
                            let (rands_list, alst)
                                = self.recur_on_str_all(rands_str(), alst)?;
                            (Combination{operator, operands: rands_list},
                             alst)
                        },
                    }
                },
                None => (EmptyNest, alst),
            };
            return self.parser.new_datum(next_datum, alst)
                              .map(|(dr, alst)| (Some(dr), alst));
        }

        // Invalid unbalanced nest end character
        else if self.config.nest_end == ch {
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
            return self.parse_next_text(alst)
                       .map(|(dr, alst)| (Some(dr), alst));
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

        // TODO: Cases for DatumRefMut with DatumRefMut, if I keep that type
    }

    // #[test]
    // fn ref_cell_crap() {
    //     use core::cell::RefCell;
    //
    //     // let en2 = RefCell::new(Datum::EmptyNest::<'_, (), DatumRefMut<'_, '_, ()>>);
    //     // // let el2 = RefCell::new(Datum::EmptyList::<(), DatumRefMut<'_, '_, ()>>);
    //     // // {
    //     // //     let mut d = Datum::List::<(), DatumRefMut<'_, '_, ()>>{
    //     // //                     elem: DatumRefMut(en2.borrow_mut()),
    //     // //                     next: DatumRefMut(el2.borrow_mut())};
    //     // // }
    //     // // let wtf = en2.borrow_mut();
    //     // // let wtf: DatumRefMut<'l, '_, ()> = DatumRefMut(en2.borrow_mut());
    //     // let wtf = DatumRefMut(en2.borrow_mut());
    //     // // *wtf = Datum::EmptyList;
    //
    //     // Crazy
    //     // let rc = RefCell::new(Datum::EmptyNest::<'_, (), DatumRefMut<'_, '_, '_, ()>>);
    //     // let mut mr = rc.borrow_mut();
    //     // let f: &dyn (for<'r> Fn(&'r RefCell<RefMutDatum<'_, '_, '_, _>>)
    //     //                 -> &'r mut RefMut<'r, RefMutDatum<'_, '_, '_, _>>)
    //     //     = &(move |_| &mut mr);
    //     // let wtf = DatumRefMut(f, &rc);
    //
    //     // Can't even do this!
    //     // let en = RefCell::new(Datum::EmptyNest::<'_, (), RefMut<'_, Datum<'_, (), RefMut<'_, Datum<'_, (), RefMut<'_, _>>>>>>);
    //     // let el = RefCell::new(Datum::EmptyList::<'_, (), RefMut<'_, Datum<'_, (), RefMut<'_, Datum<'_, (), RefMut<'_, _>>>>>>);
    //     // let l = Datum::List::<'_, (), RefMut<'_, Datum<'_, (), RefMut<'_, Datum<'_, (), RefMut<'_, _>>>>>> {
    //     //     elem: en.borrow_mut(),
    //     //     next: el.borrow_mut(),
    //     // };
    // }

    #[test]
    fn datum_equality_diff_ref() {
        use super::Datum::*;
        // use core::cell::RefCell;

        // The commented-out cases don't work due to `RefMut` implementing
        // `Drop` and the lifetimes not strictly-outliving.

        assert_eq!(Text::<(), DatumMutRef<()>>(
                       PosStr{val:"", src:"", byte_pos:0, char_pos:0}),
                   Text::<(), DatumRefMut<()>>(
                       PosStr{val:"", src:"", byte_pos:0, char_pos:0}));

        // {
        //     let rtr1 = &mut EmptyNest::<(), DatumMutRef<()>>;
        //     let rnds1 = &mut EmptyList::<(), DatumMutRef<()>>;
        //     let d1 = Combination::<(), DatumMutRef<()>>{
        //                  operator: DatumMutRef(rtr1),
        //                  operands: DatumMutRef(rnds1)};
        //     let en2 = RefCell::new(EmptyNest::<(), DatumRefMut<()>>);
        //     let el2 = RefCell::new(EmptyList::<(), DatumRefMut<()>>);
        //     {
        //         let d2 = Combination::<(), DatumRefMut<()>>{
        //                      operator: DatumRefMut(en2.borrow_mut()),
        //                      operands: DatumRefMut(el2.borrow_mut())};
        //         assert!(d1 == d2);
        //     }
        // }

        assert_eq!(EmptyNest::<(), DatumMutRef<()>>,
                   EmptyNest::<(), DatumRefMut<()>>);

        // {
        //     let e1 = &mut EmptyNest::<(), DatumMutRef<()>>;
        //     let n1 = &mut EmptyList::<(), DatumMutRef<()>>;
        //     let d1 = List::<(), DatumMutRef<()>>{elem: DatumMutRef(e1),
        //                                                 next: DatumMutRef(n1)};
        //     let en2 = RefCell::new(EmptyNest::<(), DatumRefMut<()>>);
        //     let el2 = RefCell::new(EmptyList::<(), DatumRefMut<()>>);
        //     let d2 = List::<(), DatumRefMut<()>>{
        //                  elem: DatumRefMut(en2.borrow_mut()),
        //                  next: DatumRefMut(el2.borrow_mut())};
        //     assert_eq!(d1, d2);
        // }

        assert_eq!(EmptyList::<(), DatumMutRef<()>>,
                   EmptyList::<(), DatumRefMut<()>>);

        assert!(Extra::<(), DatumMutRef<()>>(())
                == Extra::<(), DatumRefMut<()>>(()));
    }

    #[test]
    fn datum_copy_clone() {
        use super::Datum::*;

        #[derive(Copy, Clone, Debug)]
        struct DatumRef<'d, 's, ET>(&'d Datum<'s, ET, DatumRef<'d, 's, ET>>);

        impl<'d, 's, ET> Deref for DatumRef<'d, 's, ET>
            where 's: 'd
        {
            type Target = Datum<'s, ET, DatumRef<'d, 's, ET>>;

            fn deref(&self) -> &Self::Target {
                self.0
            }
        }

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

        assert_eq!(AllocExhausted::<()>, AllocExhausted::<()>);

        assert_eq!(NoAllocState::<()>, NoAllocState::<()>);

        assert_eq!(DerefTryMut::<()>, DerefTryMut::<()>);

        assert_eq!(FailedCombiner::<i32>(1), FailedCombiner::<i32>(1));
    }
}
