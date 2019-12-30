//! Traits and types that provide the different aspects of `Parser`s'
//! functionality.

use core::ops::Deref;

use crate::{Datum, DerefTryMut, Combiner, Text, Error};
use crate::combiner::{OpFn, ApFn};


/// Implementations provided for ready use.
pub mod premade
{
    mod default_classifier;
    pub use default_classifier::DefaultCharClassifier;

    mod slice_alloc;
    pub use slice_alloc::SliceDatumAllocator;

    mod empty_bindings;
    pub use empty_bindings::EmptyOperatorBindings;

    mod pair_bindings;
    pub use pair_bindings::PairOperatorBindings;
}


/// The possible errors that might be returned by a parser's `Datum` allocator.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum AllocError {
    /// No more free
    AllocExhausted,
}

impl<Pos, CE> From<AllocError> for Error<Pos, CE> {
    fn from(ae: AllocError) -> Self {
        Self::FailedAlloc(ae)
    }
}


/// Classifier of `char`s that determines which are a `Parser`'s special
/// delimiters.
///
/// Enables different applications to use different sets of delimiters.
/// E.g. URI-friendly characters might be used instead.
///
/// Do not make more than one of the predicate methods return `true` for the
/// same `char`.
pub trait CharClassifier {
    /// Predicate that determines the character(s) used to delimit the start of
    /// our nesting form.
    fn is_nest_start(&self, c: char) -> bool;

    /// Predicate that determines the character(s) used to delimit the end of
    /// our nesting form.
    fn is_nest_end(&self, c: char) -> bool;

    /// Predicate that determines the character(s) used to escape the delimiter
    /// characters of our nesting form.
    fn is_nest_escape(&self, c: char) -> bool;

    /// Predicate that determines the character(s) considered to be whitespace,
    /// which affects the delimiting of operator and operands in our nesting
    /// form.
    fn is_whitespace(&self, c: char) -> bool;
}


/// Allocator of space for `Datum` values that a `Parser` constructs.
///
/// Enables the parsing logic to be generic over this aspect so that various
/// approaches may be chosen depending on the application.  E.g. heap vs. stack
/// allocation.
pub trait DatumAllocator {
    /// The `Text` type for our `Datum` type.  It should also implement
    /// `TextConcat<Self>` so it supports concatenation which the parsing
    /// requires.
    ///
    /// Such a bound is not required here, to avoid difficulties that otherwise
    /// would happen with needing to have the bound where not really needed and
    /// where unresolvable mutually-recursive bounds would result.  The bound is
    /// instead placed on the methods of `ParseIter` and `SourceStream`
    /// implementations where it's really needed.
    type TT: Text;
    /// The ["extra" type](enum.Datum.html#variant.Extra) for our `Datum` type.
    type ET;
    /// The type of references to [`Datum`s](enum.Datum.html) yielded by our
    /// parsing.
    type DR: DerefTryMut<Target = Datum<Self::TT, Self::ET, Self::DR>>;

    /// Allocate a fresh [`Datum`](enum.Datum.html), in whatever way the
    /// particular implementation wants, and set its initial value to that of
    /// the `from` argument.  An [`AllocError`](enum.AllocError.html) is
    /// returned if allocation fails for any reason.
    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>;
}


/// Environment of bindings of operator sub-forms to macro functions that
/// substitute forms in a `Parser`'s returned AST.
///
/// Enables different applications to use different sets or none at all.
pub trait OperatorBindings<DA>
    where DA: DatumAllocator,
{
    /// The type of references to
    /// [`Operative`](enum.Combiner.html#variant.Operative) macro functions.
    type OR: Deref<Target = OpFn<DA, Self::CE>>;
    /// The type of references to
    /// [`Applicative`](enum.Combiner.html#variant.Applicative) macro functions.
    type AR: Deref<Target = ApFn<DA, Self::CE>>;
    /// The [combiner error extension](enum.Error.html#variant.FailedCombiner)
    /// type.
    type CE;

    /// Look-up any binding we might have associated with the given datum,
    /// referenced by the `operator` argument, which was found in operator
    /// (first, "head") position of a nested form.  If we do have a binding for
    /// it, return the "combiner" function that determines the semantics of the
    /// entire form and further parses and processes it in possibly arbitrary
    /// ways.  Else if we do not have a binding, return `None` to indicate that
    /// the form should not be handled according to the operator and that the
    /// operands should simply be recursively parsed.
    fn lookup(&self, operator: &Datum<DA::TT, DA::ET, DA::DR>)
              -> Option<&Combiner<Self::OR, Self::AR>>;
}
