//! Parts for "combiners".  Combiners are custom user-defined macros for our
//! notation/format/language.

use core::ops::DerefMut;

use crate::{Datum, DerefTryMut, Error};


// TODO: Still needed?
/// This module is needed so that the traits are public, as required by
/// `Combiner`, but not exported.
mod combiner {
    pub trait OperativeTrait { }
    pub trait ApplicativeTrait { }
}

use combiner::*;

/// A macro function, bound to an operator sub-form, which is called with the
/// operands sub-form(s) to determine what should be substituted for the whole
/// form.  The `OperativeRef` and `ApplicativeRef` type parameters determine the
/// types used to refer to the functions.
///
/// While these parameters as defined here can allow possibly inconsistent
/// types, further bounds on these are required by a `Parser`'s
/// [`OperatorBindings`](trait.OperatorBindings.html) which ensures that only
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

impl<TT, ET, DR, Pos, CE> OperativeTrait for OpFn<TT, ET, DR, Pos, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{ }

impl<TT, ET, DR, Pos, CE> ApplicativeTrait for ApFn<TT, ET, DR, Pos, CE>
    where DR: DerefTryMut<Target = Datum<TT, ET, DR>>,
{ }

/// The type of "operative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-form as
/// a `Datum::Text` containing the unparsed operands text.  See
/// [`combiner::Result`](type.Result.html) for the description of the return
/// value.
pub type OpFn<TT, ET, DR, Pos, CE> = dyn FnMut(DR, DR) -> Result<TT, ET, DR, Pos, CE>;

/// The type of "applicative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-forms
/// as a `Datum::List` containing the recursively parsed operands as separate
/// `Datum`s, or it is a `Datum::EmptyList` if the operands text was empty.  See
/// [`combiner::Result`](type.Result.html) for the description of the return
/// value.
pub type ApFn<TT, ET, DR, Pos, CE> = dyn FnMut(DR, DR) -> Result<TT, ET, DR, Pos, CE>;

/// The type returned by "operative" and "applicative" functions.  For a
/// successful return, the returned `Datum` is substituted for the original form
/// by the parser in the AST it yields.  An [`Error`](enum.Error.html) is
/// returned if the combiner fails for any reason.
pub type Result<TT, ET, DR, Pos, CE>
    = core::result::Result<Datum<TT, ET, DR>, Error<Pos, CE>>;
