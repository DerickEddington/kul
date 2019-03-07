//! Parts for "combiners".  Combiners are custom user-defined macros for our
//! notation/format/language.

use crate::{
    Datum, Error, TextBase,
    parser::DatumAllocator,
};


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
#[derive(Debug)]
pub enum Combiner<OperativeRef, ApplicativeRef> {
    /// An "operative" combiner, which is given unparsed operands text.
    Operative(OperativeRef),
    /// An "applicative" combiner, which is given parsed operands list.
    Applicative(ApplicativeRef),
}

/// The type of "operative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-form as
/// the `Text` type containing the unparsed operands text; and the third
/// argument is the `Parser`'s `DatumAllocator`.  See
/// [`combiner::Result`](type.Result.html) for the description of the return
/// value.
pub type OpFn<DA, CE> = dyn Fn(DADatum<DA>, <DA as DatumAllocator>::TT, &mut DA)
                               -> Result<DA, CE>;

/// The type of "applicative" functions.  First argument is the "operator"
/// sub-form as a `Datum`; and the second argument is the "operands" sub-forms
/// as a `Datum::List` containing the recursively parsed operands as separate
/// `Datum`s, or it is a `Datum::EmptyList` if the operands text was empty; and
/// the third argument is the `Parser`'s `DatumAllocator`.  See
/// [`combiner::Result`](type.Result.html) for the description of the return
/// value.
pub type ApFn<DA, CE> = dyn Fn(DADatum<DA>, DADatum<DA>, &mut DA) -> Result<DA, CE>;

/// The type returned by "operative" and "applicative" functions.  For a
/// successful `Some` return, the returned `Datum` is substituted for the
/// original form by the parser in the AST it yields.  For a successful `None`
/// return, the original form is removed from the AST.  An
/// [`Error`](../enum.Error.html) is returned if the combiner fails for any
/// reason.
pub type Result<DA, CE>
    = core::result::Result<Option<DADatum<DA>>,
                           Error<<<DA as DatumAllocator>::TT as TextBase>::Pos, CE>>;

type DADatum<DA> = Datum<<DA as DatumAllocator>::TT,
                         <DA as DatumAllocator>::ET,
                         <DA as DatumAllocator>::DR>;
