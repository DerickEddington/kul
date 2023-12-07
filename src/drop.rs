//! Custom handling of dropping for the heap-allocated `Datum` types of this
//! crate.  Enables using very-deep `Datum` trees, e.g. long lists, which would
//! otherwise cause stack overflows when droppped (due to the compiler's
//! automatic recursive dropping).  Can also be used for any other `Datum`
//! reference types of yours if they meet the requirements.

use std::ops::{Deref, DerefMut};
use std::mem::replace;
use std::rc::Rc;
use std::sync::Arc;

use crate::{Datum, DerefTryMut};
use crate::datum::{DatumBox, RcDatum, DatumRc, ArcDatum, DatumArc};

use Datum::{List, Combination};
use Datum::EmptyList as TempLeaf;


/// Avoids extensive recursive dropping by using a loop and moving-out the
/// [`Datum`](../enum.Datum.html) values to iteratively unlink and drop the
/// references to other `Datum`s they contain, instead of relying on the default
/// automatic recursive field dropping that would do recursive drop calls which
/// would overflow the stack for very-long reference chains (e.g. long lists or
/// very-deep nests).  Enables using very-long chains of `Datum` references,
/// which is essential for this crate to be robust.
///
/// This does a tree mutating restructuring algorithm to reach states where one
/// side of the branching becomes only one level deep, i.e. immediately ends in
/// a leaf node, which allows unlinking the top node from its branches which
/// then allows dropping its contained references without causing recursion down
/// into the branches.  We take advantage of a leaf, i.e. non-branching, `Datum`
/// variant for the temporary replacing.  This process repeats iteratively, as
/// much as possible, with sub-branches becoming the next top node, to drop as
/// many nodes as possible, without function-call recursion.
///
/// For example:
///```text
///      A
///    /   \
///   B     C
///  / \   / \
/// D   E F   G
///
///      B
///     / \
///    D   A
///       / \
///      E   C
///         / \
///        F   G
///
///   B       A
///  / \     / \
/// D   L   E   C
///            / \
///           F   G
///
///   A       C
///  / \     / \
/// E   L   F   G
///```
///
/// For some types used for the references, like
/// [`Rc`](http://doc.rust-lang.org/std/rc/struct.Rc.html) and
/// [`Arc`](http://doc.rust-lang.org/std/sync/struct.Arc.html), we use
/// [unwrapping](http://doc.rust-lang.org/std/rc/struct.Rc.html#method.try_unwrap)
/// to take out the inner `Datum` values so that any weak references are
/// invalidated and do not prevent us from doing the restructuring.  But
/// unwrapping will fail when there are additional strong references to a
/// `Datum`, and so we can't do the mutation and so we abort.  This is fine
/// because when there are additional strong references then the referenced
/// `Datum` won't be dropped anyway when our reference to it is dropped, and so
/// drop-recursion down into its branches won't happen anyway.
///
/// However, there is a possible race condition when `Arc` is used for the
/// reference type where additional strong references are all dropped by other
/// thread(s) after our attempted unwrapping fails (and so we'll abort) but
/// before our drop finishes, and then we'll be left with the only strong
/// reference and so the automatic drop-recursion will occur down into the
/// `Datum`'s branches.  But this is usually not a problem because the
/// drop-recursion will usually not go deep before our custom `Drop`
/// implementation (using our algorithm) is called again and probably avoids
/// further recursion (assuming the race condition does not happen frequently).
///
/// (At least, this all is what we think this approach does.  We haven't
/// formally proven it all.  There could be bugs, or this approach might be
/// inadequate, and maybe some other approach should be used instead.)
pub fn algo1<TT, ET, DR>(top_dr: &mut DR)
    where DR: Algo1DatumRef<Target = Datum<TT, ET, DR>>,
{
    #[derive(PartialEq, Eq, Copy, Clone)]
    enum Class { Branch, Leaf }

    #[derive(Copy, Clone)]
    enum Side { Left, Right }

    // Note: We're assumimg that these closures can be optimized and inlined.
    // If not, they should be made into functions that can be.  Closures allow a
    // more convenient definition (the type annotations in them are needed,
    // though).  (Functions would require redefining all the generic type
    // parameters and bounds.)

    let trytake = |node: &mut DR| -> Option<Datum<TT, ET, DR>> {
        Algo1DatumRef::try_replace(node, TempLeaf).ok()
    };

    let set = |node: &mut DR, val: Datum<TT, ET, DR>| {
        Algo1DatumRef::set(node, val);
    };

    let temp_branch = |left_dr: DR, right_dr: DR| -> Datum<TT, ET, DR> {
        List{elem: left_dr, next: right_dr}
    };

    let class = |node: &DR| {
        match &**node {
            Combination{..} | List{..} => Class::Branch,
            _ => Class::Leaf
        }
    };

    // Mode for locking the tree-node path pattern chosen for restructuring the
    // tree, during each phase of iteration.  Needed to avoid possible infinite
    // loops on repeating states, which could otherwise occur because this
    // algorithm can potentially use either side of the branches to work with.
    // If the unwrap-ability of the nodes, which affects which branch side is
    // selected, and for types like `Arc` can change dynamically, has particular
    // patterns, then repeating states could occur.
    //
    // E.g.:
    // 1) left branch (B), right sub (E);
    // 2) right branch (A) (original top), left sub (E) (sub of #1);
    // 3) becomes #1 again, could repeat infinitely.
    //
    // By locking the mode, such repeated states are prevented, by allowing only
    // one path pattern to be used for restructuring the tree, until the current
    // phase has completed.  The mode is reset after a phase, so that all
    // possible modes can again be available to process the remainder of the
    // tree, which allows flexibility (i.e. ability to mutate either branch)
    // when the `Datum` reference type uses something like `Rc` or `Arc` where
    // unwrapping might fail when there are additional strong references to a
    // subset of nodes.
    let mut mode_lock: Option<Side> = None;

    // let depth = |side, mut datum: &Datum<TT, ET, DR>| {
    //     let mut depth: usize = 0;
    //     loop {
    //         match (side, datum) {
    //             | (Side::Left, Combination{operator: side, ..})
    //             | (Side::Right, Combination{operands: side, ..})
    //             | (Side::Left, List{elem: side, ..})
    //             | (Side::Right, List{next: side, ..})
    //                 => {
    //                     depth += 1;
    //                     datum = Deref::deref(side);
    //                 }
    //             _ => break
    //         }
    //     }
    //     depth
    // };
    // println!("algo1(left:{}, right:{})",
    //          depth(Side::Left, &*top_dr), depth(Side::Right, &*top_dr));

    if let Class::Leaf = class(top_dr) {
        return; // Abort, do nothing
    }

    let Some(mut top_datum) = trytake(top_dr) else {
        return // Abort, do nothing
    };

    loop {
        // println!("  loop(left:{}, right:{})",
        //          depth(Side::Left, &top_datum), depth(Side::Right, &top_datum));

        match top_datum {
          | Combination{operator: mut left_dr, operands: mut right_dr}
          | List{elem: mut left_dr, next: mut right_dr}
          => {
              let (left_class, right_class) = (class(&left_dr), class(&right_dr));

              // Try to select a side that also branches.  When both branch and
              // we're not yet locked in a mode, give preference to the left
              // side (which is often shallower than the right for long lists,
              // which are the most common deep structure).  When locked in a
              // mode, we must go with its side, until we reach its end.
              let (mode, selected_datum, mut reuse_dr, other_class, other_dr)
                  = match (mode_lock, left_class, right_class)
              {
                  // If the left node is a branch and we're either at the end of
                  // a mode phase where the right node is a leaf, or we're
                  // locked in left mode, we must be able to take out the left
                  // datum to proceed.
                  | (_, Class::Branch, Class::Leaf)
                  | (Some(Side::Left), Class::Branch, Class::Branch)
                  =>
                      if let Some(left_datum) = trytake(&mut left_dr) {
                          (Side::Left, left_datum, left_dr, right_class, right_dr)
                      } else {
                          // Abort, do nothing more, drop `left_dr` and `right_dr`
                          break
                      },

                  // If the right node is a branch and we're either at the end
                  // of a mode phase where the left node is a leaf, or we're
                  // locked in right mode, we must be able to take out the right
                  // datum to proceed.
                  | (_, Class::Leaf, Class::Branch)
                  | (Some(Side::Right), Class::Branch, Class::Branch)
                  =>
                      if let Some(right_datum) = trytake(&mut right_dr) {
                          (Side::Right, right_datum, right_dr, left_class, left_dr)
                      } else {
                          // Abort, do nothing more, drop `left_dr` and `right_dr`
                          break
                      },

                  // If both nodes are branches and we're not locked in a mode,
                  // we must be able to take out one of the datums to proceed.
                  // Preference is given to the left datum, but the right can be
                  // used instead.
                  | (None, Class::Branch, Class::Branch)
                  =>
                      if let Some(left_datum) = trytake(&mut left_dr) {
                          (Side::Left, left_datum, left_dr, right_class, right_dr)
                      } else if let Some(right_datum) = trytake(&mut right_dr) {
                          (Side::Right, right_datum, right_dr, left_class, left_dr)
                      } else {
                          // Abort, do nothing more, drop `left_dr` and `right_dr`
                          break
                      },

                  // If neither left nor right is a branch, we're all done.
                  // Abort, do nothing more, drop `left_dr` and `right_dr`.
                  | (_, Class::Leaf, Class::Leaf)
                  => break
              };

              // Restructure or drop, and then iterate on branch next
              match (selected_datum, other_class) {
                  // If both sides are branches, we restructure the tree
                  // mutatively to reduce the branching depth of the selected
                  // side (without dropping any of our nodes yet)
                  (   Combination{operator: sel_left, operands: sel_right}
                    | List{elem: sel_left, next: sel_right},
                   Class::Branch)
                  => {
                      // Reuse `reuse_dr` and the `Datum` it refers to, for the
                      // different purpose of being a new link and new node in
                      // the restructured tree on the opposite side.  This is
                      // safe because `selected_datum` was already taken out of
                      // it.
                      let (oldsub, newsub_left, newsub_right) = match mode {
                          Side::Left => (sel_left, sel_right, other_dr),
                          Side::Right => (sel_right, other_dr, sel_left)
                      };
                      set(&mut reuse_dr, temp_branch(newsub_left, newsub_right));
                      // The new top node of the restructured tree for the next
                      // loop iteration, so it can be further restructured so
                      // nodes can eventually be dropped.
                      let (top_left, top_right) = match mode {
                          Side::Left => (oldsub, reuse_dr),
                          Side::Right => (reuse_dr, oldsub)
                      };
                      top_datum = temp_branch(top_left, top_right);
                      // If mode wasn't locked yet, lock it to the path pattern
                      // we were able to get, to prevent weird path pattern
                      // alternations that could cause infinite loops of
                      // repeating restructuring states
                      if mode_lock.is_none() {
                          mode_lock = Some(mode);
                      }
                  },

                  // If the other side is a leaf, we allow the automatic
                  // dropping of `left_dr` and `right_dr` (`reuse_dr` and
                  // `other_dr`) that will occur here, because drop-recursion
                  // down into the branch side is no longer possible (because it
                  // is no longer a branch now that `selected_datum` was taken
                  // out and replaced with a leaf).
                  (selected_datum, Class::Leaf) => {
                      // The new top node for the next loop iteration is the
                      // side that does branch, so it can be further
                      // restructured and dropped.
                      top_datum = selected_datum;
                      // Reset the mode to unlocked, now that the top node has
                      // been removed from the tree.  Weird repeating states
                      // aren't possible now (I think), so the next iteration is
                      // allowed to select whatever path pattern it can.
                      mode_lock = None;
                  },

                  _ => unreachable!()
              }
          },

          // The loop only iterates on values that are branches
          _ => unreachable!()
        };
    }
}

/// Exists so that `Datum`s can be moved-out of the reference types that refer
/// to them, even when there are other weak references.
pub trait Algo1DatumRef: DerefTryMut
    where <Self as Deref>::Target: Sized,
{
    /// Returns the inner `Datum` and replaces it with the given value, if
    /// possible.  Otherwise, an error is returned containing the passed-in
    /// value.  Some implementations might always succeed.
    fn try_replace(this: &mut Self, val: Self::Target)
                   -> Result<Self::Target, Self::Target>;

    /// Mutates the inner `Datum` to be the given value.  This must never fail.
    /// It is only called after our `try_replace` function is called on the same
    /// `this` value and succeeded, which allows this `set` function to never
    /// fail.
    fn set(this: &mut Self, val: Self::Target);
}

/// This allows using the custom drop algorithm
impl<TT, ET> Algo1DatumRef for DatumBox<TT, ET>
{
    fn try_replace(this: &mut Self, val: Self::Target)
                   -> Result<Self::Target, Self::Target> {
        Ok(replace(this, val))
    }

    fn set(this: &mut Self, val: Self::Target) {
        *DerefMut::deref_mut(this) = val;
    }
}

/// Use [algorithm #1](drop/fn.algo1.html) for dropping, to avoid
/// extensive drop recursion.
impl<TT, ET> Drop for DatumBox<TT, ET> {
    fn drop(&mut self) {
        algo1(self);
    }
}

/// Allows generically working with `Datum` reference types that wrap `Rc`-like
/// types, such as `Rc` and `Arc`.  Could also be used for other, non-standard,
/// types that are like `Rc`.
pub trait RcLike: DerefTryMut
    where <Self as Deref>::Target: Sized,
{
    /// The underlying `Rc`-like type
    type RC: Deref;

    /// Return a mutable reference to `this`'s underlying `Rc`-like value.
    fn get_rc(this: &mut Self) -> &mut Self::RC;

    /// Allocate a new `Rc`-like value and set its underlying value to the given
    /// `val`.
    fn new_rc(val: <Self as Deref>::Target) -> Self::RC;

    /// Do `try_unwrap` on the underlying `Rc`-like value.
    fn try_unwrap(rc: Self::RC) -> Result<<Self as Deref>::Target, Self::RC>;

    /// Implementation of `Algo1DatumRef::try_replace` for generic `Rc`-like
    /// types.  The default implementation uses `try_unwrap` and heap allocation
    /// which is inefficient.
    fn try_replace(this: &mut Self, val: <Self as Deref>::Target)
                   -> Result<<Self as Deref>::Target,
                             <Self as Deref>::Target>
    {
        // Must replace our wrapped `Rc`-like value to get ownership of it (both
        // because we only have a reference to our `this` value and because it
        // cannot be moved out because our `Self` type probably implements
        // `Drop`).  Unfortunately, we must allocate a new heap value to have a
        // replacement, but at least it is often reused by our drop algorithm,
        // and it is freed fairly soon (immediately if the `try_unwrap` here
        // fails).
        let old_rc = replace(Self::get_rc(this), Self::new_rc(val));
        match Self::try_unwrap(old_rc) {
            Ok(datum)
                => Ok(datum),
            Err(old_rc) => {
                // Put the old one back, deallocate the new one by unwrapping
                // (guaranteed to work because we have the only reference), and
                // return the passed-in value in the returned error.
                let new_rc = replace(Self::get_rc(this), old_rc);
                let Ok(val) = Self::try_unwrap(new_rc) else {
                    unreachable!()
                };
                Err(val)
            }
        }
    }

    /// Implementation of `Algo1DatumRef::set` for generic `Rc`-like types.
    /// The default implementation uses `DerefTryMut` and is safe to call after
    /// a success-return from `Algo1DatumRef::try_replace`.
    fn set(this: &mut Self, val: <Self as Deref>::Target) {
        // This `unwrap` won't fail because this function is only called after
        // our `try_replace` function succeeded and so our wrapped `Rc` cannot
        // have any other references (strong or weak) to its value.
        *DerefTryMut::get_mut(this).unwrap() = val;
    }
}

/// Allows generically working with `Datum` reference types that wrap `Rc`-like
/// types that can supply both strong and weak reference counts atomically.
/// Currently, this is only `Rc`, but hopefully in the future `Arc` can be
/// included as well if it can provide the ability to get both counts
/// atomically.  This could also be used for other, non-standard, types that
/// meet the requirements.
pub trait RcLikeAtomicCounts: RcLike
    where <Self as Deref>::Target: Sized,
{

    /// Atomically gets the number of strong and weak pointers to the underlying
    /// value.  I.e. gets a snapshot of the state of both the strong and weak
    /// counts at the same logical instant.  In particular, when strong=1 and
    /// weak=0 this means either count cannot change after this function returns
    /// except via use of the only strong reference, and if you control that
    /// strong reference and do not change the counts, then you can depend on
    /// being able to get mutable access (e.g. via `get_mut`) to the underlying
    /// value, guaranteed.
    fn counts(this: &Self) -> (usize, usize);

    /// Optimized implementation of `Algo1DatumRef::try_replace` for generic
    /// `Rc`-like types that can supply both strong and weak reference counts
    /// atomically.  The default implementation uses `DerefTryMut` when possible
    /// to avoid the heap allocation and unwrapping that the basic default
    /// `RcLike::try_replace` does.
    fn try_replace_optim(this: &mut Self, val: <Self as Deref>::Target)
                         -> Result<<Self as Deref>::Target,
                                   <Self as Deref>::Target>
    {
        let (strong_count, weak_count) = Self::counts(this);
        if strong_count == 1 {
            if weak_count == 0 {
                // In this case, we can optimize by getting direct mutable
                // access, guaranteed, to the underlying value.  This `unwrap`
                // won't fail because we know the underlying `get_mut` will
                // succeed because of the state of the reference counts.
                Ok(replace(DerefTryMut::get_mut(this).unwrap(), val))
            } else {
                // In this case, we cannot optimize and must use the basic
                // implementation.
                Self::try_replace(this, val)
            }
        } else {
            // Strong count was not 1.  Just fail immediately, because the basic
            // implementation would likely fail (given the strong count).  For
            // non-`Send` types like `Rc`, it would definitely fail, but for
            // multi-thread types like `Arc` it could possibly succeed now due
            // to the strong count changing to 1 after we checked it above, but
            // we don't try to handle that because it's ok to allow our drop
            // algorithm to abort on our `this` value and allow drop recursion
            // to occur for it because our algorithm will likely be called again
            // on any sub nodes and thus avoid further extensive recursion.
            Err(val)
        }
    }
}

impl<TT, ET> RcLike for DatumRc<TT, ET>
{
    type RC = Rc<RcDatum<TT, ET>>;

    fn get_rc(this: &mut Self) -> &mut Self::RC {
        &mut this.0
    }

    fn new_rc(val: <Self as Deref>::Target) -> Self::RC {
        Rc::new(val)
    }

    fn try_unwrap(rc: Self::RC) -> Result<<Self as Deref>::Target, Self::RC> {
        Rc::try_unwrap(rc)
    }
}

impl<TT, ET> RcLikeAtomicCounts for DatumRc<TT, ET> {
    /// This is atomic enough to meet the requirements, because `Rc` is
    /// single-threaded.
    fn counts(this: &Self) -> (usize, usize) {
        (Rc::strong_count(&this.0), Rc::weak_count(&this.0))
    }
}

/// This allows using the custom drop algorithm, and it allows the algorithm to
/// restructure tree nodes that have other weak references to them (which isn't
/// possible with `DerefTryMut::get_mut` alone).
impl<TT, ET> Algo1DatumRef for DatumRc<TT, ET>
{
    fn try_replace(this: &mut Self, val: Self::Target)
                   -> Result<Self::Target, Self::Target> {
        RcLikeAtomicCounts::try_replace_optim(this, val)
    }

    fn set(this: &mut Self, val: Self::Target) {
        RcLike::set(this, val);
    }
}

/// Use [algorithm #1](drop/fn.algo1.html) for dropping, to avoid
/// extensive drop recursion.
impl<TT, ET> Drop for DatumRc<TT, ET> {
    fn drop(&mut self) {
        algo1(self);
    }
}

impl<TT, ET> RcLike for DatumArc<TT, ET>
{
    type RC = Arc<ArcDatum<TT, ET>>;

    fn get_rc(this: &mut Self) -> &mut Self::RC {
        &mut this.0
    }

    fn new_rc(val: <Self as Deref>::Target) -> Self::RC {
        Arc::new(val)
    }

    fn try_unwrap(rc: Self::RC) -> Result<<Self as Deref>::Target, Self::RC> {
        Arc::try_unwrap(rc)
    }
}

/// This allows using the custom drop algorithm, and it allows the algorithm to
/// restructure tree nodes that have other weak references to them (which isn't
/// possible with `DerefTryMut::get_mut` alone).
impl<TT, ET> Algo1DatumRef for DatumArc<TT, ET>
{
    // This is not optimized like `DatumRc`'s `try_replace` because `Arc` lacks
    // the ability to atomically get the strong and weak counts.  If `Arc` is
    // ever enhanced in the future to provide that, this could be changed to use
    // `RcLikeAtomicCounts::try_replace_optim`.
    fn try_replace(this: &mut Self, val: Self::Target)
                   -> Result<Self::Target, Self::Target> {
        RcLike::try_replace(this, val)
    }

    fn set(this: &mut Self, val: Self::Target) {
        RcLike::set(this, val);
    }
}

/// Use [algorithm #1](drop/fn.algo1.html) for dropping, to avoid
/// extensive drop recursion.
impl<TT, ET> Drop for DatumArc<TT, ET> {
    fn drop(&mut self) {
        algo1(self);
    }
}


#[cfg(test)]
mod tests {
    use kul_shared_tests::utils::tree_shapes::*;

    // Pure lists (right-sided depth), pure nests (left-sided depth), and pure
    // zig-zags (alternating left-right depth) have the optimal shapes for our
    // drop algorithm and are the fastest at being dropped.

    #[test]
    fn deep_box_list() {
        let len = list_len(get_arg_tree_size());
        let boxes = make_box_list(len);
        drop(boxes);
    }

    #[test]
    fn deep_rc_list() {
        let len = list_len(get_arg_tree_size());
        let rcs = make_rc_list(len);
        drop(rcs);
    }

    #[test]
    fn deep_arc_list() {
        let len = list_len(get_arg_tree_size());
        let arcs = make_arc_list(len);
        drop(arcs);
    }

    #[test]
    fn deep_box_nest() {
        let depth = nest_depth(get_arg_tree_size());
        let boxes = make_box_nest(depth);
        drop(boxes);
    }

    #[test]
    fn deep_rc_nest() {
        let depth = nest_depth(get_arg_tree_size());
        let rcs = make_rc_nest(depth);
        drop(rcs);
    }

    #[test]
    fn deep_arc_nest() {
        let depth = nest_depth(get_arg_tree_size());
        let arcs = make_arc_nest(depth);
        drop(arcs);
    }

    #[test]
    fn deep_box_zigzag() {
        let depth = zigzag_depth(get_arg_tree_size());
        let boxes = make_box_zigzag(depth);
        drop(boxes);
    }

    #[test]
    fn deep_rc_zigzag() {
        let depth = zigzag_depth(get_arg_tree_size());
        let rcs = make_rc_zigzag(depth);
        drop(rcs);
    }

    #[test]
    fn deep_arc_zigzag() {
        let depth = zigzag_depth(get_arg_tree_size());
        let arcs = make_arc_zigzag(depth);
        drop(arcs);
    }

    // Fans (branching and depth at every node, equal at every level, the
    // maximum size (amount of nodes) per depth), are a suboptimal shape, but
    // would have to be too huge to have enough depth to overflow the stack
    // without our recursion-avoiding Drop impl, but this still exercises our
    // drop algorithm in a unique suboptimal way.

    #[test]
    fn deep_box_fan() {
        let depth = fan_depth(get_arg_tree_size());
        let boxes = make_box_fan(depth);
        drop(boxes);
    }

    #[test]
    fn deep_rc_fan() {
        let depth = fan_depth(get_arg_tree_size());
        let rcs = make_rc_fan(depth);
        drop(rcs);
    }

    #[test]
    fn deep_arc_fan() {
        let depth = fan_depth(get_arg_tree_size());
        let arcs = make_arc_fan(depth);
        drop(arcs);
    }

    // Vees (shaped like a "V", both right-sided and left-sided depth, and no
    // middle depth), with a short right-side depth of at least two, are a
    // suboptimal shape, because our algorithm prefers restructuring the left
    // side when the right has any depth, but for this shape it would be more
    // efficient to restructure the right side, but our algorithm does not do
    // that because it does not look at the branching depth beyond the first
    // level, to see which side might be significantly shorter, because that
    // would be too inefficient in general.

    fn vee2r_depths(size: usize) -> (usize, usize) {
        let right_size = 2 + 1; // list of length 1
        let left_size = size - (1 + right_size); // extra 1 for top node
        vee_depths(left_size, right_size)
    }

    #[test]
    fn deep_box_vee2r() {
        let (left_depth, right_depth) = vee2r_depths(get_arg_tree_size());
        let boxes = make_box_vee(left_depth, right_depth);
        drop(boxes);
    }

    #[test]
    fn deep_rc_vee2r() {
        let (left_depth, right_depth) = vee2r_depths(get_arg_tree_size());
        let rcs = make_rc_vee(left_depth, right_depth);
        drop(rcs);
    }

    #[test]
    fn deep_arc_vee2r() {
        let (left_depth, right_depth) = vee2r_depths(get_arg_tree_size());
        let arcs = make_arc_vee(left_depth, right_depth);
        drop(arcs);
    }

    // Extensive weak references.  Requires special logic to allow our algorithm
    // to work in their presence.

    #[test]
    fn deep_rc_weak_list() {
        let len = list_len(get_arg_tree_size());
        let rcs = make_rc_weak_list(len);
        drop(rcs);
    }

    #[test]
    fn deep_arc_weak_list() {
        let len = list_len(get_arg_tree_size());
        let arcs = make_arc_weak_list(len);
        drop(arcs);
    }

    // Additional strong references.  Does not cause stack overflows because
    // drop recursion does not go down into the branches of a `Datum` node,
    // because the `Datum` is not dropped yet, when there is more than one
    // strong reference to it.  When the strong count reaches 1, the dropping
    // will be handled by our algorithm that avoids drop recursion.

    #[test]
    fn deep_rc_multi_strong_list() {
        let len = list_len(get_arg_tree_size());
        // Strong refs to every "next" tail node
        let strong_step = 1;
        let (rcs, _strong_refs) = make_rc_multi_strong_list(len, strong_step);
        // Only drops the top node because the others have additional strong
        // references to them.
        drop(rcs);
        // Rest of the dropping, using our algorithm, occurs when `_strong_refs`
        // is dropped here.
    }

    #[test]
    fn deep_arc_multi_strong_list() {
        let len = list_len(get_arg_tree_size());
        // Strong refs to every "next" tail node
        let strong_step = 1;
        let (arcs, _strong_refs) = make_arc_multi_strong_list(len, strong_step);
        // Only drops the top node because the others have additional strong
        // references to them.
        drop(arcs);
        // Rest of the dropping, using our algorithm, occurs when `_strong_refs`
        // is dropped here.
    }
}
