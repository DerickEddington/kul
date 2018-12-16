//! Custom handling of dropping for the heap-allocated `Datum` types of this
//! crate.  Enables using very-deep `Datum` trees, e.g. long lists, which would
//! otherwise cause stack overflows when droppped (due to the compiler's
//! automatic recursive dropping).

use core::mem::replace;

use super::*;

/// Avoids extensive recursive dropping by using a loop and moving-out the
/// [`Datum`](../enum.Datum.html) values to iteratively drop them, instead of
/// relying on the default automatic recursive field dropping that would do
/// recursive drop calls which would overflow the stack for very-long reference
/// chains (e.g. long lists or very-deep nests).  Enables using very-long chains
/// of `Datum` references, which is essential for this crate to be robust.
///
/// This does a tree mutating restructuring algorithm to reach states where one
/// side of the branching becomes only one level deep, i.e. immediately ends in
/// a leaf node, which allows unlinking the top node from its branches which
/// then allows dropping it without causing recursion down into the branches.
/// This process repeats iteratively, as much as possible, with sub-nodes
/// becoming the next top node, to drop as many `Datum` nodes as possible,
/// without function-call recursion.
///
/// We move the `Datum`s themselves around, instead of the references to them,
/// because this avoids having to allocate new `Datum`s just to have the valid
/// reference type to temporarily replace the references with (which would
/// otherwise be needed due to [the generic
/// design](../trait.Parser.html#tymethod.new_datum) for allocating them).  The
/// `Datum`s aren't a lot bigger than the reference types, so moving a lot of
/// them isn't a huge difference (compared to moving a lot of references), and
/// the algorithm is basically the same either way.  We take advantage of a
/// leaf, i.e. non-branching, `Datum` variant for the temporary replacing.
///
/// For some types, like [`Rc`](http://doc.rust-lang.org/std/rc/struct.Rc.html)
/// and [`Arc`](http://doc.rust-lang.org/std/sync/struct.Arc.html), it's
/// possible that
/// [`DerefTryMut::get_mut`](../trait.DerefTryMut.html#tymethod.get_mut) fails,
/// e.g. because there are additional references (somewhere else), either strong
/// or weak, to a `Datum`, and so we can't do the mutation and so we abort.
/// This is fine when the additional references are all strong, i.e. none are
/// weak, because when there are only additional strong references then the
/// referenced `Datum` won't be dropped anyway when our reference to it is
/// dropped, and so drop-recursion down into it won't happen anyway.
///
/// However, weak references will also cause `get_mut` to fail even when we have
/// the only strong reference, and when we have the only strong reference it
/// will then be dropped recursively automatically after this function returns,
/// which could lead to extensive drop recursion and overflowing the stack if
/// the inner type does not implement a custom recursion-avoiding `drop` or if
/// there are extensive weak references to sub-nodes.
///
/// Also, there is a possible race condition with `Arc` where additional
/// reference(s), strong and/or weak, are all dropped by other thread(s) after
/// our attempted `get_mut` fails (and so we'll abort) but before our drop
/// finishes, and when it does finish we'll have the only strong reference and
/// so drop-recursion will occur into that `Datum`, which could lead to
/// extensive drop recursion and overflowing the stack if the inner type does
/// not implement a custom recursion-avoiding `drop` (or if there are extensive
/// weak references to sub-nodes).
///
/// Both of the above issues are similar in that a single strong reference to a
/// possibly-very-deep sub-tree is left unprocessed by our algorithm and so
/// could result in extensive drop recursion when the automatic field dropping
/// of the compiler is done immediately after this function.
///
/// A semi-solution to both of the above issues is to ensure that your `Datum`
/// reference type (or your `Datum` type, if it can be wrapped recursively)
/// implements a custom recursion-avoiding `drop`, e.g. by making it use this
/// function.  This can help avoid overflowing the stack when either of the
/// above issues occur because the automatic drop recursion will only go one or
/// two levels deep before the custom `drop` is called and hopefully avoids
/// further recursion.  However, extensive weak references to sub-nodes can
/// still prevent our algorithm from being able to avoid recursion and so stack
/// overflow is still possible in this case.
///
/// (At least this all is what we think this algorithm and solution do.  We
/// haven't formally proven any of it.  There could be bugs, or this approach
/// might be inadequate, and maybe some other approach should be used instead.)
pub fn drop_datum_algo1<'s, ET, DR>(top: &mut Datum<'s, ET, DR>)
    where DR: DerefTryMut<Target = Datum<'s, ET, DR>>
{
    use self::Datum::*;
    use self::Datum::EmptyList as TempLeaf;

    #[derive(PartialEq, Eq, Copy, Clone)]
    enum Class { Branch, Leaf }

    let class = |node: &DR| {
        match Deref::deref(node) {
            Combination{..} | List{..} => Class::Branch,
            _ => Class::Leaf
        }
    };

    #[derive(Copy, Clone)]
    enum Side { Left, Right }

    #[derive(Copy, Clone)]
    struct PathPattern {top: Side, sub: Side}

    // Mode for locking the tree-node path pattern chosen for restructuring the
    // tree, during each phase of iteration.  Needed to avoid possible infinite
    // loops on repeating states, which could otherwise occur because this
    // `drop` method can potentially use either side of branches to work with.
    // If the `get_mut`-ability of the nodes, which affects which branch sides
    // are used, has particular structures, then repeating states could occur.
    // E.g.: 1) left branch, right sub; 2) right branch (original top), left sub
    // (original sub); 3) becomes #1 again, repeats infinitely.  By locking the
    // mode, such repeated states are prevented, by allowing only one path
    // pattern to be used for restructuring the tree, until the current phase
    // has completed dropping one `Datum` node.  The mode is reset after each
    // "top" node is able to be dropped (a phase), so that all possible modes
    // can again be available to process the remainder of the tree, which allows
    // more flexibility (i.e. ability to mutate either side of branches) when
    // the `Datum` reference type is something like `Rc` or `Arc` where
    // `get_mut` might fail when there are additional external references to a
    // subset of nodes.
    let mut mode_lock: Option<PathPattern> = None;

    // let depth = |side, mut datum: &Datum<'s, ET, DR>| {
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
    // println!("drop_datum_algo1(left:{}, right:{})",
    //          depth(Side::Left, top), depth(Side::Right, top));

    loop {
        // println!("  loop(left:{}, right:{})",
        //          depth(Side::Left, top), depth(Side::Right, top));

        match top {
          // If top is a branch
          | Combination{operator: left, operands: right}
          | List{elem: left, next: right}
          => {
              let (left_class, right_class) = (class(left), class(right));

              // Try to choose a side that also branches, giving preference to
              // the left side (which is often shallower than the right for long
              // lists, which are the most common deep structure)
              let (mode_top, mut_branch, other_side)
                  = match (mode_lock.map(|ml| ml.top),
                           left_class,
                           DerefTryMut::get_mut(left),
                           right_class,
                           DerefTryMut::get_mut(right))
              {
                  // If the left node is a branch we can work with
                  | (None,             Class::Branch, Some(branch), _, _)
                  | (Some(Side::Left), Class::Branch, Some(branch), _, _)
                  => (Side::Left, branch, right_class),

                  // If the right node is a branch we can work with
                  | (None,              _, _, Class::Branch, Some(branch))
                  | (Some(Side::Right), _, _, Class::Branch, Some(branch))
                  => (Side::Right, branch, left_class),

                  // If the left node is a leaf while in left mode and the right
                  // node is a branch we can work with, we're at the last step
                  // of the mode phase
                  | (Some(Side::Left), Class::Leaf, _, Class::Branch, Some(branch))
                  => (Side::Left, branch, left_class),

                  // If the right node is a leaf while in right mode and the
                  // left node is a branch we can work with, we're at the last
                  // step of the mode phase
                  | (Some(Side::Right), Class::Branch, Some(branch), Class::Leaf, _)
                  => (Side::Right, branch, right_class),

                  // If neither left nor right is a branch, or if we can't work
                  // with any branch due to mode lock or no mutability, we're
                  // all done
                  | _ => break // Abort, do nothing more
              };

              // Unlink branch from top, moving it out to here
              let mut branch = replace(mut_branch, TempLeaf);

              // Restructure or drop, and then iterate on branch next
              match (&mut branch, other_side) {
                  // If both sides are branches, we restructure the tree
                  // mutatively to reduce the branching depth of one side
                  // (without dropping any of our `Datum` nodes yet)
                  | (Combination{operator: mut_left, operands: mut_right},
                     Class::Branch)
                  | (List{elem: mut_left, next: mut_right},
                     Class::Branch)
                  => {
                      // Try to choose a side, giving preference to the right
                      // side (which for long lists helps work our way to the
                      // often-shallower element leafs on the left sides, which
                      // is more efficient with this algorithm)
                      let (mode_sub, mut_sub)
                          = match (mode_lock.map(|ml| ml.sub),
                                   DerefTryMut::get_mut(mut_left),
                                   DerefTryMut::get_mut(mut_right))
                      {
                          | (None,              _, Some(datum))
                          | (Some(Side::Right), _, Some(datum))
                          => (Side::Right, datum),
                          | (None,             Some(datum), _)
                          | (Some(Side::Left), Some(datum), _)
                          => (Side::Left, datum),
                          | _ => break // Abort, do nothing more
                      };
                      // Note: this sequencing of borrow usage is necessary so
                      // the borrows are done being used by the time the values
                      // they refer to are themselves used next here.
                      *mut_branch = replace(mut_sub, TempLeaf);
                      *mut_sub = replace(top, TempLeaf);
                      *top = branch;
                      // If mode wasn't locked yet, lock it to the path pattern
                      // we were able to get, to prevent weird path pattern
                      // alternations that could cause infinite loops of
                      // repeating restructuring states
                      if mode_lock.is_none() {
                          mode_lock = Some(PathPattern{top: mode_top, sub: mode_sub});
                      }
                  },

                  // If the other side is a leaf, we can now drop top without
                  // drop-recursion down into the branch side (because branch is
                  // now unlinked from top).  Note: this will cause an
                  // additional, second, call of this method for the initial
                  // `self` when it is the top, and that's ok because recursion
                  // down into branches is no longer possible for it (because it
                  // has been mutated).
                  (_, Class::Leaf) => {
                      *top = branch;
                      // Reset the mode to unlocked, now that a node has been
                      // removed from the tree.  Weird repeating states aren't
                      // possible now (I think), so the next iteration is
                      // allowed to lock whatever path pattern it can.
                      mode_lock = None;
                  },

                  _ => unreachable!()
              }
          },
          // If top is a leaf
          | _ => break  // Stop, do nothing more
        };
    }
    // println!("  end(left:{}, right:{})",
    //          depth(Side::Left, top), depth(Side::Right, top));
}

/// Use [algorithm #1](drop/fn.drop_datum_algo1.html) for dropping, to avoid
/// extensive drop recursion.
impl<'s, ET> Drop for DatumBox<'s, ET> {
    fn drop(&mut self) {
        drop_datum_algo1(DerefMut::deref_mut(self));
    }
}

/// Use [algorithm #1](drop/fn.drop_datum_algo1.html) for dropping, to avoid
/// extensive drop recursion.
impl<'s, ET> Drop for DatumRc<'s, ET> {
    fn drop(&mut self) {
        match DerefTryMut::get_mut(self) {
            Some(dr) => drop_datum_algo1(dr),
            None => ()
        }
    }
}

/// Use [algorithm #1](drop/fn.drop_datum_algo1.html) for dropping, to avoid
/// extensive drop recursion.
impl<'s, ET> Drop for DatumArc<'s, ET> {
    fn drop(&mut self) {
        match DerefTryMut::get_mut(self) {
            Some(dr) => drop_datum_algo1(dr),
            None => ()
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    use kruvi_shared_tests::utils::*;

    // Pure lists (right-sided depth), pure nests (left-sided depth), and pure
    // zig-zags (alternating left-right depth) have the optimal shapes for our
    // drop algorithm and are the fastest at being dropped.

    fn list_len(size: usize) -> usize { (size - 1) / 2 }

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

    fn nest_depth(size: usize) -> usize { list_len(size) }

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

    fn zigzag_depth(size: usize) -> usize { list_len(size) }

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

    fn fan_depth(size: usize) -> usize {
        use std::usize::MAX;
        assert!(0 < size && size < MAX);
        let usize_width = MAX.count_ones();
        let log2floor = |n: usize| { (usize_width - 1) - n.leading_zeros() };
        (log2floor(size + 1) - 1) as usize
    }

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
        let left_size = size - 4;
        (nest_depth(left_size), 2)
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

    // Extensive weak references
    // These cause stack overflows because the weak references prevent our
    // algorithm from being able to avoid drop recursion.

    #[test]
    #[ignore]
    fn deep_rc_weak_list() {
        let len = list_len(get_arg_tree_size());
        let rcs = make_rc_weak_list(len);
        drop(rcs);
    }

    #[test]
    #[ignore]
    fn deep_arc_weak_list() {
        let len = list_len(get_arg_tree_size());
        let arcs = make_arc_weak_list(len);
        drop(arcs);
    }
}
