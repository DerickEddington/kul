//! Utilities for constructing `Datum` trees of various shapes.

use std::env;
use std::rc::{Rc, Weak as WeakRc};
use std::sync::{Arc, Weak as WeakArc};
use std::cell::Cell;

use kruvi::{DatumBox, DatumRc, RcDatum, DatumArc, ArcDatum};
use kruvi_core::Datum::*;
use super::*;


/// This allows passing a `tree-size=$SIZE` command-line argument to the tests
pub fn get_arg_tree_size() -> usize {
    // This default size is usually enough to blow the stack, without our
    // recursion-avoiding Drop impl
    const DEFAULT: usize = 1 << 24;
    let size = env::args().find_map(
        |arg|
        match *arg.splitn(2, '=').collect::<Vec<_>>() {
            ["tree-size", size] => Some(size.parse().unwrap()),
            _ => None
        }
    ).unwrap_or(DEFAULT);
    // println!("\nTrees of size {}", size);
    size
}

/// Pure lists
pub fn make_list<N, E, DR>(len: usize, new: &mut N, e: E) -> DR
    where N: FnMut(Datum<'static, usize, DR>) -> DR,
          E: Fn(usize, &mut N) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    let mut cnt: usize = 1;
    let mut d = new(EmptyList);
    while cnt <= len {
        let elem = e(cnt, new);
        d = new(List{elem, next: d});
        cnt += 1;
    }
    d
}

pub fn list_len(size: usize) -> usize {
    (size - 1) / 2
}

pub fn make_basic_list<N, DR>(len: usize, new: &mut N) -> DR
    where N: FnMut(Datum<'static, usize, DR>) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    make_list(len, new, |cnt, new| new(Extra(cnt)))
}

pub fn make_box_list(len: usize) -> DatumBox<'static, usize> {
    make_basic_list(len, &mut DatumBox::new)
}

pub fn make_rc_list(len: usize) -> DatumRc<'static, usize> {
    make_basic_list(len, &mut DatumRc::new)
}

pub fn make_arc_list(len: usize) -> DatumArc<'static, usize> {
    make_basic_list(len, &mut DatumArc::new)
}

/// Pure nests
pub fn make_nest<N, O, DR>(depth: usize, new: &mut N, o: O) -> DR
    where N: FnMut(Datum<'static, usize, DR>) -> DR,
          O: Fn(usize, &mut N) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    let mut cnt: usize = 1;
    let mut d = new(EmptyNest);
    while cnt <= depth {
        let operands = o(cnt, new);
        d = new(Combination{operator: d, operands});
        cnt += 1;
    }
    d
}

pub fn nest_depth(size: usize) -> usize {
    list_len(size)
}

pub fn make_basic_nest<N, DR>(depth: usize, new: &mut N) -> DR
    where N: FnMut(Datum<'static, usize, DR>) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    make_nest(depth, new, |_, new| new(EmptyList))
}

pub fn make_box_nest(depth: usize) -> DatumBox<'static, usize> {
    make_basic_nest(depth, &mut DatumBox::new)
}

pub fn make_rc_nest(depth: usize) -> DatumRc<'static, usize> {
    make_basic_nest(depth, &mut DatumRc::new)
}

pub fn make_arc_nest(depth: usize) -> DatumArc<'static, usize> {
    make_basic_nest(depth, &mut DatumArc::new)
}

/// Zig-zags
pub fn make_zigzag<N, EM, OD, EV, L, R, DR>
    (depth: usize, new: &N, empty: EM, odd: OD, even: EV, left: L, right: R)
     -> DR
    where N: Fn(Datum<'static, usize, DR>) -> DR,
          EM: Fn() -> Datum<'static, usize, DR>,
          OD: Fn(DR, DR) -> Datum<'static, usize, DR>,
          EV: Fn(DR, DR) -> Datum<'static, usize, DR>,
          L: Fn(usize, &N) -> DR,
          R: Fn(usize, &N) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    let mut cnt: usize = 1;
    let mut d = new(empty());
    while cnt <= depth {
        d = new(odd(d, right(cnt, &new)));
        cnt += 1;
        if cnt <= depth {
            d = new(even(left(cnt, &new), d));
            cnt += 1;
        }
    }
    d
}

pub fn zigzag_depth(size: usize) -> usize {
    list_len(size)
}

pub fn make_basic_zigzag<N, DR>(depth: usize, new: &N) -> DR
    where N: Fn(Datum<'static, usize, DR>) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    make_zigzag(depth, new,
                || EmptyList,
                |operator, operands| Combination{operator, operands},
                |elem, next| List{elem, next},
                |cnt, new| new(Extra(cnt)),
                |_, new| new(EmptyList))
}

pub fn make_box_zigzag(depth: usize) -> DatumBox<'static, usize> {
    make_basic_zigzag(depth, &DatumBox::new)
}

pub fn make_rc_zigzag(depth: usize) -> DatumRc<'static, usize> {
    make_basic_zigzag(depth, &DatumRc::new)
}

pub fn make_arc_zigzag(depth: usize) -> DatumArc<'static, usize> {
    make_basic_zigzag(depth, &DatumArc::new)
}

/// Maximum fans
pub fn make_fan<N, DR>(depth: usize, new: &mut N) -> DR
    where N: FnMut(Datum<'static, usize, DR>) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    make_nest(depth, new,
              |cnt, new|
              make_list(cnt - 1, new,
                        |cnt, new| make_fan(cnt - 1, new)))
}

pub fn fan_depth(size: usize) -> usize {
    use std::usize::MAX;
    assert!(0 < size && size < MAX);
    let usize_width = MAX.count_ones();
    let log2floor = |n: usize| { (usize_width - 1) - n.leading_zeros() };
    (log2floor(size + 1) - 1) as usize
}

pub fn make_box_fan(depth: usize) -> DatumBox<'static, usize> {
    make_fan(depth, &mut DatumBox::new)
}

pub fn make_rc_fan(depth: usize) -> DatumRc<'static, usize> {
    make_fan(depth, &mut DatumRc::new)
}

pub fn make_arc_fan(depth: usize) -> DatumArc<'static, usize> {
    make_fan(depth, &mut DatumArc::new)
}

/// "V"s
pub fn make_vee<N, DR>(left_depth: usize, right_depth: usize, mut new: &N) -> DR
    where N: Fn(Datum<'static, usize, DR>) -> DR,
          DR: DerefTryMut<Target = Datum<'static, usize, DR>>,
{
    let left = if left_depth > 0 {
        Some(make_basic_nest(left_depth - 1, &mut new))
    } else {
        None
    };
    let right = if right_depth > 0 {
        Some(make_basic_list(right_depth - 1, &mut new))
    } else {
        None
    };
    if left.is_some() || right.is_some() {
        new(Combination{operator: left.unwrap_or_else(|| new(EmptyNest)),
                        operands: right.unwrap_or_else(|| new(EmptyList))})
    } else {
        new(EmptyNest)
    }
}

pub fn vee_depths(left_size: usize, right_size: usize) -> (usize, usize) {
    (nest_depth(left_size) + 1, list_len(right_size) + 1)
}

pub fn make_box_vee(left_depth: usize, right_depth: usize) -> DatumBox<'static, usize> {
    make_vee(left_depth, right_depth, &DatumBox::new)
}

pub fn make_rc_vee(left_depth: usize, right_depth: usize) -> DatumRc<'static, usize> {
    make_vee(left_depth, right_depth, &DatumRc::new)
}

pub fn make_arc_vee(left_depth: usize, right_depth: usize) -> DatumArc<'static, usize> {
    make_vee(left_depth, right_depth, &DatumArc::new)
}

/// `Rc` lists with elements as weak refs to their parents
pub fn make_rc_weak_list(len: usize) -> DatumRc<'static, ExtraWeakRc> {
    let mut cnt: usize = 1;
    let mut d = DatumRc::new(EmptyList);
    while cnt <= len {
        d = DatumRc::new(List{elem: DatumRc::new(Extra(ExtraWeakRc(Cell::new(None)))),
                              next: d});
        match &*d {
            List{elem, ..} => match &**elem {
                Extra(ExtraWeakRc(c)) => c.set(Some(Rc::downgrade(&d.0))),
                _ => unreachable!()
            },
            _ => unreachable!()
        }
        cnt += 1;
    }
    d
}

/// Allows the needed recursive type definition
pub struct ExtraWeakRc (Cell<Option<WeakRc<RcDatum<'static, ExtraWeakRc>>>>);

/// `Arc` lists with elements as weak refs to their parents
pub fn make_arc_weak_list(len: usize) -> DatumArc<'static, ExtraWeakArc> {
    let mut cnt: usize = 1;
    let mut d = DatumArc::new(EmptyList);
    while cnt <= len {
        d = DatumArc::new(List{elem: DatumArc::new(Extra(ExtraWeakArc(Cell::new(None)))),
                               next: d});
        match &*d {
            List{elem, ..} => match &**elem {
                Extra(ExtraWeakArc(c)) => c.set(Some(Arc::downgrade(&d.0))),
                _ => unreachable!()
            },
            _ => unreachable!()
        }
        cnt += 1;
    }
    d
}

/// Allows the needed recursive type definition
pub struct ExtraWeakArc (Cell<Option<WeakArc<ArcDatum<'static, ExtraWeakArc>>>>);

/// `Rc` lists with additional strong references to some of the "next" tails
pub fn make_rc_multi_strong_list(len: usize, strong_step: usize)
                                 -> (DatumRc<'static, usize>,
                                     Vec<Rc<RcDatum<'static, usize>>>)
{
    let mut v = Vec::with_capacity(len / strong_step);
    let mut counter: usize = 0;
    let l = make_basic_list(len,
                            &mut |d| {
                                let drc = DatumRc::new(d);
                                if let List{..} = &*drc {
                                    counter += 1;
                                    if counter != len // final head not cloned
                                        && counter % strong_step == 0
                                    {
                                        v.push(Rc::clone(&drc.0));
                                    }
                                }
                                drc
                            });
    (l, v)
}

/// `Arc` lists with additional strong references to some of the "next" tails
pub fn make_arc_multi_strong_list(len: usize, strong_step: usize)
                                  -> (DatumArc<'static, usize>,
                                      Vec<Arc<ArcDatum<'static, usize>>>)
{
    let mut v = Vec::with_capacity(len / strong_step);
    let mut counter: usize = 0;
    let l = make_basic_list(len,
                            &mut |d| {
                                let darc = DatumArc::new(d);
                                if let List{..} = &*darc {
                                    counter += 1;
                                    if counter != len // final head not cloned
                                        && counter % strong_step == 0
                                    {
                                        v.push(Arc::clone(&darc.0));
                                    }
                                }
                                darc
                            });
    (l, v)
}


// This only tests the internal units of this module.  We need to make sure that
// these constructors make the shapes we want, before depending on them for
// other tests.
#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_meta {
        (($args:tt) (=> $($make:ident),+) ($expected:expr))
            =>
        {$(assert_eq!(ExpectedDatum($expected), *($make $args)));+};
    }

    fn e() -> ExpectedDatumRef { dr(Extra(EtIgnore)) }

    #[test]
    fn list() {
        macro_rules! test {
            ($depth:expr => $expected:expr)
                =>
            {test_meta!((($depth))
                        (=> make_box_list, make_rc_list, make_arc_list)
                        ($expected))};
        }
        test!(0 => EmptyList);
        test!(1 => List{elem: e(), next: dr(EmptyList)});
        test!(2 => List{elem: e(),
                        next: dr(List{elem: e(),
                                      next: dr(EmptyList)})});
        test!(3 => List{elem: e(),
                        next: dr(List{elem: e(),
                                      next: dr(List{elem: e(),
                                                    next: dr(EmptyList)})})});
        test!(4 =>
              List{elem: e(),
                   next: dr(List{elem: e(),
                                 next: dr(List{elem: e(),
                                               next: dr(List{elem: e(),
                                                             next: dr(EmptyList)})})})});
        test!(5 =>
              List{elem: e(),
                   next: dr(List{elem: e(),
                                 next: dr(List{elem: e(),
                                               next:
                                               dr(List{elem: e(),
                                                       next:
                                                       dr(List{elem: e(),
                                                               next:
                                                               dr(EmptyList)})})})})});
    }

    #[test]
    fn nest() {
        macro_rules! test {
            ($depth:expr => $expected:expr)
                =>
            {test_meta!((($depth))
                        (=> make_box_nest, make_rc_nest, make_arc_nest)
                        ($expected))};
        }
        test!(0 => EmptyNest);
        test!(1 => Combination{operator: dr(EmptyNest), operands: dr(EmptyList)});
        test!(2 => Combination{operator: dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                               operands: dr(EmptyList)});
        test!(3 =>
              Combination{operator:
                          dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                         operands: dr(EmptyList)}),
                          operands: dr(EmptyList)});
        test!(4 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands: dr(EmptyList)});
        test!(5 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands: dr(EmptyList)});
    }

    #[test]
    fn zigzag() {
        macro_rules! test {
            ($depth:expr => $expected:expr)
                =>
            {test_meta!((($depth))
                        (=> make_box_zigzag, make_rc_zigzag, make_arc_zigzag)
                        ($expected))};
        }
        test!(0 => EmptyList);
        test!(1 => Combination{operator: dr(EmptyList), operands: dr(EmptyList)});
        test!(2 =>
              List{elem: e(),
                   next: dr(Combination{operator: dr(EmptyList),
                                        operands: dr(EmptyList)})});
        test!(3 =>
              Combination{operator:
                          dr(List{elem: e(),
                                  next: dr(Combination{operator: dr(EmptyList),
                                                       operands: dr(EmptyList)})}),
                          operands: dr(EmptyList)});
        test!(4 =>
              List{elem: e(),
                   next:
                   dr(Combination{operator:
                          dr(List{elem: e(),
                                  next: dr(Combination{operator: dr(EmptyList),
                                                       operands: dr(EmptyList)})}),
                                  operands: dr(EmptyList)})});
        test!(5 =>
              Combination{operator:
                  dr(List{elem: e(),
                          next:
                          dr(Combination{operator:
                                 dr(List{elem: e(),
                                         next: dr(Combination{operator: dr(EmptyList),
                                                              operands:
                                                              dr(EmptyList)})}),
                                         operands: dr(EmptyList)})}),
                          operands: dr(EmptyList)});
    }

    #[test]
    fn fan() {
        macro_rules! test {
            ($depth:expr => $expected:expr)
                =>
            {test_meta!((($depth))
                        (=> make_box_fan, make_rc_fan, make_arc_fan)
                        ($expected))};
        }
        test!(0 => EmptyNest);
        test!(1 => Combination{operator: dr(EmptyNest), operands: dr(EmptyList)});
        test!(2 => Combination{operator: dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                               operands: dr(List{elem: dr(EmptyNest),
                                                 next: dr(EmptyList)})});
        test!(3 =>
              Combination{operator:
                  dr(Combination{operator:
                                 dr(Combination{operator: dr(EmptyNest),
                                                operands: dr(EmptyList)}),
                                 operands: dr(List{elem: dr(EmptyNest),
                                                   next: dr(EmptyList)})}),
                          operands:
                  dr(List{elem: dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                          next: dr(List{elem: dr(EmptyNest),
                                        next: dr(EmptyList)})})});
        test!(4 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(List{elem: dr(EmptyNest),
                                                          next: dr(EmptyList)})}),
                                 operands:
                                 dr(List{elem: dr(Combination{operator: dr(EmptyNest),
                                                              operands: dr(EmptyList)}),
                                         next: dr(List{elem: dr(EmptyNest),
                                                       next: dr(EmptyList)})})}),
                          operands:
                  dr(List{elem: dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands:
                                       dr(List{elem: dr(EmptyNest),
                                               next: dr(EmptyList)})}),
                          next: dr(List{elem: dr(Combination{operator: dr(EmptyNest),
                                                             operands: dr(EmptyList)}),
                                        next: dr(List{elem: dr(EmptyNest),
                                                      next: dr(EmptyList)})})})});
        test!(5 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands:
                                       dr(List{elem: dr(EmptyNest),
                                               next: dr(EmptyList)})}),
                                        operands:
                                dr(List{elem: dr(Combination{operator: dr(EmptyNest),
                                                             operands: dr(EmptyList)}),
                                        next: dr(List{elem: dr(EmptyNest),
                                                      next: dr(EmptyList)})})}),
                                 operands:
                         dr(List{elem: dr(Combination{operator:
                                              dr(Combination{operator: dr(EmptyNest),
                                                             operands: dr(EmptyList)}),
                                                      operands:
                                              dr(List{elem: dr(EmptyNest),
                                                      next: dr(EmptyList)})}),
                                 next: dr(List{elem: dr(Combination{operator:
                                                                    dr(EmptyNest),
                                                                    operands:
                                                                    dr(EmptyList)}),
                                               next: dr(List{elem: dr(EmptyNest),
                                                             next: dr(EmptyList)})})})}),
                          operands:
                  dr(List{elem:
                          dr(Combination{operator:
                                 dr(Combination{operator:
                                                dr(Combination{operator: dr(EmptyNest),
                                                               operands: dr(EmptyList)}),
                                                operands: dr(List{elem: dr(EmptyNest),
                                                                  next:
                                                                  dr(EmptyList)})}),
                                         operands:
                                 dr(List{elem: dr(Combination{operator: dr(EmptyNest),
                                                              operands: dr(EmptyList)}),
                                         next: dr(List{elem: dr(EmptyNest),
                                                       next: dr(EmptyList)})})}),
                          next:
                          dr(List{elem:
                                  dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                                 operands:
                                         dr(List{elem: dr(EmptyNest),
                                                 next: dr(EmptyList)})}),
                                  next:
                                  dr(List{elem: dr(Combination{operator:
                                                               dr(EmptyNest),
                                                               operands:
                                                               dr(EmptyList)}),
                                          next: dr(List{elem: dr(EmptyNest),
                                                        next: dr(EmptyList)})})})})});
    }

    #[test]
    fn vee() {
        macro_rules! test {
            ($left_depth:expr, $right_depth:expr => $expected:expr)
                =>
            {test_meta!((($left_depth, $right_depth))
                        (=> make_box_vee, make_rc_vee, make_arc_vee)
                        ($expected))};
        }
        test!(0, 0 => EmptyNest);
        test!(0, 1 => Combination{operator: dr(EmptyNest), operands: dr(EmptyList)});
        test!(0, 2 => Combination{operator: dr(EmptyNest),
                                  operands: dr(List{elem: e(),
                                                    next: dr(EmptyList)})});
        test!(1, 0 => Combination{operator: dr(EmptyNest), operands: dr(EmptyList)});
        test!(1, 1 => Combination{operator: dr(EmptyNest), operands: dr(EmptyList)});
        test!(1, 2 => Combination{operator: dr(EmptyNest),
                                  operands: dr(List{elem: e(),
                                                    next: dr(EmptyList)})});
        test!(1, 3 => Combination{operator: dr(EmptyNest),
                                  operands:
                                  dr(List{elem: e(),
                                          next: dr(List{elem: e(),
                                                        next: dr(EmptyList)})})});
        test!(2, 0 => Combination{operator: dr(Combination{operator: dr(EmptyNest),
                                                           operands: dr(EmptyList)}),
                                  operands: dr(EmptyList)});
        test!(2, 1 => Combination{operator: dr(Combination{operator: dr(EmptyNest),
                                                           operands: dr(EmptyList)}),
                                  operands: dr(EmptyList)});
        test!(2, 2 => Combination{operator: dr(Combination{operator: dr(EmptyNest),
                                                           operands: dr(EmptyList)}),
                                  operands: dr(List{elem: e(),
                                                    next: dr(EmptyList)})});
        test!(2, 4 => Combination{operator: dr(Combination{operator: dr(EmptyNest),
                                                           operands: dr(EmptyList)}),
                                  operands:
                          dr(List{elem: e(),
                                  next:
                                  dr(List{elem: e(),
                                          next:
                                          dr(List{elem: e(),
                                                  next: dr(EmptyList)})})})});
        test!(3, 0 =>
              Combination{operator:
                          dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                         operands: dr(EmptyList)}),
                          operands: dr(EmptyList)});
        test!(3, 1 =>
              Combination{operator:
                          dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                         operands: dr(EmptyList)}),
                          operands: dr(EmptyList)});
        test!(3, 2 =>
              Combination{operator:
                          dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                         operands: dr(EmptyList)}),
                          operands: dr(List{elem: e(),
                                            next: dr(EmptyList)})});
        test!(3, 3 =>
              Combination{operator:
                          dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                         operands: dr(EmptyList)}),
                          operands: dr(List{elem: e(),
                                            next: dr(List{elem: e(),
                                                          next: dr(EmptyList)})})});
        test!(3, 5 =>
              Combination{operator:
                          dr(Combination{operator:
                                         dr(Combination{operator: dr(EmptyNest),
                                                        operands: dr(EmptyList)}),
                                         operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(List{elem: e(),
                                        next:
                                        dr(List{elem: e(),
                                                next:
                                                dr(List{elem: e(),
                                                        next: dr(EmptyList)})})})})});
        test!(4, 0 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(EmptyList)});
        test!(4, 1 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(EmptyList)});
        test!(4, 2 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(EmptyList)})});
        test!(4, 3 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(List{elem: e(),
                                        next: dr(EmptyList)})})});
        test!(4, 4 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator: dr(EmptyNest),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(List{elem: e(),
                                        next: dr(List{elem: e(),
                                                      next: dr(EmptyList)})})})});
        test!(5, 0 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(EmptyList)});
        test!(5, 1 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(EmptyList)});
        test!(5, 2 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(EmptyList)})});
        test!(5, 3 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(List{elem: e(),
                                        next: dr(EmptyList)})})});
        test!(5, 4 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(List{elem: e(),
                                        next: dr(List{elem: e(),
                                                      next: dr(EmptyList)})})})});
        test!(5, 5 =>
              Combination{operator:
                  dr(Combination{operator:
                         dr(Combination{operator:
                                dr(Combination{operator:
                                       dr(Combination{operator: dr(EmptyNest),
                                                      operands: dr(EmptyList)}),
                                               operands: dr(EmptyList)}),
                                        operands: dr(EmptyList)}),
                                 operands: dr(EmptyList)}),
                          operands:
                  dr(List{elem: e(),
                          next: dr(List{elem: e(),
                                        next:
                                        dr(List{elem: e(),
                                                next:
                                                dr(List{elem: e(),
                                                        next: dr(EmptyList)})})})})});
    }
}
