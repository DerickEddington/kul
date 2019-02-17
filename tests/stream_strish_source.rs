//! Tests `StrishIterSourceStream` similar to how it would be used with a
//! streaming source.

use std::{
    rc::Rc, sync::Arc,
    str::Split,
    iter::{Map, once, Once},
    vec,
    marker::PhantomData,
    fmt::Debug,
};

use kruvi::{
    Parser, Datum,
    source_stream::StrishIterSourceStream,
    parser::{DatumAllocator, AllocError, DefaultCharClassifier, EmptyOperatorBindings},
    datum::{DatumBox, DatumMutRef, MutRefDatum},
    text::{TextVec, chunk::{PosStrish, RefCntStrish}, TextDatumList},
};

use kruvi_shared_tests::suites::test_suite0_with;


fn parser<DA, I>(init: I) -> Parser<DefaultCharClassifier,
                                    DA,
                                    EmptyOperatorBindings>
    where DA: From<I>,
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator: DA::from(init),
        bindings: EmptyOperatorBindings,
    }
}

#[derive(Debug)]
struct BoxDatumAllocator<R>(PhantomData<R>);

impl<R> From<()> for BoxDatumAllocator<R> {
    fn from(_: ()) -> Self { BoxDatumAllocator(PhantomData) }
}

impl<R> DatumAllocator for BoxDatumAllocator<R>
    where R: RefCntStrish
{
    type TT = TextVec<PosStrish<R>>;
    type ET = ();
    type DR = DatumBox<Self::TT, Self::ET>;

    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        Ok(DatumBox::new(from))
    }
}


fn siss_maker<SI, TT, F>(str_to_strish_iter: F)
                         -> Option<impl Fn(&'static str)
                                           -> StrishIterSourceStream<SI, TT>>
    where SI: Iterator,
          SI::Item: RefCntStrish,
          F: Fn(&'static str) -> SI,
{
    Some(move |input| StrishIterSourceStream::new(str_to_strish_iter(input)))
}

fn each_char<S>(input: &'static str) -> Map<Split<'static, &'static str>,
                                            fn(&'static str) -> S>
    where S: RefCntStrish,
{
    // Note that split causes there to be additional first and last items that
    // are empty strings, which exercises handling those too.
    input.split("").map(S::from_str)
}

#[allow(unused_results)]
fn grouped<S>(lens: Vec<usize>)
              -> impl Fn(&'static str) -> Map<vec::IntoIter<String>,
                                              fn(String) -> S>
    where S: RefCntStrish,
{
    move |input|
    input.chars().fold((vec![String::new()],
                        0,
                        lens.iter().cloned().cycle().peekable()),
                       |(mut strings, mut count, mut lens_iter), ch| {
                           count += 1;
                           if count % (lens_iter.peek().unwrap() + 1) == 0 {
                               let mut s = String::new();
                               s.push(ch);
                               strings.push(s);
                               lens_iter.next();
                               count = 1;
                           } else {
                               strings.last_mut().unwrap().push(ch);
                           }
                           (strings, count, lens_iter)
                       })
                 .0.into_iter()
                   .map(|s| S::from_str(&s))
}

fn single<S>(input: &'static str) -> Once<S>
    where S: RefCntStrish,
{
    once(S::from_str(input))
}


fn suite0_textvec<SI, F>(str_to_strish_iter: F)
    where SI: Iterator,
          SI::Item: RefCntStrish + Debug,
          F: Fn(&'static str) -> SI,
{
    test_suite0_with(parser::<BoxDatumAllocator<SI::Item>, _>(()),
                     siss_maker(str_to_strish_iter));
}

#[test]
fn suite0_each_char_rc_string() {
    suite0_textvec(each_char::<Rc<String>>);
}

#[test]
fn suite0_each_char_arc_str() {
    suite0_textvec(each_char::<Arc<str>>);
}

#[test]
fn suite0_variable_grouped_rc_box_str() {
    suite0_textvec(grouped::<Rc<Box<str>>>(vec![2, 3, 4]));
}

#[test]
fn suite0_constant_grouped_arc_string() {
    suite0_textvec(grouped::<Arc<String>>(vec![5]));
}

#[test]
fn suite0_single_rc_str() {
    suite0_textvec(single::<Rc<str>>);
}

#[test]
fn suite0_single_arc_box_str() {
    suite0_textvec(single::<Arc<Box<str>>>);
}


// Test using it with `TextDatumList`, which uses the `Datum` allocator
// arguments of the `SourceStream` methods.

type Array<'a, R> = [MutRefDatum<'a, TextDatumList<'a, PosStrish<R>, ()>, ()>];
type ArrayRef<'a, R> = &'a mut Array<'a, R>;

struct ArrayDatumAllocator<'a, R> {
    free: Option<ArrayRef<'a, R>>,
}

impl<'a, R> From<ArrayRef<'a, R>> for ArrayDatumAllocator<'a, R> {
    fn from(v: ArrayRef<'a, R>) -> Self {
        ArrayDatumAllocator{free: Some(v)}
    }
}

impl<'a, R> DatumAllocator for ArrayDatumAllocator<'a, R>
    where R: RefCntStrish
{
    type TT = TextDatumList<'a, PosStrish<R>, Self::ET>;
    type ET = ();
    type DR = DatumMutRef<'a, Self::TT, Self::ET>;

    fn new_datum(&mut self, from: Datum<Self::TT, Self::ET, Self::DR>)
                 -> Result<Self::DR, AllocError>
    {
        match self.free.take().and_then(|a| a.split_first_mut()) {
            Some((dr, rest)) => {
                *dr = from;
                self.free = Some(rest);
                Ok(DatumMutRef(dr))
            }
            None => Err(AllocError::AllocExhausted)
        }
    }
}


fn suite0_text_datum_list<SI, F>(str_to_strish_iter: F, array_size: usize)
    where SI: Iterator,
          SI::Item: RefCntStrish + Debug,
          F: Fn(&'static str) -> SI,
{
    use std::iter::{repeat_with, FromIterator};
    use Datum::Extra;

    let mut datum_array: Box<Array<'_, SI::Item>> =
        Vec::from_iter(repeat_with(|| Extra(())).take(array_size))
        .into_boxed_slice();

    test_suite0_with(
        parser::<ArrayDatumAllocator<'_, SI::Item>, _>(&mut datum_array[..]),
        siss_maker(str_to_strish_iter));
}

#[test]
fn suite0_text_datum_list_each_char_rc_string() {
    suite0_text_datum_list(each_char::<Rc<String>>, 0x300);
}

#[test]
fn suite0_text_datum_list_each_char_rc_box_str() {
    suite0_text_datum_list(each_char::<Rc<Box<str>>>, 0x300);
}

#[test]
fn suite0_text_datum_list_variable_grouped_rc_str() {
    suite0_text_datum_list(grouped::<Rc<str>>(vec![2, 1, 4, 3]), 0x200);
}

#[test]
fn suite0_text_datum_list_constant_grouped_arc_string() {
    suite0_text_datum_list(grouped::<Arc<String>>(vec![2]), 0x200);
}

#[test]
fn suite0_text_datum_list_single_arc_box_str() {
    suite0_text_datum_list(single::<Arc<Box<str>>>, 0x200);
}

#[test]
fn suite0_text_datum_list_single_arc_str() {
    suite0_text_datum_list(single::<Arc<str>>, 0x200);
}
