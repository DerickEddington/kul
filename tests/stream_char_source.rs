//! Tests `CharIterSourceStream` similar to how it would be used with a
//! streaming source.

use std::{str, fmt::Debug, marker::PhantomData};

use kruvi::{
    Parser, Datum,
    source_stream::{CharIterSourceStream, to_rc_string, to_rc_box_str, to_rc_str,
                    to_arc_string, to_arc_box_str, to_arc_str},
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


fn ciss_maker<'f, F, R>(converter: &'f F)
                        -> Option<impl Fn(&'static str)
                                          -> CharIterSourceStream<str::Chars<'static>,
                                                                  &'f F, R>>
    where F: Fn(String) -> R,
          R: RefCntStrish,
{
    Some(move |input: &'static str| CharIterSourceStream::new(input.chars(), converter))
}


fn suite0_textvec<F, R>(converter: F)
    where F: Fn(String) -> R,
          R: RefCntStrish + Debug,
{
    test_suite0_with(parser::<BoxDatumAllocator<R>, _>(()), ciss_maker(&converter));
}

#[test]
fn suite0_rc_string() {
    suite0_textvec(to_rc_string);
}

#[test]
fn suite0_rc_box_str() {
    suite0_textvec(to_rc_box_str);
}

#[test]
fn suite0_rc_str() {
    suite0_textvec(to_rc_str);
}

#[test]
fn suite0_arc_string() {
    suite0_textvec(to_arc_string);
}

#[test]
fn suite0_arc_box_str() {
    suite0_textvec(to_arc_box_str);
}

#[test]
fn suite0_arc_str() {
    suite0_textvec(to_arc_str);
}


/// Check that outside of the `pos_strish` module, the `PosStrish.val` field is
/// usable with `AsRef`.
#[test]
fn posstrish() {
    use std::rc::Rc;
    use kruvi::Text;

    let mut p = parser::<BoxDatumAllocator<Rc<str>>, _>(());
    let ciss = CharIterSourceStream::new("foo".chars(), to_rc_str);
    let pi = p.parse(ciss);
    let all = pi.collect::<Vec<_>>();
    assert_eq!(all.len(), 1);
    let datumref = all[0].as_ref().unwrap();
    let chunks = match &**datumref {
        Datum::Text(textvec) => textvec.iter_chunks().collect::<Vec<_>>(),
        _ => unreachable!(),
    };
    assert_eq!(chunks.len(), 1);
    let posstrish_chunk = chunks[0];
    assert_eq!(posstrish_chunk.val.as_ref(), "foo");
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


fn suite0_text_datum_list<F, R>(converter: F)
    where F: Fn(String) -> R,
          R: RefCntStrish + Debug,
{
    use std::iter::{repeat_with, FromIterator};
    use Datum::Extra;

    let mut datum_array: Box<Array<'_, R>> =
        Vec::from_iter(repeat_with(|| Extra(())).take(0x300))
        .into_boxed_slice();

    test_suite0_with(parser::<ArrayDatumAllocator<'_, R>, _>(&mut datum_array[..]),
                     ciss_maker(&converter));
}

#[test]
fn suite0_text_datum_list_rc_string() {
    suite0_text_datum_list(to_rc_string)
}

#[test]
fn suite0_text_datum_list_rc_box_str() {
    suite0_text_datum_list(to_rc_box_str)
}

#[test]
fn suite0_text_datum_list_rc_str() {
    suite0_text_datum_list(to_rc_str)
}

#[test]
fn suite0_text_datum_list_arc_string() {
    suite0_text_datum_list(to_arc_string)
}

#[test]
fn suite0_text_datum_list_arc_box_str() {
    suite0_text_datum_list(to_arc_box_str)
}

#[test]
fn suite0_text_datum_list_arc_str() {
    suite0_text_datum_list(to_arc_str)
}
