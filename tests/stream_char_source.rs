//! Tests `CharIterSourceStream` similar to how it would be used with a
//! streaming source.

use std::{str, fmt::Debug};

use kul::{
    Parser, Datum,
    source_stream::{CharIterSourceStream, to_rc_string, to_rc_box_str, to_rc_str,
                    to_arc_string, to_arc_box_str, to_arc_str},
    parser::{BoxDatumAllocator, SliceDatumAllocator,
             DefaultCharClassifier, DatumAllocator},
    datum::MutRefDatum,
    text::{TextVec, chunk::{PosStrish, RefCntStrish}, TextDatumList},
};

use kul_shared_tests::{
    suites::test_suite0_with,
    bindings::BasicTestOperatorBindings,
};


fn parser<DA>(allocator: DA) -> Parser<DefaultCharClassifier,
                                       DA,
                                       BasicTestOperatorBindings<DA>>
    where DA: DatumAllocator,
{
    Parser {
        classifier: DefaultCharClassifier,
        allocator,
        bindings: BasicTestOperatorBindings::default(),
    }
}


#[allow(clippy::needless_lifetimes)] // Note: Clippy is wrong: 'f is needed
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
    test_suite0_with(parser(BoxDatumAllocator::<TextVec<PosStrish<R>>, ()>::default()),
                     ciss_maker(&converter));
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
    use kul::Text;

    let mut p = parser(BoxDatumAllocator::<TextVec<PosStrish<Rc<str>>>, ()>::default());
    let ciss = CharIterSourceStream::new("foo".chars(), to_rc_str);
    let pi = p.parse(ciss);
    let all = pi.collect::<Vec<_>>();
    assert_eq!(all.len(), 1);
    let datum = all[0].as_ref().unwrap();
    let chunks = match datum {
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

fn suite0_text_datum_list<F, R>(converter: F)
    where F: Fn(String) -> R,
          R: RefCntStrish + Debug,
{
    use std::iter::repeat_with;
    use Datum::Extra;

    let mut datum_array: Box<Array<'_, R>> =
        repeat_with(|| Extra(())).take(0x200).collect::<Vec<_>>()
        .into_boxed_slice();

    test_suite0_with(parser(SliceDatumAllocator::new(&mut datum_array[..])),
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
