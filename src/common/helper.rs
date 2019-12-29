use crate::{
    Parser, ParseIter, ParseIterItem, SourceStream, TextConcat,
    parser::{CharClassifier, DatumAllocator, OperatorBindings},
};


pub(crate) fn collect_up_to_first_err<'p, CC, DA, OB, S>
    (pi: ParseIter<'p, Parser<CC, DA, OB>, S>)
     -> Vec<ParseIterItem<DA, OB>>
    where CC: CharClassifier,
          DA: DatumAllocator,
          DA::TT: TextConcat<DA>,
          OB: OperatorBindings<DA>,
          Parser<CC, DA, OB>: 'p,
          S: SourceStream<DA>,
{
    let mut already_errored = false;
    pi.take_while(|r|
                  if already_errored {
                      false
                  } else {
                      if r.is_err() {
                          already_errored = true;
                      }
                      true
                  })
      .collect()
}
