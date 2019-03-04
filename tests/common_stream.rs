use kul::common::stream;

use kul_shared_tests::suites::test_suite0;


#[test]
fn suite0_parser() {
    let p = stream::parser(stream::OperatorBindings::<i16, f64>::default());
    test_suite0(p);
}
