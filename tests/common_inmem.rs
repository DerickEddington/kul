use kruvi::common::inmem;

use kruvi_shared_tests::suites::test_suite0;


#[test]
fn suite0_parser() {
    let p = inmem::parser(inmem::OperatorBindings::<'_, bool, i8>::default());
    test_suite0(p);
}
