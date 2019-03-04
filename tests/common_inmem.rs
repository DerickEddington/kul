use kul::common::inmem;

use kul_shared_tests::suites::test_suite0;


#[test]
fn suite0_parser() {
    let p = inmem::parser(inmem::OperatorBindings::<'_, bool, i8>::default());
    test_suite0(p);
}
