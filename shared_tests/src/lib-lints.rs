//! Used by the tests of both the [core](../kul_core/index.html)
//! and the [full](../kul/index.html) crates.  It provides test suites that
//! can be run against many types of [`Parser`](../kul_core/struct.Parser.html),
//! and it uses tricks so that its representation of expected results can be
//! directly compared against a variety of
//! [`Datum`](../kul_core/enum.Datum.html) types.


kul_lints::declare_lints_on_lib_module!();
