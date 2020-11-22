//! Additional, more convenient, functionality, which leverages the Rust
//! standard library, layered on top of [`kul_core`].
//!
//! This crate:
//!
//! * Provides [`Parser`]s, for [ready-made](common/index.html) use for common
//! basic parsing applications, that set pre-chosen types for most of the
//! generic parameters.
//!
//! * Re-exports all of [`kul_core`].
//!
//! * Provides [`Datum`] reference types that wrap the standard [`Box`], [`Rc`],
//! and [`Arc`] types, for heap-allocating `Datum`s.
//!
//! * Provides [`Text`] types that use heap allocation for their chunking.
//!
//! * Provides [`SourceStream`] types suitable for efficient use with streaming
//! sources that buffer into heap-allocated string chunks.  It is possible to
//! achieve minimal copying such that streamed data is only copied once from the
//! OS into the user-space string chunks.  Once buffered, these types produce
//! `Text` types that have zero-copy operation.
//!
//! * Avoids stack overflows (when possible) when dropping the provided
//! heap-allocated `Datum` types, so they can handle being used for very-deep
//! trees (e.g. long lists), by using a custom [`Drop`] implementation for them.
//! (Otherwise the compiler's default drop recursion could overflow.)
//!
//! Unlike [`kul_core`], this crate's purpose mostly is to provide premade
//! implementations intended for ready use.  So, instead of placing such items
//! in sub-modules named `premade`, they are placed at the top of their
//! respective modules, including for the premade items re-exported from
//! `kul_core`.
//!
//! [`kul_core`]: http://docs.rs/kul_core/latest/kul_core/
//! [`Parser`]: http://docs.rs/kul_core/latest/kul_core/struct.Parser.html
//! [`Datum`]: http://docs.rs/kul_core/latest/kul_core/enum.Datum.html
//! [`Box`]: http://doc.rust-lang.org/std/boxed/struct.Box.html
//! [`Rc`]: http://doc.rust-lang.org/std/rc/struct.Rc.html
//! [`Arc`]: http://doc.rust-lang.org/std/sync/struct.Arc.html
//! [`Text`]: http://docs.rs/kul_core/latest/kul_core/trait.Text.html
//! [`SourceStream`]: http://docs.rs/kul_core/latest/kul_core/trait.SourceStream.html
//! [`Drop`]: http://doc.rust-lang.org/std/ops/trait.Drop.html


kul_lints::declare_lints_on_top_module!();
