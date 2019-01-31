//! Additional, more convenient, functionality, which leverages the Rust
//! standard library, layered on top of [`kruvi_core`].
//!
//! This crate:
//!
//! * Re-exports all of [`kruvi_core`].
//!
//! * Provides [`Datum`] reference types that wrap the standard [`Box`], [`Rc`],
//! and [`Arc`] types, for heap-allocating `Datum`s.
//!
//! * Provides [`Text`] types that use heap allocation for their chunking.
//!
//! * Avoids stack overflows (when possible) when dropping the provided
//! heap-allocated `Datum` types, so they can handle being used for very-deep
//! trees (e.g. long lists), by using a custom [`Drop`] implementation for them.
//! (Otherwise the compiler's default drop recursion could overflow.)
//!
//! * TODO: Provides an implementation of [`Parser`] that uses [`Box`] and has a
//! facility for establishing macro bindings.
//!
//! Unlike [`kruvi_core`], this crate's purpose mostly is to provide premade
//! implementations intended for ready use.  So, instead of placing such items
//! in sub-modules named `premade`, they are placed at the top of their
//! respective modules, including for the premade items re-exported from
//! `kruvi_core`.
//!
//! [`kruvi_core`]: ../kruvi_core/index.html
//! [`Datum`]: enum.Datum.html
//! [`Box`]: http://doc.rust-lang.org/std/boxed/struct.Box.html
//! [`Rc`]: http://doc.rust-lang.org/std/rc/struct.Rc.html
//! [`Arc`]: http://doc.rust-lang.org/std/sync/struct.Arc.html
//! [`Text`]: trait.Text.html
//! [`Drop`]: http://doc.rust-lang.org/std/ops/trait.Drop.html
//! [`Parser`]: struct.Parser.html

// TODO: Impl SourceStream for:
// - IntoIterator<Item=char>, by embedding in some struct that uses heap-alloc
//   for the accum'ing

// TODO: Integration tests that impl Text (and so a SourceStream) for:
// - Vec<char>, using kruvi_core's Text impl for &[char]
// - LinkedList<char>, just to test it can be done

// TODO: Parser impl that makes it easier by using Box and that provides API for
// establishing combiner bindings.

// Re-export everything from the core crate. (Except items shadowed by ours,
// which are re-exported elsewhere.)
#[doc(no_inline)]
pub use kruvi_core::*;

pub mod drop;

// The below modules shadow those of `kruvi_core` but re-export everything from
// those in addition to providing some of their own items.

pub mod datum;

/// Re-exports the core crate's module and premades.
pub mod parser {
    #[doc(no_inline)]
    pub use kruvi_core::parser::{*, premade::*};
}

/// `Text` types that use the `std` library, including heap allocation.  Also
/// re-exports the core crate's module and premades.
pub mod text {
    #[doc(no_inline)]
    pub use kruvi_core::text::{*, premade::*};

    /// Re-exports the core crate's module and premades.
    pub mod chunk {
        #[doc(no_inline)]
        pub use kruvi_core::text::chunk::{*, premade::*};
    }

    mod vec;
    pub use vec::TextVec;
}
