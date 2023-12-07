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


// Re-export everything from the core crate. (Except items shadowed by ours,
// which are re-exported elsewhere.)
#[doc(no_inline)]
#[allow(unreachable_pub)] // (TODO: Only needed because of false-positive bug?)
pub use kul_core::*;

pub mod drop;

/// `Parser`s and related types and functions, provided for convenience, that
/// use recommended types for instantiating many of the generic parameters of
/// this crate, for common basic parsing applications.
pub mod common {
    mod helper;
    pub mod inmem;
    pub mod stream;
}

/// `SourceStream` types that use the `std` library, including heap allocation.
pub mod source_stream {
    mod char_iter_src_strm;
    pub use char_iter_src_strm::*;

    mod strish_iter_src_strm;
    pub use strish_iter_src_strm::*;
}

// The below modules shadow those of `kul_core` but re-export everything from
// those in addition to providing some of their own items.

pub mod datum;

/// Types for `Parser`s that use the `std` library, including heap allocation.
/// Also re-exports the core crate's module and premades.
pub mod parser {
    #[doc(no_inline)]
    pub use kul_core::parser::{*, premade::*};

    mod box_alloc;
    pub use box_alloc::BoxDatumAllocator;

    mod rc_alloc;
    pub use rc_alloc::RcDatumAllocator;

    mod arc_alloc;
    pub use arc_alloc::ArcDatumAllocator;

    mod hashmap_bindings;
    pub use hashmap_bindings::HashMapOperatorBindings;
}

/// `Text` types that use the `std` library, including heap allocation.  Also
/// re-exports the core crate's module and premades.
#[allow(clippy::module_name_repetitions)]
pub mod text {
    #[doc(no_inline)]
    pub use kul_core::text::{*, premade::*};

    /// `TextChunk` types that use the `std` library, including heap allocation.
    /// Also re-exports the core crate's module and premades.
    pub mod chunk {
        #[doc(no_inline)]
        pub use kul_core::text::chunk::{*, premade::*};

        mod pos_strish;
        pub use pos_strish::*;
    }

    mod vec;
    pub use vec::TextVec;
}
