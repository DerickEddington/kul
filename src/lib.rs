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
//! [`kul_core`]: ../kul_core/index.html
//! [`Parser`]: ../kul_core/struct.Parser.html
//! [`Datum`]: ../kul_core/enum.Datum.html
//! [`Box`]: http://doc.rust-lang.org/std/boxed/struct.Box.html
//! [`Rc`]: http://doc.rust-lang.org/std/rc/struct.Rc.html
//! [`Arc`]: http://doc.rust-lang.org/std/sync/struct.Arc.html
//! [`Text`]: ../kul_core/trait.Text.html
//! [`SourceStream`]: ../kul_core/trait.SourceStream.html
//! [`Drop`]: http://doc.rust-lang.org/std/ops/trait.Drop.html


#![forbid(unsafe_code)]

// Warn about desired lints that would otherwise be allowed by default.
#![warn(
    // Groups
    future_incompatible,
    nonstandard_style,
    rust_2018_compatibility, // unsure if needed with Cargo.toml having edition="2018"
    rust_2018_idioms,
    unused,
    clippy::all,
    clippy::pedantic,
    // Individual lints not included in above groups and desired.
    macro_use_extern_crate,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    // missing_doc_code_examples, // maybe someday
    private_doc_tests,
    // single_use_lifetimes, // annoying hits on invisible derived impls
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences,
)]
// Exclude (re-allow) undesired lints included in above groups.
#![allow(
    explicit_outlives_requirements, // annoying hits on invisible derived impls
    clippy::non_ascii_literal,
)]


// Re-export everything from the core crate. (Except items shadowed by ours,
// which are re-exported elsewhere.)
#[doc(no_inline)]
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
