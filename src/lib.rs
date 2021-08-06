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
