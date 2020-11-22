//! Single definition of lints to use across the workspace.

#[macro_export]
macro_rules! declare_lints_on_top_module {
    () => {
        #[forbid(unsafe_code)]
        // Warn about desired lints that would otherwise be allowed by default.
        #[warn(
            // Groups
            future_incompatible,
            nonstandard_style,
            rust_2018_compatibility, // unsure if needed with edition="2018"
            rust_2018_idioms,
            unused,
            clippy::all,
            clippy::pedantic,
            // clippy::restriction, // someday
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
        #[allow(
            explicit_outlives_requirements, // annoying hits on invisible derived impls
            clippy::non_ascii_literal,
            clippy::must_use_candidate, // excessively pedantic
            clippy::missing_errors_doc, // for now
            clippy::enum_glob_use,
            clippy::wildcard_imports,
            clippy::similar_names,
            // For when clippy::restriction is on:
            clippy::else_if_without_else,
            clippy::missing_inline_in_public_items,
            clippy::implicit_return,
            clippy::missing_docs_in_private_items,
        )]
        #[path = "top.rs"] // So sub-modules' files are in same dir.
        mod top;

        pub use top::*;
    }
}
