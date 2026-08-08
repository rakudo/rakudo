unit module BundleExport;

# A module whose exports are declared through an explicit EXPORT::DEFAULT
# package (the older idiom, as Test::Async bundles use), rather than the
# `is export` trait.
package EXPORT::DEFAULT {
    our sub bundle-export($v) { "bundled " ~ $v }
}
