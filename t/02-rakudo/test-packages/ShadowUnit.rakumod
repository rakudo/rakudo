# A module whose top-level package-scoped symbols land in its GLOBALish and,
# on a `require`, in the caller's %?REQUIRE-SYMBOLS, shadowing same-named
# symbols the caller can see at compile time.
class BundleShadow {
    method which() { "required" }
    class Inner {
        method which() { "required-inner" }
    }
}

our sub bundle-shadow($v) { "required-" ~ $v }
