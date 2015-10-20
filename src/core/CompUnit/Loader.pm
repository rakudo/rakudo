class CompUnit::Loader is repr('Uninstantiable') {
    # Load a file from source and compile it
    method load-source-file(Str $path) returns CompUnit::Handle {
         ...
    }

    # Decode the specified byte buffer as source code, and compile it
    method load-source(Buf:D $bytes) returns CompUnit::Handle {
        ...
    }

    # Load a pre-compiled file
    method load-precompilation-file(Str $path) returns CompUnit::Handle {
        ...
    }

    # Load the specified byte buffer as if it was the contents of a
    # precompiled file
    method load-precompilation(Buf:D $bytes) returns CompUnit::Handle {
        ... # XXX this one needs MoarVM/JVM backends to expose a new API
    }
}

# vim: ft=perl6 expandtab sw=4
