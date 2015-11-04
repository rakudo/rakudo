class CompUnit::Loader is repr('Uninstantiable') {
    # Load a file from source and compile it
    method load-source-file(Str $path) returns CompUnit::Handle {
        my $*CTXSAVE := self;
        my $fh := nqp::open($path, 'r');
        nqp::setencoding($fh, 'utf8');
        my $source := nqp::readallfh($fh);
        nqp::closefh($fh);

        # Get the compiler and compile the code, then run it
        # (which runs the mainline and captures UNIT).
        my $?FILES   := $path;
        my $eval     := nqp::getcomp('perl6').compile($source);

        my $*MAIN_CTX;
        $eval();
        CompUnit::Handle.new($*MAIN_CTX)
    }

    # Decode the specified byte buffer as source code, and compile it
    method load-source(Buf:D $bytes) returns CompUnit::Handle {
        ...
    }

    # Load a pre-compiled file
    method load-precompilation-file(Str $path) returns CompUnit::Handle {
        my $*CTXSAVE := self;
        my %*COMPILING := nqp::hash();
        my Mu $*MAIN_CTX;
        nqp::loadbytecode($path);
        CompUnit::Handle.new($*MAIN_CTX)
    }

    # Load the specified byte buffer as if it was the contents of a
    # precompiled file
    method load-precompilation(Buf:D $bytes) returns CompUnit::Handle {
        ... # XXX this one needs MoarVM/JVM backends to expose a new API
    }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }
}

# vim: ft=perl6 expandtab sw=4
