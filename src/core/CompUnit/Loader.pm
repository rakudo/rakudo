class CompUnit::Loader is repr('Uninstantiable') {
    # Load a file from source and compile it
    method load-source-file(IO::Path $path) returns CompUnit::Handle {
        # Get the compiler and compile the code, then run it
        # (which runs the mainline and captures UNIT).
        my $?FILES   := $path.Str;
        self.load-source($path.slurp(:bin))
    }

    # Decode the specified byte buffer as source code, and compile it
    method load-source(Buf:D $bytes) returns CompUnit::Handle {
        my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);

        my $*CTXSAVE := self;
        my $eval     := nqp::getcomp('perl6').compile($bytes.decode);

        my $*MAIN_CTX;
        $eval();

        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);

        CATCH {
            default {
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                .throw;
            }
        }

        CompUnit::Handle.new($*MAIN_CTX)
    }

    # Load a pre-compiled file
    proto method load-precompilation-file(|) { * }
    multi method load-precompilation-file(IO::Path $path) returns CompUnit::Handle {
        my $*CTXSAVE := self;
        my %*COMPILING := nqp::hash();
        my Mu $*MAIN_CTX;
        nqp::loadbytecode($path.Str);
        CompUnit::Handle.new($*MAIN_CTX)
    }

    multi method load-precompilation-file(IO::Handle $handle) returns CompUnit::Handle {
        my $*CTXSAVE := self;
        my %*COMPILING := nqp::hash();
        my Mu $*MAIN_CTX;
        nqp::loadbytecodefh(nqp::getattr($handle, IO::Handle, '$!PIO'), $handle.path.Str);
        CompUnit::Handle.new($*MAIN_CTX)
    }

    # Load the specified byte buffer as if it was the contents of a
    # precompiled file
    method load-precompilation(Buf:D $bytes) returns CompUnit::Handle {
        my $*CTXSAVE := self;
        my %*COMPILING := nqp::hash();
        my Mu $*MAIN_CTX;
        nqp::loadbytecodebuffer($bytes);
        CompUnit::Handle.new($*MAIN_CTX)
    }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }
}

# vim: ft=perl6 expandtab sw=4
