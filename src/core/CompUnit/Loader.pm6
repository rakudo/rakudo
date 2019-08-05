class CompUnit::Loader is repr('Uninstantiable') {
    # Load a file from source and compile it
    method load-source-file(IO::Path $path --> CompUnit::Handle) {
        # Get the compiler and compile the code, then run it
        # (which runs the mainline and captures UNIT).
        my $?FILES   := $path.Str;
        self.load-source($path.slurp(:bin))
    }

    # Decode the specified byte buffer as source code, and compile it
    method load-source(Blob:D $bytes --> CompUnit::Handle:D) {
        my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);

        my $handle   := CompUnit::Handle.new;
        my $*CTXSAVE := $handle;
        my $eval     := nqp::getcomp('perl6').compile($bytes.decode);

        $eval();

        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);

        CATCH {
            default {
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                .throw;
            }
        }

        $handle
    }

    # Load a pre-compiled file
    proto method load-precompilation-file(|) {*}
    multi method load-precompilation-file(IO::Path $path --> CompUnit::Handle:D) {
        my $handle     := CompUnit::Handle.new;
        my $*CTXSAVE   := $handle;
        # '%?OPTIONS' is expected by some code; mainly by the World object
        my %*COMPILING := nqp::hash('%?OPTIONS', nqp::hash());
        nqp::loadbytecode($path.Str);
        $handle
    }

    multi method load-precompilation-file(IO::Handle $file --> CompUnit::Handle:D) {
        my $handle     := CompUnit::Handle.new;
        my $*CTXSAVE   := $handle;
        my %*COMPILING := nqp::hash('%?OPTIONS', nqp::hash());
#?if !jvm
        # Switch file handle to binary mode before passing it off to the VM,
        # so we don't lose things hanging around in the decoder.
        $file.encoding(Nil);
        nqp::loadbytecodefh(nqp::getattr($file, IO::Handle, '$!PIO'), $file.path.Str);
#?endif
        $handle
    }

    # Load the specified byte buffer as if it was the contents of a
    # precompiled file
    method load-precompilation(Blob:D $bytes --> CompUnit::Handle:D) {
        my $handle     := CompUnit::Handle.new;
        my $*CTXSAVE   := $handle;
        my %*COMPILING := nqp::hash('%?OPTIONS', nqp::hash());
        nqp::loadbytecodebuffer($bytes);
        $handle
    }
}

# vim: ft=perl6 expandtab sw=4
