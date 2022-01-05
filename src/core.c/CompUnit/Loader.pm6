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
        my $preserve_global := nqp::ifnull(nqp::gethllsym('Raku', 'GLOBAL'), Mu);

        my $handle   := CompUnit::Handle.new;
        my $*CTXSAVE := $handle;
        my $eval     := nqp::getcomp('Raku').compile($bytes.decode);

        $eval();

        nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);

        CATCH {
            default {
                nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);
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
        nqp::loadbytecode($path.Str);
        $handle
    }

    multi method load-precompilation-file(IO::Handle $file --> CompUnit::Handle:D) {
        my $handle     := CompUnit::Handle.new;
        my $*CTXSAVE   := $handle;
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
        nqp::loadbytecodebuffer($bytes);
        $handle
    }
}

# vim: expandtab shiftwidth=4
