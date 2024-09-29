class CompUnit::Loader is repr('Uninstantiable') {

    # Load a file from source and compile it
    method load-source-file(IO::Path:D $path --> CompUnit::Handle:D) {
        # Get the compiler and compile the code, then run it
        # (which runs the mainline and captures UNIT).
        my $?FILES   := $path.Str;
        self.load-source($path.slurp(:bin))
    }

    # Decode the specified byte buffer as source code, and compile it
    method load-source(Blob:D $bytes --> CompUnit::Handle:D) {
        my $original-GLOBAL := nqp::ifnull(nqp::gethllsym('Raku','GLOBAL'),Mu);
        CATCH {  # use CATCH instead of LEAVE: makes the normal flow faster
            default {
                nqp::bindhllsym('Raku','GLOBAL',$original-GLOBAL);
                .rethrow;
            }
        }

        my $handle := my $*CTXSAVE := CompUnit::Handle.new;
        nqp::getcomp('Raku').compile($bytes.decode)();      # compile *and* run

        nqp::bindhllsym('Raku','GLOBAL',$original-GLOBAL);
        $handle
    }

    # Load a pre-compiled file
    proto method load-precompilation-file(|) {*}
    multi method load-precompilation-file(
      IO::Path:D $precompiled-file
    --> CompUnit::Handle:D) {
        my $handle := my $*CTXSAVE := CompUnit::Handle.new;
        nqp::loadbytecode($precompiled-file.Str);
        $handle
    }

    multi method load-precompilation-file(
      IO::Handle:D $precompiled-handle
    --> CompUnit::Handle:D) {
        my $compunit-handle := my $*CTXSAVE := CompUnit::Handle.new;

        # Switch file handle to binary mode before passing it off to the VM,
        # so we don't lose things hanging around in the decoder.
        $precompiled-handle.encoding(Nil);
#?if jvm
        nqp::loadbytecodebuffer($precompiled-handle.slurp(:bin, :close));
#?endif
#?if !jvm
        nqp::loadbytecodefh(
          nqp::getattr($precompiled-handle,IO::Handle,'$!PIO'),
          $precompiled-handle.path.Str
        );
#?endif
        $compunit-handle
    }

    # Load the specified byte buffer as if it was the contents of a
    # precompiled file
    method load-precompilation(Blob:D $bytes --> CompUnit::Handle:D) {
        my $handle := my $*CTXSAVE := CompUnit::Handle.new;
        nqp::loadbytecodebuffer($bytes);
        $handle
    }
}

# vim: expandtab shiftwidth=4
