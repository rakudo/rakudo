my class IO::Pipe is IO::Handle {
    has $.proc;
    has $!on-read;
    has $!on-write;
    has $!on-close;
    has $!bin-supply;
    has $!eof = False;
    has $!closed = False;

    method TWEAK(:$!on-close!, :$enc, :$bin, :$!on-read, :$!on-write, :$!bin-supply --> Nil) {
        if $bin {
            die X::IO::BinaryAndEncoding.new if nqp::isconcrete($enc);
        }
        else {
            my $encoding = Rakudo::Internals.NORMALIZE_ENCODING($enc || 'utf-8');
            nqp::bindattr(self, IO::Handle, '$!encoding', $encoding);
            my $decoder := Encoding::Decoder::Builtin.new($encoding, :translate-nl);
            $decoder.set-line-separators($.nl-in.list);
            nqp::bindattr(self, IO::Handle, '$!decoder', $decoder);
        }
    }

    method read-internal($) {
        if $!on-read {
            my \result = $!on-read();
            $!eof = True if result.elems == 0;
            result
        }
        else {
            die "This pipe was opened for writing, not reading"
        }
    }

    method eof-internal() {
        $!eof
    }

    method write-internal($data) {
        $!on-write
            ?? $!on-write($data)
            !! die "This pipe was opened for reading, not writing"
    }

    method flush(IO::Handle:D: --> True) { #`(No buffering) }

    method close(IO::Pipe:D:) {
        $!on-close();
        $!closed = True;
        $!proc;
    }

    method opened(IO::Pipe:D:) {
        not $!closed
    }

    method t(IO::Pipe:D:) {
        False
    }

    method native-descriptor(IO::Pipe:D:) {
        fail "An IO::Pipe does not have a native-descriptor"
    }

    method IO   { IO::Path }
    method path { IO::Path }
}

# vim: ft=perl6 expandtab sw=4
