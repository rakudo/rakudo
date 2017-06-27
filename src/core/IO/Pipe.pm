my class IO::Pipe is IO::Handle {
    has $.proc;
    has $!on-read;
    has $!on-write;
    has $!on-close;
    has $!on-native-descriptor;
    has $!eof = False;
    has $!closed = False;

    method TWEAK(:$!on-close!, :$enc, :$bin, :$!on-read, :$!on-write,
                 :$!on-native-descriptor --> Nil) {
        if $bin {
            die X::IO::BinaryAndEncoding.new if nqp::isconcrete($enc);
        }
        else {
            my $encoding = Encoding::Registry.find($enc || 'utf-8');
            nqp::bindattr(self, IO::Handle, '$!encoding', $encoding.name);
            my $decoder := $encoding.decoder(:translate-nl);
            $decoder.set-line-separators($.nl-in.list);
            nqp::bindattr(self, IO::Handle, '$!decoder', $decoder);
            nqp::bindattr(self, IO::Handle, '$!encoder', $encoding.encoder(:translate-nl))
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
        $!closed = True;
        $!on-close()
    }

    method opened(IO::Pipe:D:) {
        not $!closed
    }

    method t(IO::Pipe:D:) {
        False
    }

    method native-descriptor(IO::Pipe:D:) {
        $!on-native-descriptor
            ?? $!on-native-descriptor()
            !! die("This pipe does not have an associated native descriptor")
    }

    method IO   { IO::Path }
    method path { IO::Path }
}

# vim: ft=perl6 expandtab sw=4
