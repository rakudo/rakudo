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
            X::IO::BinaryAndEncoding.new.throw if nqp::isconcrete($enc)
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

    method READ($) {
        if $!on-read {
            loop {
                my \result = $!on-read();
                if result.DEFINITE {
                    return result if result.elems;
                }
                else {
                    $!eof = True;
                    return nqp::create(buf8.^pun)
                }
            }
        }
        else {
            X::AdHoc.new( payload => "This pipe was opened for writing, not reading" ).throw
        }
    }

    method EOF() {
        $!eof
    }

    method WRITE($data) {
        $!on-write
            ?? $!on-write($data)
            !! X::AdHoc.new( payload => "This pipe was opened for reading, not writing").throw
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

# vim: expandtab shiftwidth=4
