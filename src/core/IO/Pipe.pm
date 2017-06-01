my class IO::Pipe is IO::Handle {
    has $.proc;

    method TWEAK(:$enc, :$bin, Mu :$PIO --> Nil) {
        if $bin {
            die X::IO::BinaryAndEncoding.new if nqp::isconcrete($enc);
        }
        else {
            my $encoding = Rakudo::Internals.NORMALIZE_ENCODING($enc || 'utf-8');
            nqp::bindattr(self, IO::Handle, '$!encoding', $encoding);
            my $decoder := Rakudo::Internals::VMBackedDecoder.new($encoding, :translate-nl);
            $decoder.set-line-separators($.nl-in.list);
            nqp::bindattr(self, IO::Handle, '$!decoder', $decoder);
        }
        nqp::bindattr(self, IO::Handle, '$!PIO', nqp::decont($PIO));
    }

    method close(IO::Pipe:D:) {
        my $PIO := nqp::getattr(nqp::decont(self), IO::Handle, '$!PIO');
        $!proc.status( nqp::closefh_i($PIO) ) if nqp::defined($PIO);
        nqp::bindattr(nqp::decont(self), IO::Handle, '$!PIO', Mu);
        $!proc;
    }

    method IO   { IO::Path }
    method path { IO::Path }
}

# vim: ft=perl6 expandtab sw=4
