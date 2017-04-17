my class IO::Pipe is IO::Handle {
    has $.proc;
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
