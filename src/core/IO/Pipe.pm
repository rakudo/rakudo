class IO::Pipe does IO does PIO {
    has $.proc;
    method close(IO::Pipe:D:) {
        my $PIO := nqp::getattr(nqp::decont(self), PIO, '$!PIO');
        $!proc.status( nqp::closefh_i($PIO) ) if nqp::defined($PIO);
        nqp::bindattr(nqp::decont(self), PIO, '$!PIO', Mu);
        $!proc;
    }
}

# vim: ft=perl6 expandtab sw=4
