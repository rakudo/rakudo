my class IO::Pipe is IO::Handle {
    has $.proc;
    method close(IO::Pipe:D:) {
        my $PIO := nqp::getattr(nqp::decont(self), IO::Handle, '$!PIO');
        $!proc.status( nqp::closefh_i($PIO) ) if nqp::defined($PIO);
        nqp::bindattr(nqp::decont(self), IO::Handle, '$!PIO', Mu);
        $!proc;
    }

    method lines($limit = Inf) {
        if $limit == Inf {
            gather while nqp::p6definite(my $line = self.get) {
                take $line;
            }
        }
        else {
            my $count = 0;
            gather while ++$count <= $limit && nqp::p6definite(my $line = self.get) {
                take $line;
            }
        }
    }
}

# vim: ft=perl6 expandtab sw=4
