class IO::Pipe does IO does PIO {
    has Str $.command;

    method BUILD(:$!command,|c) { self!set-PIO-attributes(|c) }

    method close(IO::Pipe:D:) {
        # TODO:b catch errors
        my $ps = Proc::Status.new;
        $ps.status( nqp::closefh_i($!PIO) ) if nqp::defined($!PIO);
        $!PIO := Mu;
        $ps;
    }

#    method seek(|c) {
#        fail "You cannot seek on an {self.^name}";
#    }
}

# vim: ft=perl6 expandtab sw=4
