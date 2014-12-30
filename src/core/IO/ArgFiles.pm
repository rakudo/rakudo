my class IO::ArgFiles is IO::Handle {
    has $.args;
    has $.filename;
    has $!io;
    has $.ins;

    method eof() {
        ! $!args && $!io.opened && $!io.eof
    }

    method get() {
        unless $!io.defined && $!io.opened {
            $!filename = $!args ?? $!args.shift !! '-';
            $!io = open($!filename, :r) ||
                fail "Unable to open file '$!filename'";
        }
        my $x = $!io.get;
        while !$x.defined {
            $!io.close;
            $!io = IO::Handle;
            fail "End of argfiles reached" unless $!args;
            $x = self.get;
        }
        $!ins++;
        $x;
    }

    method lines($limit = *) {
        my $l = nqp::istype($limit,Whatever) ?? Inf !! $limit;
        gather while $l-- > 0 {
           take $.get // last;
        }
    }
    method slurp(IO::ArgFiles:D:) {
        my @chunks;
        if $!io && $!io.opened {
            @chunks.push: nqp::p6box_s($!io.readall);
            $!io.close;
        }
        while $!args {
            @chunks.push: slurp $!args.shift;
        }
        return $*IN.slurp-rest unless @chunks;
        @chunks.join;
    }
}

# vim: ft=perl6 expandtab sw=4
