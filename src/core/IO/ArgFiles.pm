my class IO::ArgFiles is IO {
    has $.args;
    has $.filename;
    has $!io;
    has $!ins;

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
            $!io = IO;
            fail "End of argfiles reached" unless $!args;
            $x = self.get;
        }
        $!ins++;
        $x;
    }

    method lines($limit = *) {
        my $l = $limit ~~ Whatever ?? $Inf !! $limit;
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
            my $fn = $!args.shift;
            my $file = open($fn);
            @chunks.push: $file.slurp;
        }
        return $*IN.slurp unless @chunks;
        @chunks.join;
    }
}

