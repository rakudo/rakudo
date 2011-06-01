class IO::ArgFiles is IO {
    has $!args;
    has $.filename;

    method eof() {
        ! $!args && $!filename.defined && (!pir::istrue($!PIO) || ?$!PIO.eof);
    }

    method get() {
        unless $!PIO {
            $!filename = $!args ?? $!args.shift !! '-';
            self.open($!filename, :r) ||
                fail "Unable to open file '$!filename'";
        }
        my $x = $!PIO.readline;
        if $x eq '' && ?$!PIO.eof {
            self.close;
            $!PIO = Nil;
            $!args ?? self.get !! fail "End of argfiles reached"
        }
        else {
            $!ins++;
            $x.chomp;
        }
    }
}

# vim: ft=perl6
