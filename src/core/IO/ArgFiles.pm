my class ArgFiles {
    has $.args;
    has $.filename;
    has $!io;
    has $!ins;

    method eof() { ! $!args }

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
        gather while !$.eof && $l-- > 0 {
           my $line = $.get;
           if $line.defined {
               take $line;
           }
        }
    }
}

