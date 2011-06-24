# XXX Relatively cheaty, just to get us able to output something.
# But you should see what USED to be here! O.O
sub print(*@list) {
    nqp::print(@list.shift) while @list.gimme(1);
    1.Bool
}

sub say(|$) {
    print pir__perl6_box_rpa__PP(pir::perl6_current_args_rpa__P()).gist, "\n"
}

sub gist(|$) {
    pir__perl6_box_rpa__PP(pir::perl6_current_args_rpa__P()).gist
}

class IO {
    has $!PIO;
    has Int $.ins = 0;
    has $.chomp = True;

    proto method open(|$) { * }
    multi method open($filename, :$r, :$w, :$a, :$bin) {
        my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r');
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO, '$!PIO',
             $filename eq '-'
                ?? pir::getstdin__P()
                !! pir::open__PSS(nqp::unbox_s($filename), nqp::unbox_s($mode))
        );
        $!PIO.encoding($bin ?? 'binary' !! 'utf8');
        self;
    }

    method close() {
        # TODO:b catch errors
        $!PIO.close;
        1.Bool;
    }

    method eof() {
        nqp::p6bool($!PIO.eof);
    }

    method get() {
        my Str $x = nqp::p6box_s($!PIO.readline);
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        return Str if self.eof && $x eq '';

        $!ins++;
        # XXX
        # comment out as long as initiliaztion of attributes is a no-op
#        $!chomp ?? $x.chomp !! $x;
        $x.chomp;
    }

    method lines() {
        gather while (my $line = self.get).defined {
            take $line;
        }

    }
}
