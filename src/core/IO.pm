# XXX Relatively cheaty, just to get us able to output something.
# But you should see what USED to be here! O.O
sub print(*@list) {
    nqp::print(@list.shift) while @list.gimme(1);
    1.Bool
}

sub say(|$) {
    my $args := pir::perl6_current_args_rpa__P();
    print nqp::shift($args).gist while $args;
    print "\n";
}

sub gist(|$) {
    pir__perl6_box_rpa__PP(pir::perl6_current_args_rpa__P()).gist
}

class IO {
    has $!PIO;
    has Int $.ins = 0;
    has $.chomp = 1.Bool;

    proto method open(|$) { * }
    multi method open($filename, :$r, :$w, :$a, :$bin, :$chomp = 1.Bool) {
        my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r');
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO, '$!PIO',
             $filename eq '-'
                ?? ( $w || $a ?? pir::getstdout__P() !! pir::getstdin__P() )
                !! pir::open__PSS(nqp::unbox_s($filename), nqp::unbox_s($mode))
        );
        $!chomp = $chomp;
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

    method lines($limit = $Inf) {
        my $count = 0;
        gather while (my $line = self.get).defined && ++$count <= $limit {
            take $line;
        }
    }

    method print(*@list) {
        $!PIO.print(nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        1.Bool
    }
    method say(|$) {
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print: "\n";
    }
}

proto sub open(|$) { * }
multi sub open($filename, :$r, :$w, :$a, :$bin, :$chomp = 1.Bool) {
    IO.new.open($filename, :$r, :$w, :$a, :$bin, :$chomp);
}

$PROCESS::IN  = open('-');
$PROCESS::OUT = open('-', :w);
$PROCESS::ERR = IO.new;
nqp::bindattr(pir::perl6_decontainerize__PP($PROCESS::ERR),
        IO, '$!PIO', pir::getstderr__P());
