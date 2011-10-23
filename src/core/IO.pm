# XXX Relatively cheaty, just to get us able to output something.
# But you should see what USED to be here! O.O
sub print(*@list) {
    $*OUT.print(@list.shift) while @list.gimme(1);
    Bool::True
}

sub say(|$) {
    my $args := pir::perl6_current_args_rpa__P();
    $*OUT.print(nqp::shift($args).gist) while $args;
    $*OUT.print("\n");
}

sub note(*@list) {
    my $args := pir::perl6_current_args_rpa__P();
    $*ERR.print(nqp::shift($args).gist) while $args;
    $*ERR.print("\n");
}

sub gist(|$) {
    nqp::p6parcel(pir::perl6_current_args_rpa__P(), Mu).gist
}

sub prompt($msg) {
    print $msg;
    $*IN.get;
}

class IO {
    has $!PIO;
    has Int $.ins = 0;
    has $.chomp = Bool::True;
    has $.path;

    proto method open(|$) { * }
    multi method open($path, :$r, :$w, :$a, :$bin, :$chomp = Bool::True) {
        my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r');
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO, '$!PIO',
             $path eq '-'
                ?? ( $w || $a ?? pir::getstdout__P() !! pir::getstdin__P() )
                !! pir::open__PSS(nqp::unbox_s($path), nqp::unbox_s($mode))
        );
        $!path = $path;
        $!chomp = $chomp;
        $!PIO.encoding($bin ?? 'binary' !! 'utf8');
        self;
    }

    method close() {
        # TODO:b catch errors
        $!PIO.close;
        Bool::True;
    }

    method eof() {
        nqp::p6bool($!PIO.eof);
    }

    method get() {
        unless $!PIO {
            self.open($.path, :chomp($.chomp));
        }
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

    method read(IO:D: Int:D $bytes) {
        my Mu $parrot_buffer := $!PIO.read_bytes(nqp::unbox_i($bytes));
        my $buf := nqp::create(Buf);
        nqp::bindattr($buf, Buf, '$!buffer', $parrot_buffer);
        $buf;
    }
    # first arguemnt should probably be an enum
    # valid values for $whence:
    #   0 -- seek from beginning of file
    #   1 -- seek relative to current position
    #   2 -- seek from the end of the file
    method seek(IO:D: Int:D $whence, Int:D $offset) {
        $!PIO.seek(nqp::unbox_i($whence), nqp::unbox_i($offset));
        True;
    }
    method tell(IO:D:) returns Int {
        nqp::p6box_i($!PIO.tell);
    }

    method write(IO:D: Buf:D $buf) {
        my Mu $b := nqp::getattr(
                        nqp::p6decont($buf),
                        Buf,
                        '$!buffer'
                    );
        $!PIO.print($b.get_string('binary'));
        True;
    }

    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method print(IO:D: *@list) {
        $!PIO.print(nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    method say(IO:D: |$) {
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print: "\n";
    }
    
    method slurp() {
        nqp::p6box_s($!PIO.readall());
    }

    method d() {
        self.e && nqp::p6bool(pir::stat__Isi(nqp::unbox_s($!path), pir::const::STAT_ISDIR))
    }

    method e() {
        nqp::p6bool(pir::stat__Isi(nqp::unbox_s($!path), pir::const::STAT_EXISTS))
    }

    method f() {
        self.e && nqp::p6bool(pir::stat__Isi(nqp::unbox_s($!path), pir::const::STAT_ISREG))
    }

    method l() {
        nqp::p6bool(pir::new__Ps('File').is_link(nqp::unbox_s($!path)))
    }

    method r() {
        nqp::p6bool(pir::new__Ps('OS').can_read(nqp::unbox_s($!path)))
    }

    method s() {
        self.e 
          && nqp::p6bool(
              nqp::isgt_i(
                  pir::stat__Isi(nqp::unbox_s($!path), 
                                 pir::const::STAT_FILESIZE),
                  0))
    }

    method t() {
        self.opened && nqp::p6bool($!PIO.isatty)
    }

    method w() {
        nqp::p6bool(pir::new__Ps('OS').can_write(nqp::unbox_s($!path)))
    }

    method x() {
        nqp::p6bool(pir::new__Ps('OS').can_execute(nqp::unbox_s($!path)))
    }
    
}

sub unlink($path) {
    try {
        pir::new__PS('OS').rm($path);
    }
    $! ?? fail($!) !! Bool::True
}

proto sub open(|$) { * }
multi sub open($path, :$r, :$w, :$a, :$bin, :$chomp = Bool::True) {
    IO.new.open($path, :$r, :$w, :$a, :$bin, :$chomp);
}

proto sub lines(|$) { * }
multi sub lines($fh = $*ARGFILES, $limit = $Inf) { 
    $fh.lines($limit) 
}

proto sub get(|$) { * }
multi sub get($fh = $*ARGFILES) {
    $fh.get()
}

proto sub close(|$) { * }
multi sub close($fh) {
    $fh.close()
}

proto sub slurp(|$) { * }
multi sub slurp($filename) {
    my $handle = open($filename, :r);
    my $contents = $handle.slurp();
    $handle.close();
    $contents
}

proto sub cwd(|$) { * }
multi sub cwd() {
    my $pwd;
    try {
        $pwd = pir::new__Ps('OS').cwd();
    }
    $! ?? fail($!) !! $pwd;
}

sub dir($path = '.', Mu :$test = none('.', '..')) {
    my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s($path.Stringy));
    my int $elems = pir::set__IP($RSA);
    my @res;
    loop (my int $i = 0; $i < $elems; $i = $i + 1) {
        my Str $item := nqp::p6box_s(nqp::atpos($RSA, $i));
        @res.push: $item if $test.ACCEPTS($item);
    }
    @res;

}

proto sub chdir(|$) { * }
multi sub chdir($path as Str) {
    try {
        pir::new__PS('OS').chdir($path)
    }
    $! ?? fail($!) !! True
}

proto sub mkdir(|$) { * }
multi sub mkdir($path as Str, $mode = 0o777) {
    try {
        pir::new__PS('OS').mkdir($path, $mode)
    }
    $! ?? fail($!) !! True
}

$PROCESS::IN  = open('-');
$PROCESS::OUT = open('-', :w);
$PROCESS::ERR = IO.new;
nqp::bindattr(nqp::p6decont($PROCESS::ERR),
        IO, '$!PIO', pir::getstderr__P());


