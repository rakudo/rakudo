sub print(|$) {
    my $args := pir::perl6_current_args_rpa__P();
    $*OUT.print(nqp::shift($args)) while $args;
    Bool::True
}

sub say(|$) {
    my $args := pir::perl6_current_args_rpa__P();
    $*OUT.print(nqp::shift($args).gist) while $args;
    $*OUT.print("\n");
}

sub note(|$) {
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
        unless pir::defined($!PIO) {
            self.open($.path, :chomp($.chomp));
        }
        return Str if self.eof;
        my Str $x = nqp::p6box_s($!PIO.readline);
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x.=chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins++;
        $x;
    }
    
    method getc() {
        unless $!PIO {
            self.open($.path, :chomp($.chomp));
        }
        my $c = nqp::p6box_s($!PIO.read(1));
        fail if $c eq '';
        $c;
    }

    method lines($limit = $Inf) {
        my $count = 0;
        gather while (my $line = self.get).defined && ++$count <= $limit {
            take $line;
        }
    }

    method read(IO:D: Cool:D $bytes as Int) {
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
        my str $encoding = $!PIO.encoding;
        $!PIO.encoding('binary');
        $!PIO.print($b.get_string('binary'));
        $!PIO.encoding($encoding) unless $encoding eq 'binary';
        True;
    }

    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    proto method print(|$) { * }
    multi method print(IO:D: Str:D $value) {
        $!PIO.print(nqp::unbox_s($value));
        Bool::True
    }
    multi method print(IO:D: *@list) {
        $!PIO.print(nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    multi method say(IO:D: |$) {
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
    
    method z() {
        self.e && self.s == 0;
    }

    # not spec'd
    method copy($dest) {
        if self.d() {
            die "Cannot copy '$.path': Is a directory"
        }
        try {
            pir::new__PS('File').copy(nqp::unbox_s(~$.path), nqp::unbox_s(~$dest));
        }
        $! ?? fail($!) !! True
    }

    my class X::IO::Chmod { ... }
    method chmod($mode) {
        pir::new__PS('OS').chmod(nqp::unbox_s(~$.path), nqp::unbox_i($mode.Int));
        return True;
        CATCH {
            default {
                X::IO::Chmod.new(
                    :$.path,
                    :$mode,
                    os-error => .Str,
                ).throw;
            }
        }
    }
}

my class X::IO::Unlink { ... }
sub unlink($path) {
    pir::new__PS('OS').unlink($path);
    return True;
    CATCH {
        default {
            X::IO::Unlink.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
}

my class X::IO::Rmdir { ... }
sub rmdir($path) {
    pir::new__PS('OS').rmdir($path);
    return True;
    CATCH {
        default {
            X::IO::Rmdir.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
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

proto sub getc(|$) { * }
multi sub getc($fh = $*ARGFILES) {
    $fh.getc()
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
multi sub slurp(IO $io = $*ARGFILES) {
    $io.slurp;
}

my class X::IO::Cwd { ... }
proto sub cwd(|$) { * }
multi sub cwd() {
    return pir::new__Ps('OS').cwd();

    CATCH {
        default {
            X::IO::Cwd.new(
                os-error => .Str,
            ).throw;
        }
    }
}

my class X::IO::Dir { ... }
sub dir($path = '.', Mu :$test = none('.', '..')) {
    my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s($path.Stringy));
    my int $elems = pir::set__IP($RSA);
    my @res;
    loop (my int $i = 0; $i < $elems; $i = $i + 1) {
        my Str $item := nqp::p6box_s(nqp::atpos($RSA, $i));
        @res.push: $item if $test.ACCEPTS($item);
    }
    return @res;

    CATCH {
        default {
            X::IO::Dir.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
}

my class X::IO::Chdir { ... }
proto sub chdir(|$) { * }
multi sub chdir($path as Str) {
    pir::new__PS('OS').chdir(nqp::unbox_s($path));
    $*CWD = nqp::p6box_s(pir::new__PS('OS').cwd);
    return True;
    CATCH {
        default {
            X::IO::Chdir.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
}

my class X::IO::Mkdir { ... }
proto sub mkdir(|$) { * }
multi sub mkdir($path as Str, $mode = 0o777) {
    pir::new__PS('OS').mkdir($path, $mode);
    return True;
    CATCH {
        default {
            X::IO::Mkdir.new(
                :$path,
                :$mode,
                os-error => .Str,
            ).throw;
        }
    }
}

$PROCESS::IN  = open('-');
$PROCESS::OUT = open('-', :w);
$PROCESS::ERR = IO.new;
nqp::bindattr(nqp::p6decont($PROCESS::ERR),
        IO, '$!PIO', pir::getstderr__P());

my class X::IO::Rename { ... }
sub rename(Cool $from as Str, Cool $to as Str) {
    pir::new__PS('OS').rename(nqp::unbox_s($from), nqp::unbox_s($to));
    return True;
    CATCH {
        default {
            if .Str ~~ /'rename failed: '(.*)/ {
                X::IO::Rename.new(
                    :$from,
                    :$to,
                    os-error => $0.Str,
                ).throw;
            } else {
                die "Unexpected error: $_";
            }
        }
    }
}
my class X::IO::Copy { ... }
sub copy(Cool $from as Str, Cool $to as Str) {
    pir::new__PS('File').copy(nqp::unbox_s($from), nqp::unbox_s($to));
    return True;
    CATCH {
        default {
            X::IO::Copy.new(
                :$from,
                :$to,
                os-error => .Str,
            ).throw;
        }
    }
}

