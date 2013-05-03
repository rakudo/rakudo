my role IO { }

sub print(|) {
    my $args := pir::perl6_current_args_rpa__P();
    $*OUT.print(nqp::shift($args)) while $args;
    Bool::True
}

sub say(|) {
    my $args := pir::perl6_current_args_rpa__P();
    $*OUT.print(nqp::shift($args).gist) while $args;
    $*OUT.print("\n");
}

sub note(|) {
    my $args := pir::perl6_current_args_rpa__P();
    $*ERR.print(nqp::shift($args).gist) while $args;
    $*ERR.print("\n");
}

sub gist(|) {
    nqp::p6parcel(pir::perl6_current_args_rpa__P(), Mu).gist
}

sub prompt($msg) {
    print $msg;
    $*OUT.flush();
    $*IN.get;
}

my role IO::FileTestable does IO {
    method d() {
        self.e && nqp::p6bool(nqp::stat(nqp::unbox_s(self.Str), nqp::const::STAT_ISDIR))
    }

    method e() {
        nqp::p6bool(nqp::stat(nqp::unbox_s(self.Str), nqp::const::STAT_EXISTS))
    }

    method f() {
        self.e && nqp::p6bool(nqp::stat(nqp::unbox_s(self.Str), nqp::const::STAT_ISREG))
    }

    method l() {
        nqp::p6bool(pir::new__Ps('File').is_link(nqp::unbox_s(self.Str)))
    }

    method r() {
        nqp::p6bool(pir::new__Ps('OS').can_read(nqp::unbox_s(self.Str)))
    }

    method s() {
        self.e 
          && nqp::p6box_i( nqp::stat(nqp::unbox_s(self.Str), 
                                 nqp::const::STAT_FILESIZE) );
    }

    method w() {
        nqp::p6bool(pir::new__Ps('OS').can_write(nqp::unbox_s(self.Str)))
    }

    method x() {
        nqp::p6bool(pir::new__Ps('OS').can_execute(nqp::unbox_s(self.Str)))
    }
    
    method z() {
        self.e && self.s == 0;
    }

    method modified() {
         nqp::p6box_i(nqp::stat(nqp::unbox_s(self.Str), nqp::const::STAT_MODIFYTIME));
    }

    method accessed() {
         nqp::p6box_i(nqp::stat(nqp::unbox_s(self.Str), nqp::const::STAT_ACCESSTIME));
    }

    method changed() { 
         nqp::p6box_i(nqp::stat(nqp::unbox_s(self.Str), nqp::const::STAT_CHANGETIME));
    }
}

my class IO::Handle does IO::FileTestable {
    has $!PIO;
    has Int $.ins = 0;
    has $.chomp = Bool::True;
    has $.path;

    proto method open(|) { * }
    multi method open($path? is copy, :$r, :$w, :$a, :$p, :$bin, :$chomp = Bool::True,
            :enc(:$encoding) = 'utf8') {
        $path //= $.path;
        my $mode =  $p ?? ($w ||  $a ?? 'wp' !! 'rp') !!
                   ($w ?? 'w' !! ($a ?? 'wa' !! 'r' ));
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO::Handle, '$!PIO',
             $path eq '-'
                ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                !! nqp::open(nqp::unbox_s($path.Str), nqp::unbox_s($mode))
        );
        $!path = $path;
        $!chomp = $chomp;
        $!PIO.encoding($bin ?? 'binary' !! PARROT_ENCODING($encoding));
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
        unless nqp::defined($!PIO) {
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

    method read(IO::Handle:D: Cool:D $bytes as Int) {
        my Mu $parrot_buffer := $!PIO.read_bytes(nqp::unbox_i($bytes));
        my $buf := nqp::create(Buf);
        nqp::bindattr_s($buf, Buf, '$!buffer', $parrot_buffer.get_string('binary'));
        $buf;
    }
    # first arguemnt should probably be an enum
    # valid values for $whence:
    #   0 -- seek from beginning of file
    #   1 -- seek relative to current position
    #   2 -- seek from the end of the file
    method seek(IO::Handle:D: Int:D $offset, Int:D $whence) {
        $!PIO.seek(nqp::unbox_i($whence), nqp::unbox_i($offset));
        True;
    }
    method tell(IO::Handle:D:) returns Int {
        nqp::p6box_i($!PIO.tell);
    }

    method write(IO::Handle:D: Buf:D $buf) {
        my str $b = nqp::getattr_s(
                        nqp::p6decont($buf),
                        Buf,
                        '$!buffer'
                    );
        my str $encoding = $!PIO.encoding;
        $!PIO.encoding('binary');
        $!PIO.print($b);
        $!PIO.encoding($encoding) unless $encoding eq 'binary';
        True;
    }

    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t() {
        self.opened && nqp::p6bool($!PIO.isatty)
    }


    proto method print(|) { * }
    multi method print(IO::Handle:D: Str:D $value) {
        $!PIO.print(nqp::unbox_s($value));
        Bool::True
    }
    multi method print(IO::Handle:D: *@list) {
        $!PIO.print(nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    multi method say(IO::Handle:D: |) {
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print: "\n";
    }
    
    method slurp() {
        nqp::p6box_s($!PIO.readall());
    }


    # not spec'd
    method copy($dest) {
        try {
            nqp::copy(nqp::unbox_s(~$.path), nqp::unbox_s(~$dest));
        }
        $! ?? fail(X::IO::Copy.new(from => $.path, to => $dest, os-error => ~$!)) !! True
    }

    method chmod($mode) {
        nqp::chmod(nqp::unbox_s(~$.path), nqp::unbox_i($mode.Int));
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

    method Str {
        $.path
    }

    method flush() {
        fail("File handle not open, so cannot flush")
            unless nqp::defined($!PIO);
        $!PIO.flush();
        True;
    }
}

my class IO::Path is Cool does IO::FileTestable {
    method SPEC { IO::Spec.MODULE };
    has Str $.basename;
    has Str $.directory = '.';
    has Str $.volume = '';

    method dir() {
        die "IO::Path.dir is deprecated in favor of .directory";
    }
    submethod BUILD(:$!basename, :$!directory, :$!volume, :$dir) {
        die "Named paramter :dir in IO::Path.new deprecated in favor of :directory"
            if defined $dir;
    }

    multi method new(Str:D $path) {
         self.new( |$.SPEC.split($path).hash );
    }

    multi method Str(IO::Path:D:) {
        $.SPEC.join($.volume, $.directory, $.basename);
    }
    multi method gist(IO::Path:D:) {
        "{self.^name}<{self.Str}>";
    }
    multi method Numeric(IO::Path:D:) {
        self.basename.Numeric;
    }
    method Bridge(IO::Path:D:) {
        self.basename.Bridge;
    }
    method Int(IO::Path:D:) {
        self.basename.Int;
    }

    method path(IO::Path:D:) {
        self;
    }

    method IO(IO::Path:D: *%opts) {
        IO::Handle.new(:path(~self), |%opts);
    }
    method open(IO::Path:D: *%opts) {
        open(~self, |%opts);
    }
    method contents(IO::Path:D: *%opts) {
        dir(~self, |%opts);
    }

    method is-absolute {
        $.SPEC.is-absolute(~self);
    }
    method is-relative {
        ! $.SPEC.is-absolute(~self);
    }
    method absolute ($base = $*CWD) {
        return self.new($.SPEC.rel2abs(~self, $base))
    }
    method relative ($relative_to_directory = $*CWD) {
        return self.new($.SPEC.abs2rel(~self, $relative_to_directory));
    }

    method cleanup {
        return self.new($.SPEC.canonpath(~self));
    }
    method resolve {
        fail "Not Yet Implemented: requires readlink()";
    }

    method parent {
        if self.is-absolute {
            return self.new($.SPEC.join($.volume, $.directory, ''));
        }
        elsif all($.basename, $.directory) eq $.SPEC.curdir {
            return self.new(:$.volume, directory=>$.SPEC.curdir,
                             basename=>$.SPEC.updir);
        }
        elsif $.basename eq $.SPEC.updir && $.directory eq $.SPEC.curdir 
           or !grep({$_ ne $.SPEC.updir}, $.SPEC.splitdir($.directory)) {  
            return self.new(    # If all updirs, then add one more
                :$.volume,
                directory => $.SPEC.catdir($.directory, $.SPEC.updir),
                :$.basename );
        }
        else {
            return self.new( $.SPEC.join($.volume, $.directory, '') );
        }
    }

    method child ($childname) {
        self.new($.SPEC.join: $.volume,
                              $.SPEC.catdir($.directory, $.basename),
                              $childname);
    }

}

my class IO::Path::Unix   is IO::Path { method SPEC { IO::Spec::Unix   };  }
my class IO::Path::Win32  is IO::Path { method SPEC { IO::Spec::Win32  };  }
my class IO::Path::Cygwin is IO::Path { method SPEC { IO::Spec::Cygwin };  }


sub dir(Cool $path = '.', Mu :$test = none('.', '..')) {
    my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s($path.Str));
    my int $elems = nqp::elems($RSA);
    my @res;
    my ($directory, $volume) = IO::Spec.splitpath(~$path, :nofile);
    loop (my int $i = 0; $i < $elems; $i = $i + 1) {
        my Str $file := nqp::p6box_s(pir::trans_encoding__Ssi(
			nqp::atpos_s($RSA, $i),
			pir::find_encoding__Is('utf8')));
        if $file ~~ $test {
            #this should be like IO::Path.child(:basename($file)) because of :volume
            @res.push: IO::Path.new(:basename($file), :$directory, :$volume);
        }
    }
    return @res.list;

    CATCH {
        default {
            X::IO::Dir.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
}

sub unlink($path) {
    nqp::unlink($path);
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

sub rmdir($path) {
    nqp::rmdir($path);
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

proto sub open(|) { * }
multi sub open($path, :$r, :$w, :$a, :$p, :$bin, :$chomp = Bool::True, :enc(:$encoding) = 'utf8') {
    IO::Handle.new.open($path, :$r, :$w, :$a, :$p, :$bin, :$chomp, :$encoding);
}

proto sub lines(|) { * }
multi sub lines($fh = $*ARGFILES, $limit = $Inf) { 
    $fh.lines($limit) 
}

proto sub get(|) { * }
multi sub get($fh = $*ARGFILES) {
    $fh.get()
}

proto sub getc(|) { * }
multi sub getc($fh = $*ARGFILES) {
    $fh.getc()
}

proto sub close(|) { * }
multi sub close($fh) {
    $fh.close()
}

proto sub slurp(|) { * }
multi sub slurp($filename, :$bin = False) {
    my $handle = open($filename, :r, :$bin);
    if $bin {
        my $Buf = Buf.new();
        loop {
            my $current  = $handle.read(10_000);
            $Buf ~= $current;
            last if $current.bytes == 0;
        }
        $handle.close;
        $Buf;
    }
    else {
        my $contents = $handle.slurp();
        $handle.close();
        $contents
    }
}

multi sub slurp(IO::Handle $io = $*ARGFILES) {
    $io.slurp;
}

proto sub spurt(|) { * }
multi sub spurt(Cool $filename,
                Cool $contents,
                :encoding(:$enc) = 'utf8',
                :$createonly,
                :$append) {
    fail("File '$filename' already exists, but :createonly was give to spurt")
        if $createonly && $filename.IO.e;
    my $mode = $append ?? :a !! :w;
    my $fh = open($filename.Str, :$enc, |$mode);
    $fh.print($contents);
    $fh.close;
}
multi sub spurt(Cool $filename,
                Buf $contents,
                :$createonly,
                :$append) {
    fail("File '$filename' already exists, but :createonly was give to spurt")
        if $createonly && $filename.IO.e;
    my $mode = $append ?? :a !! :w;
    my $fh = open($filename.Str, :bin, |$mode);
    $fh.write($contents);
    $fh.close;
}

proto sub cwd(|) { * }
multi sub cwd() {
    return nqp::p6box_s(
		pir::trans_encoding__Ssi(
			nqp::cwd(),
			pir::find_encoding__Is('utf8')));

    CATCH {
        default {
            X::IO::Cwd.new(
                os-error => .Str,
            ).throw;
        }
    }
}


proto sub chdir(|) { * }
multi sub chdir($path as Str) {
    nqp::chdir(nqp::unbox_s($path));
    $*CWD = cwd();
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

proto sub mkdir(|) { * }
multi sub mkdir($path as Str, $mode = 0o777) {
    nqp::mkdir($path, $mode);
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
$PROCESS::ERR = IO::Handle.new;
nqp::bindattr(nqp::p6decont($PROCESS::ERR),
        IO::Handle, '$!PIO', nqp::getstderr());

sub rename(Cool $from as Str, Cool $to as Str) {
    nqp::rename(nqp::unbox_s($from), nqp::unbox_s($to));
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
sub copy(Cool $from as Str, Cool $to as Str) {
    nqp::copy(nqp::unbox_s($from), nqp::unbox_s($to));
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
sub symlink(Cool $target as Str, Cool $name as Str) {
    nqp::symlink(nqp::unbox_s($target), nqp::unbox_s($name));
    return True;
    CATCH {
        default {
            X::IO::Symlink.new(
                :$target,
                :$name,
                os-error => .Str,
            ).throw;
        }
    }
}
sub link(Cool $target as Str, Cool $name as Str) {
    nqp::link(nqp::unbox_s($target), nqp::unbox_s($name));
    return True;
    CATCH {
        default {
            X::IO::Link.new(
                :$target,
                :$name,
                os-error => .Str,
            ).throw;
        }
    }
}

sub chmod($mode, $filename) { $filename.IO.chmod($mode); $filename }
