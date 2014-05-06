my role IO { }

sub print(|) {
    my $args := nqp::p6argvmarray();
    $*OUT.print(nqp::shift($args)) while $args;
    Bool::True
}

sub say(|) {
    my $args := nqp::p6argvmarray();
    $*OUT.print(nqp::shift($args).gist) while $args;
    $*OUT.print("\n");
}

sub note(|) {
    my $args := nqp::p6argvmarray();
    $*ERR.print(nqp::shift($args).gist) while $args;
    $*ERR.print("\n");
}

sub gist(|) {
    nqp::p6parcel(nqp::p6argvmarray(), Mu).gist
}

sub prompt($msg) {
    print $msg;
    $*OUT.flush();
    $*IN.get;
}

my role IO::FileTestable does IO {
    method d() {
        self.e && nqp::p6bool(nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)), 
                                        nqp::const::STAT_ISDIR))
    }

    method e() {
        nqp::p6bool(nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)), 
                              nqp::const::STAT_EXISTS))
    }

    method f() {
        self.e && nqp::p6bool(nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)), 
                              nqp::const::STAT_ISREG))
    }

    method s() {
        self.e
          && nqp::p6box_i( nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)),
                                 nqp::const::STAT_FILESIZE) );
    }

    method l() {
        nqp::p6bool(nqp::fileislink(IO::Spec.rel2abs(self.Str)))
    }

    method r() {
        nqp::p6bool(nqp::filereadable(IO::Spec.rel2abs(self.Str)))
    }

    method w() {
        nqp::p6bool(nqp::filewritable(IO::Spec.rel2abs(self.Str)))
    }

    method x() {
        nqp::p6bool(nqp::fileexecutable(IO::Spec.rel2abs(self.Str)))
    }

    method z() {
        self.f && self.s == 0;
    }

    method modified() {
         nqp::p6box_i(nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)),
                                nqp::const::STAT_MODIFYTIME));
    }

    method accessed() {
         nqp::p6box_i(nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)),
                                nqp::const::STAT_ACCESSTIME));
    }

    method changed() { 
         nqp::p6box_i(nqp::stat(nqp::unbox_s(IO::Spec.rel2abs(self.Str)),
                                nqp::const::STAT_CHANGETIME));
    }
}

my class IO::Handle does IO::FileTestable {
    has $!PIO;
    has Int $.ins = 0;
    has $.chomp = Bool::True;
    has $.path;

    proto method open(|) { * }
    multi method open($path? is copy, :$r is copy, :$w is copy, :$rw, :$a, :$p, :$bin, :$chomp = Bool::True,
            :enc(:$encoding) = 'utf8') {
        $path //= $!path;
        $r = $w = True if $rw;
        my $abspath = defined($*CWD) ?? IO::Spec.rel2abs($path) !! $path;
#?if parrot
        my $mode =  $p ?? ($w ||  $a ?? 'wp' !! 'rp') !!
                   ($w ?? 'w' !! ($a ?? 'wa' !! 'r' ));
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO::Handle, '$!PIO',
             $path eq '-'
                ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                !! nqp::open(nqp::unbox_s($abspath.Str), nqp::unbox_s($mode))
        );
#?endif
#?if !parrot
        if $p {
            #~ my $mode =  $p ?? ($w ||  $a ?? 'wp' !! 'rp');

            my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
            my Mu $hash-without         := nqp::hash();
            my Mu $enviter := nqp::iterator($hash-with-containers);
            my $envelem;
            while $enviter {
                $envelem := nqp::shift($enviter);
                nqp::bindkey($hash-without, nqp::iterkey_s($envelem), nqp::decont(nqp::iterval($envelem)))
            }

            my $errpath = '';
            nqp::bindattr(self, IO::Handle, '$!PIO',
                 $path eq '-'
                    ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                    !! nqp::openpipe(nqp::unbox_s($abspath.Str), nqp::unbox_s($*CWD.Str), $hash-without, nqp::unbox_s($errpath))
            );
        }
        else {
            my $mode =  $w ?? 'w' !! ($a ?? 'wa' !! 'r' );
            # TODO: catch error, and fail()
            nqp::bindattr(self, IO::Handle, '$!PIO',
                 $path eq '-'
                    ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                    !! nqp::open(nqp::unbox_s($abspath.Str), nqp::unbox_s($mode))
            );
        }
#?endif
        $!path = $path;
        $!chomp = $chomp;
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($encoding)) unless $bin;
        self;
    }

    method close() {
        # TODO:b catch errors
        nqp::closefh($!PIO) if nqp::defined($!PIO);
        $!PIO := Mu;
        Bool::True;
    }

    method eof() {
        nqp::p6bool(nqp::eoffh($!PIO));
    }

    method get() {
        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        return Str if self.eof;
        my Str $x = nqp::p6box_s(nqp::readlinefh($!PIO));
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x.=chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins++;
        $x;
    }
    
    method getc() {
        unless $!PIO {
            self.open($!path, :chomp($.chomp));
        }
        my $c = nqp::p6box_s(nqp::getcfh($!PIO));
        fail if $c eq '';
        $c;
    }

    method lines($limit = $Inf) {
        if $limit == $Inf {
            gather while nqp::p6definite(my $line = self.get) {
                take $line;
            }
        }
        else {
            my $count = 0;
            gather while ++$count <= $limit && nqp::p6definite(my $line = self.get) {
                take $line;
            }
        }
    }

    method read(IO::Handle:D: Cool:D $bytes as Int) {
        my $buf := buf8.new();
#?if parrot
        # Relies on nqp::encode passing the binary encoding straight on down
        # to Parrot.
        my Mu $parrot_buffer := $!PIO.read_bytes(nqp::unbox_i($bytes));
        nqp::encode($parrot_buffer.get_string('binary'), 'binary', $buf);
#?endif
#?if !parrot
        nqp::readfh($!PIO, $buf, nqp::unbox_i($bytes));
#?endif
        $buf;
    }
    # second arguemnt should probably be an enum
    # valid values for $whence:
    #   0 -- seek from beginning of file
    #   1 -- seek relative to current position
    #   2 -- seek from the end of the file
    method seek(IO::Handle:D: Int:D $offset, Int:D $whence) {
#?if moar
        nqp::seekfh($!PIO, $offset, $whence);
#?endif
#?if !moar
        $!PIO.seek(nqp::unbox_i($whence), nqp::unbox_i($offset));
#?endif
        True;
    }
    method tell(IO::Handle:D:) returns Int {
        nqp::p6box_i(
#?if parrot
            $!PIO.tell
#?endif
#?if !parrot
            nqp::tellfh($!PIO)
#?endif
        );
    }

    method write(IO::Handle:D: Blob:D $buf) {
#?if parrot
        # This relies on the Parrot 'binary' encoding and that nqp::decode
        # passes encoding straight down to Parrot.
        my str $encoding = $!PIO.encoding;
        $!PIO.encoding('binary');
        $!PIO.print(nqp::decode(nqp::decont($buf), 'binary'));
        $!PIO.encoding($encoding) unless $encoding eq 'binary';
#?endif
#?if !parrot
        nqp::writefh($!PIO, nqp::decont($buf));
#?endif
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
        nqp::printfh($!PIO, nqp::unbox_s($value));
        Bool::True
    }
    multi method print(IO::Handle:D: *@list) {
        nqp::printfh($!PIO, nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    multi method say(IO::Handle:D: |) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print: "\n";
    }
    
    method slurp(:$bin, :enc($encoding)) {
        self.open(:r, :$bin) unless self.opened;
        self.encoding($encoding) if $encoding.defined;

        if $bin {
            my $Buf = buf8.new();
            loop {
                my $current  = self.read(10_000);
                $Buf ~= $current;
                last if $current.bytes == 0;
            }
            self.close;
            $Buf;
        }
        else {
            my $contents = nqp::p6box_s(nqp::readallfh($!PIO));
            self.close();
            $contents
        } 
    }

    proto method spurt(|) { * }
    multi method spurt(Cool $contents,
                    :encoding(:$enc) = 'utf8',
                    :$createonly, :$append) {
    
        fail("File '" ~ self.path ~ "' already exists, but :createonly was give to spurt")
            if $createonly && self.e;
        
        if self.opened {
            self.encoding($enc);
        } else {
            my $mode = $append ?? :a !! :w;
            self.open(:$enc, |$mode);
        }
        self.print($contents);
        self.close;
    }
    
    multi method spurt(Blob $contents,
                    :$createonly,
                    :$append) {
        fail("File '" ~ self.path ~ "' already exists, but :createonly was give to spurt")
                if $createonly && self.e;
        
        unless self.opened {
            my $mode = $append ?? :a !! :w;
            self.open(:bin, |$mode);
        }
        self.write($contents);
        self.close;
    }

    # not spec'd
    method copy($dest) {
        warn "IO::Handle.copy is deprecated.  Please use IO::Path.copy instead.";
        try {
            nqp::copy(nqp::unbox_s(IO::Spec.rel2abs(~$!path)), 
                      nqp::unbox_s(IO::Spec.rel2abs(~$dest)));
        }
        $! ?? fail(X::IO::Copy.new(from => $!path, to => $dest, os-error => ~$!)) !! True
    }

    method chmod(Int $mode) {
        self.path.absolute.chmod($mode)
    }

    method IO { self }

    method path {  IO::Path.new($!path)  }

    multi method Str (IO::Handle:D:) {  $!path  }

    multi method gist (IO::Handle:D:) {
        self.opened
            ?? "IO::Handle<$!path>(opened, at line {$.ins} / octet {$.tell})"
            !! "IO::Handle<$!path>(closed)"
    }

    multi method perl (IO::Handle:D:) {
        "IO::Handle.new(path => {$!path.perl}, ins => {$!ins.perl}, chomp => {$!chomp.perl})"
    }


    method flush() {
        fail("File handle not open, so cannot flush")
            unless nqp::defined($!PIO);
        nqp::flushfh($!PIO);
        True;
    }

    method encoding($enc?) {
        $enc.defined
            ?? nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc))
            !! $!PIO.encoding
    }
}

#?if moar
my class IO::Notification { ... }
#?endif

my class IO::Path is Cool does IO::FileTestable {
    method SPEC { IO::Spec.MODULE };
    has Str $.path;

    method dir() {
        die "IO::Path.dir is deprecated in favor of .directory";
    }

    multi method ACCEPTS(IO::Path:D: IO::Path:D \other) {
        self.cleanup.parts eqv other.cleanup.parts
    }

    multi method ACCEPTS(IO::Path:D: Mu \other) {
        self.cleanup.parts eqv IO::Path.new(|other).cleanup.parts
    }

    submethod BUILD(:$!path!, :$dir) {
        die "Named paramter :dir in IO::Path.new deprecated in favor of :directory"
            if defined $dir;
    }

    multi method new(:$basename!, :$directory = '.', :$volume = '') {
        self.new: path=>$.SPEC.join($volume, $directory, $basename);
    }

    multi method new(Str:D $path) {
        self.new(:$path)
    }

    method path(IO::Path:D:) {
        self;
    }

    method parts {
        $.SPEC.split($!path).hash
    }
    method basename {
        self.parts<basename>
    }
    method directory {
        self.parts<directory>
    }
    method volume {
        self.parts<volume>
    }

    multi method Str(IO::Path:D:) {
         $!path;
    }
    multi method gist(IO::Path:D:) {
        "{self.^name}<{ $!path }>";
    }
    multi method perl(IO::Path:D:) {
         "IO::Path.new(path => " ~ $.Str.perl ~ ")";
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

    method succ(IO::Path:D:) {
        self.new(:$.volume, :$.directory, basename=> $.basename.succ)
    }
    method pred(IO::Path:D:) {
        self.new(:$.volume, :$.directory, basename=> $.basename.pred)
    }

    method IO(IO::Path:D: *%opts) {
        IO::Handle.new(:$!path, |%opts);
    }
    method open(IO::Path:D: *%opts) {
        open($!path, |%opts);
    }

#?if moar
    method watch(IO::Path:D:) {
        IO::Notification.watch_path($!path);
    }
#?endif

    method is-absolute {
        $.SPEC.is-absolute($!path);
    }
    method is-relative {
        ! $.SPEC.is-absolute($!path);
    }
    method absolute ($base = ~$*CWD) {
        return self.new($.SPEC.rel2abs($!path, $base));
    }
    method relative ($relative_to_directory = ~$*CWD) {
        return self.new($.SPEC.abs2rel($!path, $relative_to_directory));
    }

    method cleanup (:$parent) {
        return self.new($.SPEC.canonpath($!path, :$parent));
    }
    method resolve {
        # NYI: requires readlink()
        X::NYI.new(feature=>'IO::Path.resolve').fail;
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
        self.new: path => $.SPEC.catfile($!path, $childname);
    }

    method copy(IO::Path:D: $dest, :$createonly = False) {
        my $absdest = IO::Spec.rel2abs($dest);
        if $createonly and $absdest.e {
            fail(X::IO::Copy.new(from => $!path, to => $dest,
                    os-error => "Destination file $dest exists and :createonly passed to copy."));
        }
        try {
            nqp::copy(nqp::unbox_s(IO::Spec.rel2abs($!path)), nqp::unbox_s(~$absdest));
        }
        $! ?? fail(X::IO::Copy.new(from => $!path, to => $dest, os-error => ~$!)) !! True
    }

    method chmod(IO::Path:D: Int $mode) {
        nqp::chmod(nqp::unbox_s(IO::Spec.rel2abs($!path)), nqp::unbox_i($mode.Int));
        return True;
        CATCH {
            default {
                X::IO::Chmod.new(
                    :$!path,
                    :$mode,
                    os-error => .Str,
                ).throw;
            }
        }
    }

    method contents(IO::Path:D: Mu :$test = none('.', '..')) {

        CATCH {
            default {
                X::IO::Dir.new(
                    :$!path,
                    os-error => .Str,
                ).throw;
            }
        }
#?if parrot
        my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s(self.absolute.Str));
        my int $elems = nqp::elems($RSA);
        gather loop (my int $i = 0; $i < $elems; $i = $i + 1) {
            my Str $file := nqp::p6box_s(pir::trans_encoding__Ssi(
              nqp::atpos_s($RSA, $i),
              pir::find_encoding__Is('utf8')));
            if $file ~~ $test {
                take self.child($file);
            }
        }
#?endif
#?if !parrot
        my Mu $dirh := nqp::opendir(self.absolute.Str);
        my $next = 1;
        gather {
#?endif
#?if jvm
            take $_.path if $_ ~~ $test for ".", "..";
#?endif
#?if !parrot
            loop {
                my Str $elem := nqp::nextfiledir($dirh);
                if nqp::isnull_s($elem) || !$elem {
                    nqp::closedir($dirh);
                    last;
                } else {
#?endif
#?if jvm
                    # jvm's nextfiledir gives us absolute paths back, moar does not.
                    $elem := $elem.substr($*CWD.chars + 1) if self.is-relative;
#?endif
#?if moar
                    next unless $elem ~~ $test;
                    $elem := $.SPEC.catfile($!path, $elem) if self ne '.';
#?endif
#?if !parrot
                    if $elem.substr(0, 2) eq any("./", ".\\") {
                        $elem := $elem.substr(2);
                    }
                    take $elem.path if $elem ~~ $test;
                }
            }
        }
#?endif
    }

}

my class IO::Path::Unix   is IO::Path { method SPEC { IO::Spec::Unix   };  }
my class IO::Path::Win32  is IO::Path { method SPEC { IO::Spec::Win32  };  }
my class IO::Path::Cygwin is IO::Path { method SPEC { IO::Spec::Cygwin };  }
my class IO::Path::QNX    is IO::Path { method SPEC { IO::Spec::QNX    };  }


sub dir(Cool $path = '.', Mu :$test = none('.', '..')) {
    $path.path.contents(:$test)
}

sub unlink($path as Str) {
    my $abspath = IO::Spec.rel2abs($path);
    nqp::unlink($abspath);
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

sub rmdir($path as Str) {
    my $abspath = IO::Spec.rel2abs($path);
    nqp::rmdir($abspath);
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
multi sub open($path, :$r is copy, :$w is copy, :$rw, :$a, :$p, :$bin, :$chomp = Bool::True, :enc(:$encoding) = 'utf8') {
    IO::Handle.new.open($path, :$r, :$w, :$rw, :$a, :$p, :$bin, :$chomp, :$encoding);
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
multi sub slurp($filename, :$bin = False, :$enc = 'utf8') {
    $filename.IO.slurp(:$bin, :$enc);
}

multi sub slurp(IO::Handle $io = $*ARGFILES, :$bin, :$enc) {
    $io.slurp(:$bin, :$enc);
}

proto sub spurt(|) { * }
multi sub spurt(IO::Handle $fh,
                Cool $contents,
                :encoding(:$enc) = 'utf8',
                :$createonly,
                :$append) {
    $fh.spurt($contents, :$enc, :$createonly, :$append);
}
multi sub spurt(IO::Handle $fh,
                Blob $contents,
                :$createonly,
                :$append) {
    $fh.spurt($contents, :$createonly, :$append);
}

multi sub spurt(Cool $filename,
                Cool $contents,
                :encoding(:$enc) = 'utf8',
                :$createonly,
                :$append) {
    $filename.IO.spurt($contents, :$enc, :$createonly, :$append);
}

multi sub spurt(Cool $filename,
                Blob $contents,
                :$createonly,
                :$append) {
    $filename.IO.spurt($contents, :$createonly, :$append);
}

{
    proto sub cwd(|) { * }
    multi sub cwd() {
        return nqp::p6box_s(
#?if parrot
            pir::trans_encoding__Ssi(
                nqp::cwd(),
                pir::find_encoding__Is('utf8'))
#?endif
#?if !parrot
            nqp::cwd(),
#?endif
        );
        CATCH {
            default {
                X::IO::Cwd.new(
                    os-error => .Str,
                ).throw;
            }
        }
    }
    PROCESS::<&cwd> := &cwd;
}

proto sub cwd(|) { * }
multi sub cwd() {
    $*CWD
} 

{
    proto sub chdir(|) { * }
    multi sub chdir($path as Str) {
        nqp::chdir(nqp::unbox_s($path));
        $*CWD = IO::Path.new(cwd());
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
    PROCESS::<&chdir> := &chdir;
}

proto sub chdir(|) { * }
multi sub chdir(IO::Path:D $path) { chdir $path.Str }
multi sub chdir($path as Str) {
    my $newpath = IO::Path.new(IO::Spec.canonpath($path));
    if $newpath.is-relative {
        my $tmp = $*CWD;
        for IO::Spec.splitdir($newpath) -> $segment {
            given $segment {
                when '..' { $tmp .= parent; }
                when '.' { }
                default { $tmp .= child($segment); }
            }
        }
        $newpath = $tmp;
    }
    if $newpath.d {
        $*CWD = $newpath; 
    } else {
        X::IO::Chdir.new(
            path => $newpath,
            os-error => 'Directory does not exist'
        ).throw;
    }
}

proto sub mkdir(|) { * }
multi sub mkdir($path as Str, $mode = 0o777) {
    my $abspath = IO::Spec.rel2abs($path);
    nqp::mkdir($abspath, $mode);
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
nqp::bindattr(nqp::decont($PROCESS::ERR),
        IO::Handle, '$!PIO', nqp::getstderr());

sub rename(Cool $from as Str, Cool $to as Str) {
    my $absfrom = IO::Spec.rel2abs($from);
    my $absto = IO::Spec.rel2abs($to);
    nqp::rename(nqp::unbox_s($absfrom), nqp::unbox_s($absto));
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
    my $absfrom = IO::Spec.rel2abs($from);
    my $absto = IO::Spec.rel2abs($to);
    nqp::copy(nqp::unbox_s($absfrom), nqp::unbox_s($absto));
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
    my $abstarget = IO::Spec.rel2abs($target);
    nqp::symlink(nqp::unbox_s($abstarget), nqp::unbox_s($name));
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
    my $abstarget = IO::Spec.rel2abs($target);
    nqp::link(nqp::unbox_s($abstarget), nqp::unbox_s($name));
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

sub chmod($mode, $filename) { $filename.path.absolute.chmod($mode); $filename }

# vim: ft=perl6 expandtab sw=4
