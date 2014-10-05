my class IO::Path { ... }

my class IO::Handle does IO::FileTestable {
    has $!PIO;
    has int $.ins;
    has $.chomp = Bool::True;
    has $.path;
    has Bool $!isDir;

    proto method open(|) { * }
    multi method open($path? is copy, :$r is copy, :$w is copy, :$rw, :$a, :$p, :$bin, :$chomp = Bool::True,
            :$enc = 'utf8') {
        $path //= $!path;
        my $is_std_handle = $path eq "-";
        $r = $w = True if $rw;
        my $abspath = !$is_std_handle && defined($*CWD) ?? $*SPEC.rel2abs($path) !! $path;
        $!isDir = Bool::True if !$is_std_handle &&
            nqp::p6bool(nqp::stat($abspath.Str, nqp::const::STAT_EXISTS))
            && nqp::p6bool(nqp::stat($abspath.Str, nqp::const::STAT_ISDIR));
        fail (X::IO::Directory.new(:$path, :trying<open>)) if $!isDir;
#?if parrot
        my $mode =  $p ?? ($w ||  $a ?? 'wp' !! 'rp') !!
                   ($w ?? 'w' !! ($a ?? 'wa' !! 'r' ));
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO::Handle, '$!PIO',
             $is_std_handle
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
                 $is_std_handle
                    ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                    !! nqp::openpipe(nqp::unbox_s($abspath.Str), nqp::unbox_s($*CWD.Str), $hash-without, nqp::unbox_s($errpath))
            );
        }
        else {
            my $mode =  $w ?? 'w' !! ($a ?? 'wa' !! 'r' );
            # TODO: catch error, and fail()
            nqp::bindattr(self, IO::Handle, '$!PIO',
                 $is_std_handle
                    ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                    !! nqp::open(nqp::unbox_s($abspath.Str), nqp::unbox_s($mode))
            );
        }
#?endif
        $!path = $path;
        $!chomp = $chomp;
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc)) unless $bin;
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
        fail (X::IO::Directory.new(:$!path, :trying<get>)) if $!isDir;
        return Str if self.eof;
        my Str $x = nqp::p6box_s(nqp::readlinefh($!PIO));
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x.=chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins = $!ins + 1;
        $x;
    }
    
    method getc() {
        unless $!PIO {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<getc>)) if $!isDir;
        my $c = nqp::p6box_s(nqp::getcfh($!PIO));
        fail if $c eq '';
        $c;
    }

    proto method words (|) { * }
    multi method words(IO::Handle:D: :$close) {
        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<words>)) if $!isDir;

        my str $str;
        my int $chars;
        my int $pos;
        my int $left;
        my int $nextpos;

        gather {
            until nqp::eoffh($!PIO) {

#?if moar
                # optimize for ASCII
                $str   = $str ~ nqp::readcharsfh($!PIO, 65536);
#?endif
#?if !moar
                my Buf $buf := Buf.new;
                nqp::readfh($!PIO, $buf, 65536);
                $str = $str ~ nqp::unbox_s($buf.decode);
#?endif
                $chars = nqp::chars($str);
                $pos   = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

                while ($left = $chars - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
                    last unless $left = $chars - $nextpos; # broken word

                    take
                      nqp::box_s(nqp::substr($str, $pos, $nextpos - $pos), Str);

                    $pos = nqp::findnotcclass(
                      nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
                }

                $str = $pos < $chars ?? nqp::substr($str,$pos) !! '';
            }
            self.close if $close;
        }
    }
    # can probably go after GLR
    multi method words(IO::Handle:D: :$eager!, :$close) {
        return self.words(:$close) if !$eager;

        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<words>)) if $!isDir;

        my str $str;
        my int $chars;
        my int $pos;
        my int $left;
        my int $nextpos;
        my Mu $rpa := nqp::list();

        until nqp::eoffh($!PIO) {

#?if moar
            $str   = $str ~ nqp::readcharsfh($!PIO, 65536); # optimize for ASCII
#?endif
#?if !moar
            my Buf $buf := Buf.new;
            nqp::readfh($!PIO, $buf, 65536);
            $str   = $str ~ nqp::unbox_s($buf.decode);
#?endif
            $chars = nqp::chars($str);
            $pos   = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

            while ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
                last unless $left = $chars - $nextpos; # broken word

                nqp::push($rpa,
                  nqp::box_s(nqp::substr($str, $pos, $nextpos - $pos), Str) );

                $pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
            }

            $str = $pos < $chars ?? nqp::substr($str,$pos) !! '';
        }
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
    }
    multi method words(IO::Handle:D: :$count!, :$close) {
        return self.words(:$close) if !$count;

        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<words>)) if $!isDir;

        my str $str;
        my int $chars;
        my int $pos;
        my int $left;
        my int $nextpos;
        my int $found;

        until nqp::eoffh($!PIO) {

#?if moar
            $str   = $str ~ nqp::readcharsfh($!PIO, 65536); # optimize for ASCII
#?endif
#?if !moar
            my Buf $buf := Buf.new;
            nqp::readfh($!PIO, $buf, 65536);
            $str   = $str ~ nqp::unbox_s($buf.decode);
#?endif
            $chars = nqp::chars($str);
            $pos   = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

            while ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
                last unless $left = $chars - $nextpos; # broken word

                $found = $found + 1;

                $pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
            }

            $str = $pos < $chars ?? nqp::substr($str,$pos) !! '';
        }
        self.close if $close;
        nqp::box_i($found, Int);
    }
    multi method words(IO::Handle:D: $limit, :$eager, :$close) {
        return self.words(:$eager,:$close)
          if $limit == Inf or $limit ~~ Whatever;

        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<words>)) if $!isDir;

        my str $str;
        my int $chars;
        my int $pos;
        my int $left;
        my int $nextpos;
        my int $count = $limit;
        my Mu $rpa := nqp::list();

        until nqp::eoffh($!PIO) {

#?if moar
            $str   = $str ~ nqp::readcharsfh($!PIO, 65536); # optimize for ASCII
#?endif
#?if !moar
            my Buf $buf := Buf.new;
            nqp::readfh($!PIO, $buf, 65536);
            $str   = $str ~ nqp::unbox_s($buf.decode);
#?endif
            $chars = nqp::chars($str);
            $pos   = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

            while $count and ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
                last unless $left = $chars - $nextpos; # broken word

                nqp::push($rpa,
                  nqp::box_s(nqp::substr($str, $pos, $nextpos - $pos), Str) );
                $count = $count - 1;

                $pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
            }

            $str = $pos < $chars ?? nqp::substr($str,$pos) !! '';
        }
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
    }

    proto method lines (|) { * }
    multi method lines(IO::Handle:D: :$close) {
        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<lines>)) if $!isDir;

        if $.chomp {
            gather {
                until nqp::eoffh($!PIO) {
#?if parrot
                    my Mu $line := nqp::readlinefh($!PIO);
                    last if nqp::eoffh($!PIO);
                    take nqp::p6box_s($line).chomp;
#?endif
#?if !parrot
                    take nqp::p6box_s(nqp::readlinefh($!PIO)).chomp;
#?endif
                    $!ins = $!ins + 1;
                }
                self.close if $close;
            }
        }
        else {
            gather {
                until nqp::eoffh($!PIO) {
#?if parrot
                    my Mu $line := nqp::readlinefh($!PIO);
                    last if nqp::eoffh($!PIO);
                    take nqp::p6box_s($line);
#?endif
#?if !parrot
                    take nqp::p6box_s(nqp::readlinefh($!PIO));
#?endif
                    $!ins = $!ins + 1;
                }
                self.close if $close;
            }
        }
    }
    # can probably go after GLR
    multi method lines(IO::Handle:D: :$eager!, :$close) {
        return self.lines if !$eager;

        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<lines>)) if $!isDir;

        my Mu $rpa := nqp::list();
        if $.chomp {
            until nqp::eoffh($!PIO) {
#?if parrot
                my Mu $line := nqp::readlinefh($!PIO);
                last if nqp::eoffh($!PIO);
                nqp::push($rpa, nqp::p6box_s($line).chomp);
#?endif
#?if !parrot
                nqp::push($rpa, nqp::p6box_s(nqp::readlinefh($!PIO)).chomp );
#?endif
                $!ins = $!ins + 1;
            }
        }
        else {
            until nqp::eoffh($!PIO) {
#?if parrot
                my Mu $line := nqp::readlinefh($!PIO);
                last if nqp::eoffh($!PIO);
                nqp::push($rpa, nqp::p6box_s($line));
#?endif
#?if !parrot
                nqp::push($rpa, nqp::p6box_s(nqp::readlinefh($!PIO)) );
#?endif
                $!ins = $!ins + 1;
            }
        }
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
    }
    multi method lines(IO::Handle:D: :$count!, :$close) {
        return self.lines(:$close) if !$count;

        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<lines>)) if $!isDir;

        until nqp::eoffh($!PIO) {
            nqp::readlinefh($!PIO);
#?if parrot
            last if nqp::eoffh($!PIO);
#?endif
            $!ins = $!ins + 1;
        }
        nqp::box_i($!ins, Int);
    }
    multi method lines(IO::Handle:D: $limit, :$eager, :$close) {
        return self.lines(:$eager, :$close)
          if $limit == Inf or $limit ~~ Whatever;

        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<lines>)) if $!isDir;

        my Mu $rpa := nqp::list();
        my int $count = $limit + 1;
        if $.chomp {
            while $count = $count - 1 {
                last if nqp::eoffh($!PIO);
#?if parrot
                my Mu $line := nqp::readlinefh($!PIO);
                last if nqp::eoffh($!PIO);
                nqp::push($rpa, nqp::p6box_s($line).chomp);
#?endif
#?if !parrot
                nqp::push($rpa, nqp::p6box_s(nqp::readlinefh($!PIO)).chomp );
#?endif
                $!ins = $!ins + 1;
            }
        }
        else {
            while $count = $count - 1 {
#?if parrot
                my Mu $line := nqp::readlinefh($!PIO);
                last if nqp::eoffh($!PIO);
                nqp::push($rpa, nqp::p6box_s($line));
#?endif
#?if !parrot
                nqp::push($rpa, nqp::p6box_s(nqp::readlinefh($!PIO)) );
#?endif
                $!ins = $!ins + 1;
            }
        }
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
    }

    method read(IO::Handle:D: Cool:D $bytes as Int) {
        fail (X::IO::Directory.new(:$!path, :trying<read>)) if $!isDir;
        my $buf := buf8.new();
        nqp::readfh($!PIO, $buf, nqp::unbox_i($bytes));
        $buf;
    }

    # second arguemnt should probably be an enum
    # valid values for $whence:
    #   0 -- seek from beginning of file
    #   1 -- seek relative to current position
    #   2 -- seek from the end of the file
    method seek(IO::Handle:D: Int:D $offset, Int:D $whence) {
        nqp::seekfh($!PIO, $offset, $whence);
        True;
    }

    method tell(IO::Handle:D:) returns Int {
        nqp::p6box_i(
            nqp::tellfh($!PIO)
        );
    }

    method write(IO::Handle:D: Blob:D $buf) {
        fail (X::IO::Directory.new(:$!path, :trying<write>)) if $!isDir;
        nqp::writefh($!PIO, nqp::decont($buf));
        True;
    }

    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t() {
        self.opened && nqp::p6bool($!PIO.isatty)
    }


    proto method print(|) { * }
    multi method print(IO::Handle:D: Str:D \x) {
        fail (X::IO::Directory.new(:$!path, :trying<print>)) if $!isDir;
        nqp::printfh($!PIO, nqp::unbox_s(x));
        Bool::True
    }
    multi method print(IO::Handle:D: *@list) {
        fail (X::IO::Directory.new(:$!path, :trying<print>)) if $!isDir;
        nqp::printfh($!PIO, nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    multi method say(IO::Handle:D: |) {
        fail (X::IO::Directory.new(:$!path, :trying<say>)) if $!isDir;
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print: "\n";
    }
    
    method slurp(:$bin, :enc($encoding)) {
        self.open(:r, :$bin) unless self.opened;
        fail (X::IO::Directory.new(:$!path, :trying<slurp>, :use<dir>))
            if $!isDir;
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
                    :$enc = 'utf8',
                    :$createonly, :$append) {
        fail("File '" ~ self.path ~ "' already exists, but :createonly was specified")
            if $createonly && self.e;
        fail (X::IO::Directory.new(:$!path, :trying<spurt>, :use<mkdir>))
            if self.d();
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
        fail (X::IO::Directory.new(:$!path, :trying<spurt>, :use<mkdir>))
            if self.d();
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
            nqp::copy(nqp::unbox_s($*SPEC.rel2abs(~$!path)), 
                      nqp::unbox_s($*SPEC.rel2abs(~$dest)));
        }
        $! ?? fail(X::IO::Copy.new(from => $!path, to => $dest, os-error => ~$!)) !! True
    }

    method chmod(Int $mode) {
        self.path.absolute.chmod($mode)
    }

    method IO { IO::Path.new($!path) }

    method path {  IO::Path.new($!path)  }

    multi method Str (IO::Handle:D:) {  $!path  }

    multi method gist (IO::Handle:D:) {
        self.opened
            ?? "IO::Handle<$!path>(opened, at line {$.ins} / octet {$.tell})"
            !! "IO::Handle<$!path>(closed)"
    }

    multi method perl (IO::Handle:D:) {
        "IO::Handle.new(path => {$!path.perl}, ins => {$!ins.perl}, chomp => {$!chomp.perl}, Directory => {$!isDir.Bool})"
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

    submethod DESTROY() {
        self.close;
    }

#?if moar
    method watch(IO::Handle:D:) {
        IO::Notification.watch_path($!path);
    }
#?endif
}

# vim: ft=perl6 expandtab sw=4
