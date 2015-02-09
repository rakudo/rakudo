my class IO::Path { ... }
my class IO::Special { ... }

my class IO::Handle does IO {
    has $.path;
    has $!PIO;
    has int $.ins;
    has $.chomp = Bool::True;
    has $.nl    = "\n";
    has int $!pipe;

    method pipe(IO::Handle:D: |c) {
        self.open(:p, |c);
    }

    method open(IO::Handle:D:
      :$r is copy,
      :$w is copy,
      :$rw,
      :$a,
      :$p,
      :$bin,
      :$chomp = True,
      :$enc   = 'utf8',
      :$nl    = "\n",
    ) {

        if $!path eq '-' {
            $!path =
              IO::Special.new(:what( $w ?? << <STDOUT> >> !! << <STDIN> >> ));
        }

        if nqp::istype($!path,IO::Special) {
            my $what := $!path.what;
            if $what eq '<STDIN>' {
                $!PIO := nqp::getstdin();
            }
            elsif $what eq '<STDOUT>' {
                $!PIO := nqp::getstdout();
            }
            elsif $what eq '<STDERR>' {
                $!PIO := nqp::getstderr();
            }
            else {
                die "Don't know how to open '$_' especially";
            }
            $!chomp = $chomp;
            nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc)) unless $bin;
            return self;
        }

        fail (X::IO::Directory.new(:$!path, :trying<open>)) if $!path.d;
        $r = $w = True if $rw;

        if $p {
            $!pipe = 1;

            my str $errpath;
            $!PIO := nqp::openpipe(
              nqp::unbox_s($!path.Str),
              nqp::unbox_s($*CWD.Str),
              CLONE-HASH-DECONTAINERIZED(%*ENV),
              $errpath,
            );
        }
        else {
            my $mode =  $w ?? 'w' !! ($a ?? 'wa' !! 'r' );
            # TODO: catch error, and fail()
            $!PIO := nqp::open(
              nqp::unbox_s($!path.abspath),
              nqp::unbox_s($mode),
            );
        }

        $!chomp = $chomp;
        nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc)) unless $bin;
        self;
    }

    method input-line-separator {
        Proxy.new(
          FETCH => {
              $!nl
          },
          STORE => -> $, $nl is copy {
            nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
          }
        );
    }

    method close(IO::Handle:D:) {
        # TODO:b catch errors
        if $!pipe {
            my $ps = Proc::Status.new;
            $ps.status( nqp::closefh_i($!PIO) ) if nqp::defined($!PIO);
            $!PIO := Mu;
            $ps;
        }
        else {
            nqp::closefh($!PIO) if nqp::defined($!PIO);
            $!PIO := Mu;
            True;
        }
    }

    method eof(IO::Handle:D:) {
        nqp::p6bool(nqp::eoffh($!PIO));
    }

    method get(IO::Handle:D:) {
        return Str if self.eof;

        my Str $x = nqp::p6box_s(nqp::readlinefh($!PIO));
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x.=chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins = $!ins + 1;
        $x;
    }

    method getc(IO::Handle:D:) {
        my $c = nqp::p6box_s(nqp::getcfh($!PIO));
        fail if $c eq '';
        $c;
    }

    proto method words (|) { * }
    multi method words(IO::Handle:D: :$close) {
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
          if $limit == Inf or nqp::istype($limit,Whatever);

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

        if $.chomp {
            gather {
                until nqp::eoffh($!PIO) {
#?if parrot
                    my Mu $line := nqp::readlinefh($!PIO);
                    last if nqp::eoffh($!PIO);
                    $!ins = $!ins + 1;
                    take nqp::p6box_s($line).chomp;
#?endif
#?if !parrot
                    $!ins = $!ins + 1;
                    take nqp::p6box_s(nqp::readlinefh($!PIO)).chomp;
#?endif
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
                    $!ins = $!ins + 1;
                    take nqp::p6box_s($line);
#?endif
#?if !parrot
                    $!ins = $!ins + 1;
                    take nqp::p6box_s(nqp::readlinefh($!PIO));
#?endif
                }
                self.close if $close;
            }
        }
    }
    # can probably go after GLR
    multi method lines(IO::Handle:D: :$eager!, :$close) {
        return self.lines if !$eager;

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
            }
        }
        $!ins = nqp::elems($rpa);
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
    }
    multi method lines(IO::Handle:D: :$count!, :$close) {
        return self.lines(:$close) if !$count;

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
          if $limit == Inf or nqp::istype($limit,Whatever);

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
            }
        }
        $!ins = nqp::elems($rpa);
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
    }

    method read(IO::Handle:D: Int(Cool:D) $bytes) {
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
        nqp::p6box_i(nqp::tellfh($!PIO));
    }

    method write(IO::Handle:D: Blob:D $buf) {
        nqp::writefh($!PIO, nqp::decont($buf));
        True;
    }

    method opened(IO::Handle:D:) {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t(IO::Handle:D:) {
        self.opened && nqp::p6bool($!PIO.isatty)
    }


    proto method print(|) { * }
    multi method print(IO::Handle:D: Str:D \x) {
        nqp::printfh($!PIO, nqp::unbox_s(x));
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

    method slurp(IO::Handle:D: |c) {
        DEPRECATED('$handle.slurp-rest', |<2014.10 2015.10>);
        self.slurp-rest(|c);
    }

    method slurp-rest(IO::Handle:D: :$bin, :$enc) {
        if $bin {
            my $Buf := buf8.new();
            loop {
                my $buf := buf8.new();
                nqp::readfh($!PIO,$buf,65536);
                last if $buf.bytes == 0;
                $Buf := $Buf ~ $buf;
            }
            $Buf;
        }
        else {
            self.encoding($enc) if $enc.defined;
            nqp::p6box_s(nqp::readallfh($!PIO));
        }
    }

    proto method spurt(|) { * }
    multi method spurt(IO::Handle:D: Cool $contents, :$nodepr) {
        DEPRECATED("IO::Path.spurt", |<2014.10 2015.10>) unless $nodepr;
        self.print($contents);
    }

    multi method spurt(IO::Handle:D: Blob $contents, :$nodepr) {
        DEPRECATED("IO::Path.spurt", |<2014.10 2015.10>) unless $nodepr;
        self.write($contents);
    }

    # not spec'd
    method copy(IO::Handle:D: $dest) {
        DEPRECATED("IO::Path.copy", |<2014.10 2015.10>);
        $!path.copy($dest);
    }

    method chmod(IO::Handle:D: Int $mode) { $!path.chmod($mode) }
    method IO(IO::Handle:D: |c)           { $!path.IO(|c) }
    method path(IO::Handle:D:)            { $!path.IO }
    multi method Str(IO::Handle:D:)       { $!path }

    multi method gist(IO::Handle:D:) {
        self.opened
            ?? "IO::Handle<$!path>(opened, at line {$.ins} / octet {$.tell})"
            !! "IO::Handle<$!path>(closed)"
    }

    multi method perl(IO::Handle:D:) {
        "IO::Handle.new(path => {$!path.perl}, ins => {$!ins.perl}, chomp => {$!chomp.perl})"
    }


    method flush(IO::Handle:D:) {
        fail("File handle not open, so cannot flush")
            unless nqp::defined($!PIO);
        nqp::flushfh($!PIO);
        True;
    }

    method encoding(IO::Handle:D: $enc?) {
        $enc.defined
            ?? nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc))
            !! $!PIO.encoding
    }

    submethod DESTROY(IO::Handle:D:) {
        self.close;
    }

    # setting cannot do "handles", so it's done by hand here
    method e(IO::Handle:D:) { $!path.e }
    method d(IO::Handle:D:) { $!path.d }
    method f(IO::Handle:D:) { $!path.f }
    method s(IO::Handle:D:) { $!path.s }
    method l(IO::Handle:D:) { $!path.l }
    method r(IO::Handle:D:) { $!path.r }
    method w(IO::Handle:D:) { $!path.w }
    method x(IO::Handle:D:) { $!path.x }
    method modified(IO::Handle:D:) { $!path.modified }
    method accessed(IO::Handle:D:) { $!path.accessed }
    method changed(IO::Handle:D:)  { $!path.changed  }

#?if moar
    method watch(IO::Handle:D:) {
        IO::Notification.watch_path($!path);
    }
#?endif
}

# vim: ft=perl6 expandtab sw=4
