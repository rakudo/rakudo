my class IO::Path { ... }
my class IO::Special { ... }
my class Proc { ... }

my class IO::Handle does IO {
    has $.path;
    has $!PIO;
    has int $.ins;
    has $.chomp is rw = Bool::True;
    has $.nl    = "\n";

    method open(IO::Handle:D:
      :$r, :$w, :$x, :$a, :$update,
      :$rw, :$rx, :$ra,
      :$mode is copy,
      :$create is copy,
      :$append is copy,
      :$truncate is copy,
      :$exclusive is copy,
      :$bin,
      :$chomp = True,
      :$enc   = 'utf8',
      :$nl    = "\n",
    ) {

        $mode //= do {
            when so ($r && $w) || $rw { $create              = True; 'rw' }
            when so ($r && $x) || $rx { $create = $exclusive = True; 'rw' }
            when so ($r && $a) || $ra { $create = $append    = True; 'rw' }

            when so $r { 'ro' }
            when so $w { $create = $truncate  = True; 'wo' }
            when so $x { $create = $exclusive = True; 'wo' }
            when so $a { $create = $append    = True; 'wo' }

            when so $update { 'rw' }

            default { 'ro' }
        }

        if $!path eq '-' {
            given $mode {
                when 'ro' { return $*IN;  }
                when 'wo' { return $*OUT; }
                default {
                    die "Cannot open standard stream in mode '$_'";
                }
            }
        }

        if nqp::istype($!path, IO::Special) {
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

        fail (X::IO::Directory.new(:$!path, :trying<open>))
          if $!path.e && $!path.d;

        my $llmode = do given $mode {
            when 'ro' { 'r' }
            when 'wo' { '-' }
            when 'rw' { '+' }
            default { die "Unknown mode '$_'" }
        }

        $llmode = join '', $llmode,
            $create    ?? 'c' !! '',
            $append    ?? 'a' !! '',
            $truncate  ?? 't' !! '',
            $exclusive ?? 'x' !! '';

        # TODO: catch error, and fail()
        $!PIO := nqp::open(
          nqp::unbox_s($!path.abspath),
          nqp::unbox_s($llmode),
        );

        $!chomp = $chomp;
        nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc)) unless $bin;
        self;
    }

    method nl is rw {
        Proxy.new(
          FETCH => {
              $!nl
          },
          STORE => -> $, $nl {
            nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
          }
        );
    }

    method close(IO::Handle:D:) {
        # TODO:b catch errors
        nqp::closefh($!PIO) if nqp::defined($!PIO);
        $!PIO := Mu;
        True;
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

    proto method comb(|) { * }
    multi method comb(IO::Handle:D: :$close = False) {
        self.split(:$close,:COMB)
    }
    multi method comb(IO::Handle:D: Int:D $size, :$close = False) {
        return self.split(:$close,:COMB) if $size <= 1;

        Seq.new(class :: does Iterator {
            has Mu  $!handle;
            has Mu  $!size;
            has int $!close;

            submethod BUILD(\handle, \size, \close) {
                $!handle := handle;
                $!size    = size.Int;
                $!close   = close;
                self
            }
            method new(\handle, \size, \close) {
                nqp::create(self).BUILD(handle, size, close);
            }
            method !readcharsfh() {
                my Mu $PIO := nqp::getattr($!handle, IO::Handle, '$!PIO');
#?if jvm
                my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
                # we only get half the number of chars
                nqp::readfh($PIO, $buf, $!size + $!size);
                nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
                nqp::readcharsfh($PIO, $!size);
#?endif
            }

            method pull-one() {
                my str $str = self!readcharsfh;
                if nqp::chars($str) {
                    nqp::p6box_s($str)
                }
                else {
                    $!handle.close if $!close;
                    IterationEnd
                }
            }
            method push-all($target) {
                my str $str = self!readcharsfh;
                while nqp::chars($str) == $size {
                    $target.push(nqp::p6box_s($str));
                    $str = self!readcharsfh;
                }
                $target.push(nqp::p6box_s($str)) if nqp::chars($str);
                $!handle.close if $!close;
                IterationEnd
            }
            method count-only() {
                my int $found;
                my str $str = self!readcharsfh;
                while nqp::chars($str) == $size {
                    $found = $found + 1;
                    $str   = self!readcharsfh;
                }
                $found = $found + 1 if nqp::chars($str);
                $!handle.close if $!close;
                nqp::p6box_i($found)
            }
        }.new(self, $size, +$close));
    }
    multi method comb(IO::Handle:D: $comber, :$close = False) {
        return self.split(:$close,:COMB)
          if nqp::istype($comber,Cool) && $comber.Str.chars == 0;

        Seq.new(class :: does Iterator {
            has Mu  $!handle;
            has Mu  $!regex;
            has str $!comber;
            has int $!close;
            has str $!str;
            has str $!left;
            has Mu  $!strings;
            has int $!elems;
            has int $!done;

            submethod BUILD(\handle, \comber, \close) {
                $!handle := handle;
                nqp::istype(comber,Regex)
                  ?? ($!regex := comber)
                  !! ($!comber = nqp::unbox_s(comber.Str));
                $!close = close;
                $!left  = '';
                self!next-chunk until $!elems || $!done;
                self
            }
            method new(\handle, \comber, \close) {
                nqp::create(self).BUILD(handle, comber, close);
            }
            method !readcharsfh() {
                my Mu $PIO := nqp::getattr($!handle, IO::Handle, '$!PIO');
#?if jvm
                my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
                # we only get half the number of chars, but that's ok
                nqp::readfh($PIO, $buf, 65536); # optimize for ASCII
                nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
                nqp::readcharsfh($PIO, 65536); # optimize for ASCII
#?endif
            }

            method !next-chunk(--> Nil) {
                my int $chars = nqp::chars($!left);
                $!str = nqp::concat($!left,self!readcharsfh);
                if nqp::chars($!str) == $chars { # nothing read anymore
                    $!done = 1;
                }
                else {
                    $!strings := nqp::list();
                    with $!regex {
                        my \matches   = $!str.match($!regex, :g);
                        $!elems = matches.elems;
                        nqp::setelems($!strings,$!elems);
                        my int $i;
                        my int $from;
                        my int $to;
                        my Mu $match;
                        while $i < $!elems {
                            $match := matches[$i];
                            $from = $match.from;
                            $to   = $match.to;
                            nqp::bindpos($!strings,$i,
                              nqp::substr($!str,$from,$to - $from));
                            $i = $i + 1;
                        }
                        $!left = nqp::substr($!str,$to);
                    }
                    else {
                        my int $pos;
                        my int $found;
                        nqp::push($!strings,$comber)
                          while ($found = nqp::index($!str,$!comber,$pos)) > 0;
                        $!left  = nqp::substr($!str,$pos);
                        $!elems = nqp::elems($!strings);
                    }
                }
                Nil
            }
            method pull-one() {
                if $!elems {
                    $!elems = $!elems - 1;
                    nqp::p6box_s(nqp::shift($!strings));
                }
                else {
                    self!next-chunk until $!elems || $!done;
                    if $!elems {
                        $!elems = $!elems - 1;
                        nqp::p6box_s(nqp::shift($!strings));
                    }
                    else {
                        $!handle.close if $!close;
                        IterationEnd;
                    }
                }
            }
            method push-all($target) {
                while $!elems {
                    while $!elems {
                        $target.push(nqp::p6box_s(nqp::shift($!strings)));
                        $!elems = $!elems - 1;
                    }
                    self!next-chunk until $!elems || $!done;
                }
                $!handle.close if $!close;
                IterationEnd
            }
            method count-only() {
                my int $found;
                while $!elems {
                    $found  = $found + $!elems;
                    $!elems = 0;
                    self!next-chunk until $!elems || $!done;
                }
                $!handle.close if $!close;
                nqp::p6box_i($found)
            }
        }.new(self, $comber, +$close));
    }

    multi method split(IO::Handle:D: :$close = False, :$COMB) {
        Seq.new(class :: does Iterator {
            has Mu  $!handle;
            has int $!close;
            has int $!COMB;
            has str $!str;
            has int $!first;
            has int $!last;
            has int $index;
            has int $chars;

            submethod BUILD(\handle, \close, \COMB) {
                $!handle := handle;
                $!close   = close;
                $!COMB    = ?COMB;
                self!next-chunk();
                $!first = $!last = 1 if $!chars && !$!COMB;
                self
            }
            method new(\handle, \close, \COMB) {
                nqp::create(self).BUILD(handle, close, COMB);
            }
            method !readcharsfh() {
                my Mu $PIO := nqp::getattr($!handle, IO::Handle, '$!PIO');
#?if jvm
                my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
                # we only get half the number of chars, but that's ok
                nqp::readfh($PIO, $buf, 65536); # optimize for ASCII
                nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
                nqp::readcharsfh($PIO, 65536); # optimize for ASCII
#?endif
            }
            method !next-chunk(--> Nil) {
                $!str   = self!readcharsfh;
                $!index = 0;
                $!chars = nqp::chars($!str);
                Nil
            }
            method pull-one() {
                self!next-chunk if !$!index == $!chars;
                if $!first {
                    $!first = 0;
                    ''
                }
                elsif $!index < $!chars {
                    nqp::p6box_s(nqp::substr($!str,$!index++,1))
                }
                elsif $!last {
                    $!last = 0;
                    ''
                }
                else {
                    $!handle.close if $!close;
                    IterationEnd;
                }
            }
            method push-all($target) {
                $target.push('') if $!first;
                while $!index < $!chars {
                    $target.push(
                      nqp::p6box_s(nqp::substr($!str,$!index++,1)))
                        while $!index < $!chars;
                    self!next-chunk();
                }
                $target.push('') if $!last;
                $!handle.close if $!close;
                IterationEnd
            }
            method count-only() {
                my int $found = $!first + $!last;
                while $!chars {
                    $found = $found + $!chars;
                    self!next-chunk();
                }
                $!handle.close if $!close;
                nqp::p6box_i($found)
            }
        }.new(self, +$close, $COMB));
    }
    multi method split(IO::Handle:D: $splitter, :$close = False, :$COMB) {
        return self.split(:$close,:$COMB)
          if nqp::istype($splitter,Cool) && $splitter.Str.chars == 0;

        Seq.new(class :: does Iterator {
            has Mu  $!handle;
            has Mu  $!regex;
            has str $!splitter;
            has int $!close;
            has str $!str;
            has str $!left;
            has Mu  $!strings;
            has int $!elems;
            has int $!done;

            submethod BUILD(\handle, \splitter, \close) {
                $!handle := handle;
                nqp::istype(splitter,Regex)
                  ?? ($!regex   := splitter)
                  !! ($!splitter = nqp::unbox_s(splitter.Str));
                $!close = close;
                $!left  = '';
                self!next-chunk until $!elems || $!done;
                self
            }
            method new(\handle, \splitter, \close) {
                nqp::create(self).BUILD(handle, splitter, close);
            }
            method !readcharsfh() {
                my Mu $PIO := nqp::getattr($!handle, IO::Handle, '$!PIO');
#?if jvm
                my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
                # we only get half the number of chars, but that's ok
                nqp::readfh($PIO, $buf, 65536); # optimize for ASCII
                nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
                nqp::readcharsfh($PIO, 65536); # optimize for ASCII
#?endif
            }

            method !next-chunk(--> Nil) {
                my int $chars = nqp::chars($!left);
                $!str = nqp::concat($!left,self!readcharsfh);
                if nqp::chars($!str) == $chars { # nothing read anymore
                    $!done = 2;
                }
                else {
                    with $!regex {
                        my \matches   = $!str.match($!regex, :g);
                        my int $elems = matches.elems;
                        my Mu $strings := nqp::list();
                        nqp::setelems($strings,$elems);
                        my int $i;
                        my Mu $match;
                        my int $from;
                        while $i < $elems {
                            $match := matches[$i];
                            nqp::bindpos($strings,$i,
                              nqp::substr($!str,$from,$match.from - $from));
                            $from = $match.to;
                            $i    = $i + 1;
                        }
                        $!left = nqp::substr(
                          $!str,$from,nqp::chars($!str) - $from);
                        $!strings := $strings; # lexical natives faster
                    }
                    else {
                        $!strings := nqp::split($!splitter,$!str);
                        $!left =
                          nqp::elems($!strings) ?? nqp::pop($!strings) !! '';
                    }
                    $!elems = nqp::elems($!strings);
                }
                Nil
            }
            method pull-one() {
                if $!elems {
                    $!elems = $!elems - 1;
                    nqp::p6box_s(nqp::shift($!strings));
                }
                else {
                    self!next-chunk until $!elems || $!done;
                    if $!elems {
                        $!elems = $!elems - 1;
                        nqp::p6box_s(nqp::shift($!strings));
                    }
                    elsif $!done == 2 {
                        $!done = 1;
                        nqp::p6box_s($!str);
                    }
                    else {
                        $!handle.close if $!close;
                        IterationEnd;
                    }
                }
            }
            method push-all($target) {
                while $!elems {
                    while $!elems {
                        $target.push(nqp::p6box_s(nqp::shift($!strings)));
                        $!elems = $!elems - 1;
                    }
                    self!next-chunk until $!elems || $!done;
                }
                $target.push(nqp::p6box_s($!str));
                $!handle.close if $!close;
                IterationEnd
            }
            method count-only() {
                my int $found = 1;
                while $!elems {
                    $found = $found + $!elems;
                    self!next-chunk until $!elems || $!done;
                }
                $!handle.close if $!close;
                nqp::p6box_i($found)
            }
        }.new(self, $splitter, +$close));
    }

    proto method words (|) { * }
    multi method words(IO::Handle:D: :$close) {
        Seq.new(class :: does Iterator {
            has $!handle;
            has $!close;
            has str $!str;
            has int $!pos;

            submethod BUILD(\handle, $!close) {
                $!handle := handle;
                $!str = self!readcharsfh();
                $!pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, 0, nqp::chars($!str));
                self
            }
            method new(\handle, \close) {
                nqp::create(self).BUILD(handle, close);
            }
            method !readcharsfh() {
                my Mu $PIO := nqp::getattr($!handle, IO::Handle, '$!PIO');
#?if jvm
                my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
                # we only get half the number of chars, but that's ok
                nqp::readfh($PIO, $buf, 65536); # optimize for ASCII
                nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
                nqp::readcharsfh($PIO, 65536); # optimize for ASCII
#?endif
            }

            method !next-chunk(\chars) {
                $!str = $!pos < chars
                  ?? nqp::concat(nqp::substr($!str,$!pos),self!readcharsfh)
                  !! self!readcharsfh;
                chars = nqp::chars($!str);
                $!pos = chars
                  ?? nqp::findnotcclass(
                       nqp::const::CCLASS_WHITESPACE, $!str, 0, chars)
                  !! 0;
            }
            method pull-one() {
                my int $chars;
                my int $left;
                my int $nextpos;

                while $chars = nqp::chars($!str) {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        my str $found =
                          nqp::substr($!str, $!pos, $nextpos - $!pos);
                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);

                        return nqp::p6box_s($found);
                    }
                    self!next-chunk($chars);
                }
                $!handle.close if $!close;
                IterationEnd
            }
            method push-exactly($target, int $n) {
                my int $found;
                my int $chars;
                my int $left;
                my int $nextpos;

                while $chars = nqp::chars($!str) {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        $target.push(nqp::p6box_s(
                          nqp::substr($!str, $!pos, $nextpos - $!pos)
                        ));
                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);

                        $found = $found + 1;
                        return nqp::p6box_i($n) if $found == $n;
                    }
                    self!next-chunk($chars);
                }
                $!handle.close if $close;
                nqp::p6box_i($found)
            }
            method push-all($target) {
                my int $chars;
                my int $left;
                my int $nextpos;

                while $chars = nqp::chars($!str) {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        $target.push(nqp::p6box_s(
                          nqp::substr($!str, $!pos, $nextpos - $!pos)
                        ));

                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);
                    }
                    self!next-chunk($chars);
                }
                $!handle.close if $close;
                IterationEnd
            }
            method count-only() {
                my int $found;
                my int $chars;
                my int $left;
                my int $nextpos;

                while $chars = nqp::chars($!str) {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        $found = $found + 1;

                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);
                    }
                    self!next-chunk($chars);
                }
                $!handle.close if $!close;
                nqp::p6box_i($found)
            }
        }.new(self, $close));
    }

    proto method lines (|) { * }
    multi method lines(IO::Handle:D: $limit) {
        # we should probably deprecate this feature
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines
          !! self.lines[ $!ins .. $!ins + $limit.Int - 1 ]
    }
    my role LinesIterCommon does Iterator {
        has $!handle;
        has $!PIO;
        has $!close;

        submethod BUILD(\handle, $!close) {
            $!handle := handle;
            $!PIO    := nqp::getattr(handle, IO::Handle, '$!PIO');
            self
        }
        method new(\handle, \close) {
            nqp::create(self).BUILD(handle, close);
        }
        method count-only() {
            my int $seen;
            my str $line;
            $line = nqp::readlinefh($!PIO);
            while nqp::chars($line) {
                $seen = $seen + 1;
                $line = nqp::readlinefh($!PIO);
            }
            $!close
              ?? $!handle.close
              !! nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                   nqp::getattr($!handle, IO::Handle, '$!ins' + $seen));

            $seen
        }
    }
    multi method lines(IO::Handle:D: :$close) {
        if $.chomp {  # this can go as soon as we have chomp support on PIO
            Seq.new(class :: does LinesIterCommon {
                method pull-one() {
                    my str $line = nqp::readlinefh($!PIO);
                    if nqp::chars($line) {
                        nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                          nqp::add_i(
                            nqp::getattr_i($!handle, IO::Handle, '$!ins'),
                            1));
                        nqp::p6box_s($line).chomp;
                    }
                    else {
                        $!handle.close if $!close;
                        IterationEnd
                    }
                }
                method push-exactly($target, int $n) {
                    my int $found;
                    my str $line = nqp::readlinefh($!PIO);
                    while nqp::chars($line) {
                        $target.push(nqp::p6box_s($line).chomp);
                        $found = $found + 1;
                        last if $found == $n;

                        $line = nqp::readlinefh($!PIO);
                    }

                    if $!close { # don't bother updating .ins
                        $!handle.close unless nqp::chars($line);
                    }
                    else {
                        nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                          nqp::add_i(
                            nqp::getattr_i($!handle, IO::Handle, '$!ins'),
                            $found));
                    }
                    nqp::p6box_i($found);
                }
                method push-all($target) {
                    my str $line = nqp::readlinefh($!PIO);
                    if $!close {   # don't bother keeping track of $!ins
                        while nqp::chars($line) {
                            $target.push(nqp::p6box_s($line).chomp);
                            $line = nqp::readlinefh($!PIO);
                        }
                        $!handle.close;
                    }
                    else {
                        my int $found;
                        while nqp::chars($line) {
                            $target.push(nqp::p6box_s($line).chomp);
                            $found = $found + 1;
                            $line  = nqp::readlinefh($!PIO);
                        }
                        nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                          nqp::add_i(
                            nqp::getattr_i($!handle, IO::Handle, '$!ins'),
                            $found));
                    }
                    IterationEnd;
                }
            }.new(self, $close));
        }
        else {
            Seq.new(class :: does LinesIterCommon {
                method pull-one() {
                    my str $line = nqp::readlinefh($!PIO);
                    if nqp::chars($line) {
                        nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                          nqp::add_i(
                            nqp::getattr_i($!handle, IO::Handle, '$!ins'),
                            1));
                        nqp::p6box_s($line);
                    }
                    else {
                        $!handle.close if $!close;
                        IterationEnd;
                    }
                }
                method push-exactly($target, int $n) {
                    my str $line = nqp::readlinefh($!PIO);
                    my int $found;
                    while nqp::chars($line) {
                        $target.push(nqp::p6box_s($line));
                        $found = $found + 1;
                        last if $found == $n;

                        $line = nqp::readlinefh($!PIO);
                    }

                    if $!close { # don't bother updating .ins
                        $!handle.close unless nqp::chars($line);
                    }
                    else {
                        nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                          nqp::add_i(
                            nqp::getattr_i($!handle, IO::Handle, '$!ins'),
                            $found));
                    }
                    nqp::p6box_i($found);
                }
                method push-all($target) {
                    my str $line = nqp::readlinefh($!PIO);
                    if $!close {   # don't bother keeping track of $!ins
                        while nqp::chars($line) {
                            $target.push(nqp::p6box_s($line));
                            $line = nqp::readlinefh($!PIO);
                        }
                        $!handle.close;
                    }
                    else {
                        my int $found;
                        while nqp::chars($line) {
                            $target.push(nqp::p6box_s($line));
                            $found = $found + 1;
                            $line  = nqp::readlinefh($!PIO);
                        }
                        nqp::bindattr_i($!handle, IO::Handle, '$!ins',
                          nqp::add_i(
                            nqp::getattr_i($!handle, IO::Handle, '$!ins'),
                            $found));
                    }
                    IterationEnd;
                }
            }.new(self, $close));
        }
    }

    method read(IO::Handle:D: Int(Cool:D) $bytes) {
        my $buf := buf8.new();
        nqp::readfh($!PIO, $buf, nqp::unbox_i($bytes));
        $buf;
    }

    method !readcharsfh(int $chars) {
#?if jvm
        my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
        nqp::readfh($!PIO, $buf, $chars + $chars); # a char = 2 bytes
        nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
        nqp::readcharsfh($!PIO, $chars);
#?endif
    }

    method Supply(IO::Handle:D: :$size = 65536, :$bin --> Supply:D) {
        if $bin {
            supply {
                my $buf := buf8.new;
                my int $bytes = $size;
                nqp::readfh($!PIO, $buf, $bytes);
                while nqp::elems($buf) {
                    emit $buf;
                    nqp::readfh($!PIO, $buf, $bytes);
                }
            }
        }
        else {
            supply {
                my int $chars = $size;
                my str $str = self!readcharsfh($chars);
                while nqp::chars($str) {
                    emit nqp::p6box_s($str);
                    $str = self!readcharsfh($chars);
                }
            }
        }
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
    multi method print(IO::Handle:D: str:D \x) {
        nqp::printfh($!PIO,x);
        Bool::True
    }
    multi method print(IO::Handle:D: Str:D \x) {
        nqp::printfh($!PIO, nqp::unbox_s(x));
        Bool::True
    }
    multi method print(IO::Handle:D: *@list is raw) { # is raw gives List, which is cheaper
        nqp::printfh($!PIO, nqp::unbox_s(.Str)) for @list;
        Bool::True
    }

    multi method say(IO::Handle:D: |) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print-nl;
    }

    method print-nl(IO::Handle:D:) {
        nqp::printfh($!PIO, nqp::unbox_s($!nl));
        Bool::True;
    }

    proto method slurp-rest(|) { * }
    multi method slurp-rest(IO::Handle:D: :$bin!) returns Buf {
        my $Buf := buf8.new();
        loop {
            my $buf := buf8.new();
            nqp::readfh($!PIO,$buf,65536);
            last if $buf.bytes == 0;
            $Buf := $Buf ~ $buf;
        }
        $Buf;
    }
    multi method slurp-rest(IO::Handle:D: :$enc) returns Str {
        self.encoding($enc) if $enc.defined;
        nqp::p6box_s(nqp::readallfh($!PIO));
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
        IO::Notification.watch-path($!path);
    }
#?endif
}

# vim: ft=perl6 expandtab sw=4
