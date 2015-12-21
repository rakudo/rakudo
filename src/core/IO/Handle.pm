my class IO::Path { ... }
my class IO::Special { ... }
my class Proc { ... }

my class IO::Handle does IO {
    has $.path;
    has $!PIO;
    has Bool $.chomp is rw;
    has Str $.enc;
    has $.nl-in = ["\x0A", "\r\n"];
    has Str:D $.nl-out is rw = "\n";

    my role Binary {
        method get { callsame.?encode(self.enc) }
        method getc { self.read(1).[0] }
    }

    method open(IO::Handle:D:
      :$r, :$w, :$x, :$a, :$update,
      :$rw, :$rx, :$ra,
      :$mode is copy,
      :$create is copy,
      :$append is copy,
      :$truncate is copy,
      :$exclusive is copy,
      :$bin = False,
      :$!chomp = True,
      :$enc = $bin ?? 'latin1' !! 'utf8',
      :$!nl-in = $!nl-in,
      :$!nl-out = $!nl-out,
    ) {

        self does Binary if $bin;
        $!enc = Rakudo::Internals.NORMALIZE_ENCODING($enc);

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
#?if !jvm
            Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!PIO, $!nl-in);
#?endif
            nqp::setencoding($!PIO, $!enc);
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

        {
            CATCH { .fail }
            $!PIO := nqp::open(
              nqp::unbox_s($!path.abspath),
              nqp::unbox_s($llmode),
            );
        }

        Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!PIO, $!nl-in);
        nqp::setencoding($!PIO, $!enc);
        self;
    }

    method nl-in is rw {
        Proxy.new(
          FETCH => {
              $!nl-in
          },
          STORE => -> $, $nl-in {
            Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!PIO, $!nl-in = $nl-in);
          }
        );
    }

    method close(IO::Handle:D: --> True) {
        # TODO: catch errors
        nqp::closefh($!PIO) if nqp::defined($!PIO);
        $!PIO := Mu;
    }

    method eof(IO::Handle:D:) {
        nqp::p6bool(nqp::eoffh($!PIO));
    }

    method get(IO::Handle:D:) {
        my str $str;
        if $!chomp {
            $str = nqp::readlinechompfh($!PIO);
            # loses last empty line because EOF is set too early, RT #126598
            nqp::chars($str) || !nqp::eoffh($!PIO) ?? $str !! Nil
        }
        else {
            $str = nqp::readlinefh($!PIO);
            # no need to check EOF
            nqp::chars($str) ?? $str !! Nil
        }
    }

    method getc(IO::Handle:D:) {
        my str $c = nqp::getcfh($!PIO);
        nqp::chars($c) ?? $c !! Nil
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

            method pull-one() {
                my str $str = $!handle.readchars($!size);
                if nqp::chars($str) {
                    nqp::p6box_s($str)
                }
                else {
                    $!handle.close if $!close;
                    IterationEnd
                }
            }
            method push-all($target) {
                my str $str = $!handle.readchars($!size);
                while nqp::chars($str) == $size {
                    $target.push(nqp::p6box_s($str));
                    $str = $!handle.readchars($!size);
                }
                $target.push(nqp::p6box_s($str)) if nqp::chars($str);
                $!handle.close if $!close;
                IterationEnd
            }
            method count-only() {
                my int $found;
                my str $str = $!handle.readchars($!size);
                while nqp::chars($str) == $size {
                    $found = $found + 1;
                    $str   = $!handle.readchars($!size);
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
            method !next-chunk(--> Nil) {
                my int $chars = nqp::chars($!left);
                $!str = nqp::concat($!left,$!handle.readchars);
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
                        my int $extra = nqp::chars($!comber);
                        while ($found = nqp::index($!str,$!comber,$pos)) >= 0 {
                            nqp::push($!strings,$!comber);
                            $pos = $found + $extra;
                        }
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
            method !next-chunk(--> Nil) {
                $!str   = $!handle.readchars;
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
            method !next-chunk(--> Nil) {
                my int $chars = nqp::chars($!left);
                $!str = nqp::concat($!left,$!handle.readchars);
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
                    $!elems = 0;
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
            has int $!searching;

            submethod BUILD(\handle, $!close) {
                $!handle   := handle;
                $!searching = 1;
                $!str       = ""; # RT #126492
                self!next-chunk;
                self
            }
            method new(\handle, \close) {
                nqp::create(self).BUILD(handle, close);
            }
            method !next-chunk() {
                my int $chars = nqp::chars($!str);
                $!str = $!pos < $chars ?? nqp::substr($!str,$!pos) !! "";
                $chars = nqp::chars($!str);

                while $!searching {
                    $!str = nqp::concat($!str,$!handle.readchars);
                    my int $new = nqp::chars($!str);
                    $!searching = 0 if $new == $chars; # end
                    $!pos = ($chars = $new)
                      ?? nqp::findnotcclass(
                           nqp::const::CCLASS_WHITESPACE, $!str, 0, $chars)
                      !! 0;
                    last if $!pos < $chars;
                }
            }
            method pull-one() {
                my int $chars;
                my int $left;
                my int $nextpos;

                while ($chars = nqp::chars($!str)) && $!searching {
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
                    self!next-chunk;
                }
                if $!pos < $chars {
                    my str $found = nqp::substr($!str,$!pos);
                    $!pos = $chars;
                    nqp::p6box_s($found)
                }
                else {
                    $!handle.close if $!close;
                    IterationEnd
                }
            }
            method push-all($target) {
                my int $chars;
                my int $left;
                my int $nextpos;

                while ($chars = nqp::chars($!str)) && $!searching {
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
                    self!next-chunk;
                }
                $target.push(nqp::p6box_s(nqp::substr($!str,$!pos)))
                  if $!pos < $chars;
                $!handle.close if $close;
                IterationEnd
            }
            method count-only() {
                my int $found;
                my int $chars;
                my int $left;
                my int $nextpos;

                while ($chars = nqp::chars($!str)) && $!searching {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        $found = $found + 1;

                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);
                    }
                    self!next-chunk;
                }
                $found = $found + 1 if $!pos < $chars;
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
          !! self.lines[ 0 .. $limit.Int - 1 ]
    }
    multi method lines(IO::Handle:D: :$close) {
        Seq.new(class :: does Iterator {
            has $!handle;
            has $!close;

            submethod BUILD(\handle, $!close) {
                $!handle := handle;
                self
            }
            method new(\handle, \close) {
                nqp::create(self).BUILD(handle, close);
            }
            method pull-one() is raw {
                $!handle.get // do {
                    $!handle.close if $!close;
                    IterationEnd
                }
            }
            method push-all($target) {
                my $line;
                $target.push($line) while ($line := $!handle.get).DEFINITE;
                $!handle.close if $close;
                IterationEnd
            }
            method count-only() {
                my $line;
                my int $seen;
                $seen = $seen + 1 while ($line := $!handle.get).DEFINITE;
                $!handle.close if $!close;
                $seen
            }
        }.new(self, $close));
    }

    method read(IO::Handle:D: Int(Cool:D) $bytes) {
        my $buf := buf8.new();
        nqp::readfh($!PIO, $buf, nqp::unbox_i($bytes));
        $buf;
    }

    method readchars(Int(Cool:D) $chars = 65536) { # optimize for ASCII
#?if jvm
        my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
        # a char = 2 bytes
        nqp::readfh($!PIO, $buf, nqp::unbox_i($chars + $chars));
        nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
        nqp::readcharsfh($!PIO, nqp::unbox_i($chars));
#?endif
    }

    method Supply(IO::Handle:D: :$size = 65536, :$bin --> Supply:D) {
        if $bin {
            supply {
                my $buf := self.read($size);
                while nqp::elems($buf) {
                    emit $buf;
                    $buf := self.read($size);
                }
                done;
            }
        }
        else {
            supply {
                my int $chars = $size;
                my str $str = self.readchars($chars);
                while nqp::chars($str) {
                    emit nqp::p6box_s($str);
                    $str = self.readchars($chars);
                }
                done;
            }
        }
    }

    proto method seek(|) { * }
    multi method seek(IO::Handle:D: Int:D $offset, SeekType:D $whence = SeekFromBeginning) {
        nqp::seekfh($!PIO, $offset, +$whence);
    }

    method tell(IO::Handle:D:) returns Int {
        nqp::p6box_i(nqp::tellfh($!PIO));
    }

    method write(IO::Handle:D: Blob:D $buf --> True) {
        nqp::writefh($!PIO, nqp::decont($buf));
    }

    method opened(IO::Handle:D:) {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t(IO::Handle:D:) {
        self.opened && nqp::p6bool(nqp::isttyfh($!PIO))
    }

    method lock(IO::Handle:D: Int:D $flag) {
        nqp::lockfh($!PIO, $flag)
    }

    method unlock(IO::Handle:D: --> True) {
        nqp::unlockfh($!PIO);
    }


    proto method print(|) { * }
    multi method print(IO::Handle:D: str:D \x --> True) {
        nqp::printfh($!PIO,x);
    }
    multi method print(IO::Handle:D: Str:D \x --> True) {
        nqp::printfh($!PIO, nqp::unbox_s(x));
    }
    multi method print(IO::Handle:D: *@list is raw --> True) { # is raw gives List, which is cheaper
        nqp::printfh($!PIO, nqp::unbox_s(.Str)) for @list;
    }

    proto method put(|) { * }
    multi method put(IO::Handle:D: str:D \x --> True) {
        nqp::printfh($!PIO,x);
        nqp::printfh($!PIO, nqp::unbox_s($!nl-out));
    }
    multi method put(IO::Handle:D: Str:D \x --> True) {
        nqp::printfh($!PIO, nqp::unbox_s(x));
        nqp::printfh($!PIO, nqp::unbox_s($!nl-out));
    }
    multi method put(IO::Handle:D: *@list is raw --> True) { # is raw gives List, which is cheaper
        nqp::printfh($!PIO, nqp::unbox_s(.Str)) for @list;
        nqp::printfh($!PIO, nqp::unbox_s($!nl-out));
    }

    multi method say(IO::Handle:D: |) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print-nl;
    }

    method print-nl(IO::Handle:D: --> True) {
        nqp::printfh($!PIO, nqp::unbox_s($!nl-out));
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
            ?? "IO::Handle<$!path>(opened, at octet {$.tell})"
            !! "IO::Handle<$!path>(closed)"
    }

    multi method perl(IO::Handle:D:) {
        "IO::Handle.new(path => {$!path.perl}, chomp => {$!chomp.perl})"
    }


    method flush(IO::Handle:D: --> True) {
        fail("File handle not open, so cannot flush")
            unless nqp::defined($!PIO);
        nqp::flushfh($!PIO);
    }

    method encoding(IO::Handle:D: $enc?) {
        $enc.defined
          ?? nqp::setencoding($!PIO,Rakudo::Internals.NORMALIZE_ENCODING($enc))
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

    method native-descriptor(IO::Handle:D:) {
        nqp::filenofh($!PIO)
    }
}

# vim: ft=perl6 expandtab sw=4
