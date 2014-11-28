enum SeekType (:SeekFromBeginning(0), :SeekFromCurrent(1), :SeekFromEnd(2));

my role PIO {
    has $!PIO;
    has $.chomp;
    has $.nl;
    has int $.ins;

    method !set-PIO-attributes(Mu :$PIO,:$chomp,:$bin,:$encoding,:$nl,:$enc) {
        $!chomp = $chomp // True;
        nqp::bind($!PIO, nqp::decont($PIO));  # := doesn't work
        self.encoding($bin ?? 'binary' !! $encoding // $enc // 'utf8');
        self.nl($nl // "\n");
    }

    method close(PIO:D:) {
        # TODO:b catch errors
        nqp::closefh($!PIO) if nqp::defined($!PIO);
        $!PIO := Mu;
        True;
    }

    method eof(PIO:D:) {
        nqp::p6bool(nqp::eoffh($!PIO));
    }

    method get(PIO:D:) {
        return Str if self.eof;

        my Str $x := nqp::p6box_s(nqp::readlinefh($!PIO));
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x .= chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins = $!ins + 1;
        $x;
    }

    method getc(PIO:D:) {
        my $c := nqp::p6box_s(nqp::getcfh($!PIO));
        fail if $c eq '';
        $c;
    }

    proto method words (|) { * }
    multi method words(PIO:D: :$close) {
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
    multi method words(PIO:D: :$eager!, :$close) {
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
    multi method words(PIO:D: :$count!, :$close) {
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
    multi method words(PIO:D: $limit, :$eager, :$close) {
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
    multi method lines(PIO:D: :$close) {

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
    multi method lines(PIO:D: :$eager!, :$close) {
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
    multi method lines(PIO:D: :$count!, :$close) {
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
    multi method lines(PIO:D: $limit, :$eager, :$close) {
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

    method read(PIO:D: Cool:D $bytes as Int) {
        my $buf := buf8.new();
        nqp::readfh($!PIO, $buf, nqp::unbox_i($bytes));
        $buf;
    }

    proto method seek(|) { * }
    multi method seek(PIO:D: Int:D $offset, Int:D $whence) {
        DEPRECATED(
          SeekType.^enum_value_list[$whence],
          |<2014.12 2015.12>,
          :what("numerical seektype $whence"),
        );
        nqp::seekfh($!PIO, $offset, $whence);
        True;
    }
    multi method seek(PIO:D: Int:D $offset, SeekType:D $whence) {
        nqp::seekfh($!PIO, $offset, +$whence);
        True;
    }

    method tell(PIO:D:) returns Int {
        nqp::p6box_i(nqp::tellfh($!PIO));
    }

    method write(PIO:D: Blob:D $buf) {
        nqp::writefh($!PIO, nqp::decont($buf));
        True;
    }

    method opened(PIO:D:) {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t(PIO:D:) {
        self.opened && nqp::p6bool($!PIO.isatty)
    }

    proto method print(|) { * }
    multi method print(PIO:D: Str:D \x) {
        nqp::printfh($!PIO, nqp::unbox_s(x));
        Bool::True
    }
    multi method print(PIO:D: *@list) {
        nqp::printfh($!PIO, nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    multi method say(PIO:D: |) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);  # lose the object
        nqp::printfh($!PIO, nqp::unbox_s(nqp::shift($args).gist)) while $args;
        nqp::printfh($!PIO, nqp::unbox_s("\n"));
    }

    method slurp-rest(PIO:D: :$bin,:$encoding,:$enc) {
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
            self.encoding($bin ?? 'binary' !! $encoding // $enc // 'utf8')
              if $bin.defined || $encoding.defined || $enc.defined;
            nqp::p6box_s(nqp::readallfh($!PIO));
        }
    }

    method flush(PIO:D:) {
        fail("File handle not open, so cannot flush")
            unless nqp::defined($!PIO);
        nqp::flushfh($!PIO);
        True;
    }

    proto method encoding(|) { * }
    multi method encoding(PIO:D:) { $!PIO.encoding }
    multi method encoding(PIO:D: $encoding) {
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($encoding));
    }

    proto method nl(|) { * }
    multi method nl() { $!nl }
    multi method nl($nl) {
        nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
    }
}

# vim: ft=perl6 expandtab sw=4
