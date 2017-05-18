my role IO::Socket {
    has $!PIO;
    has Str $.encoding = 'utf8';
    has $.nl-in is rw = ["\n", "\r\n"];

    # JVM has a buffer here; Moar does enough buffering of its own
    # and gets it much more correct when bytes cross boundaries, so we use its.
#?if jvm
    has $!buffer = buf8.new;
#?endif

    # if bin is true, will return Buf, Str otherwise
    method recv (Cool $chars = Inf, :$bin? = False) {
        fail('Socket not available') unless $!PIO;

#?if jvm
        if $!buffer.elems < $chars {
            my $r := nqp::readfh($!PIO, nqp::decont(buf8.new), $*DEFAULT-READ-ELEMS);
            $!buffer.append($r);
        }

        if $bin {
            my $rec;
            if $!buffer.elems > $chars {
                $rec = $!buffer.subbuf(0, $chars);
                $!buffer = $!buffer.subbuf($chars);
            } else {
                $rec = $!buffer;
                $!buffer = buf8.new;
            }
            $rec;
        } else {
            my $rec = nqp::decode(nqp::decont($!buffer), 'utf8');
            if $rec.chars > $chars {
                $rec = substr($rec,0,$chars);
                my $used = $rec.encode('utf8').elems;
                $!buffer = $!buffer.subbuf($used)
            } else {
                $!buffer = buf8.new;
            }
            $rec;
        }
#?endif
#?if moar
        if $bin {
            nqp::readfh($!PIO, nqp::decont(buf8.new),
                $chars == Inf ?? 1048576 !! $chars.Int);
        }
        else {
            nqp::p6box_s(nqp::readcharsfh($!PIO,
                $chars == Inf ?? 1048576 !! $chars.Int));
        }
#?endif
    }

    method read(IO::Socket:D: Int(Cool) $bufsize) {
        fail('Socket not available') unless $!PIO;
        my int $toread = $bufsize;
        my $res := nqp::readfh($!PIO,buf8.new,$toread);

        while nqp::elems($res) < $toread {
            my $buf := nqp::readfh($!PIO,buf8.new,$toread - nqp::elems($res));
            nqp::elems($buf)
              ?? $res.append($buf)
              !! return $res
        }
        $res
    }

    method get() {
        my Mu $io := nqp::getattr(self, $?CLASS, '$!PIO');
        nqp::setencoding($io, Rakudo::Internals.NORMALIZE_ENCODING($!encoding));
        Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($io, $!nl-in);
        my str $line = nqp::readlinechompfh($io);
        if nqp::chars($line) || !nqp::eoffh($io) {
            $line
        }
        else {
            Nil
        }
    }

    method lines() {
        gather while (my $line = self.get()).DEFINITE {
            take $line;
        }
    }

    method poll(Int $bitmask, $seconds) {
        die 'Socket.poll is NYI'
    }

    method print (Str(Cool) $string --> True) {
        fail("Not connected") unless $!PIO;
        nqp::printfh($!PIO, nqp::unbox_s($string));
    }

    method put (Str(Cool) $string --> True) {
        fail("Not connected") unless $!PIO;
        nqp::printfh($!PIO, nqp::unbox_s($string));
        nqp::printfh($!PIO, nqp::unbox_s("\n"));  # XXX should be $!nl-out
    }

    method write(Blob:D $buf --> True) {
        fail('Socket not available') unless $!PIO;
        nqp::writefh($!PIO, nqp::decont($buf));
    }

    method close (--> True) {
        fail("Not connected!") unless $!PIO;
        nqp::closefh($!PIO);
        $!PIO := nqp::null;
    }

    method native-descriptor(::?CLASS:D:) {
        nqp::filenofh($!PIO)
    }
}

# vim: ft=perl6 expandtab sw=4
