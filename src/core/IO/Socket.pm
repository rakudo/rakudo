my role IO::Socket does IO {
    has $!PIO;
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
            my $r := nqp::readfh($!PIO, nqp::decont(buf8.new), 65536);
            $!buffer ~= $r;
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
        my $res = buf8.new();
        my $buf;
        repeat {
            $buf := buf8.new();
            nqp::readfh($!PIO, $buf, nqp::unbox_i($bufsize - $res.elems));
            $res ~= $buf;
        } while $res.elems < $bufsize && $buf.elems;
        $res;
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
        $!PIO := Mu;
    }
}

# vim: ft=perl6 expandtab sw=4
