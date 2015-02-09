my role IO::Socket does IO {
    has $!PIO;
    # JVM and Parrot have a buffer here; Moar does enough buffering of its own
    # and gets it much more correct when bytes cross boundaries, so we use its.
#?if parrot
    has $!buffer = '';
#?endif
#?if jvm
    has $!buffer = buf8.new;
#?endif

    # if bin is true, will return Buf, Str otherwise
    method recv (Cool $chars = Inf, :$bin? = False) {
        fail('Socket not available') unless $!PIO;

#?if parrot
        if $!buffer.chars < $chars {
            my str $r = $!PIO.recv;
            unless $bin {
                my Mu $bb := pir::new__Ps('ByteBuffer');
                pir::set__vPs($bb, $r);
                $r = $bb.get_string(NORMALIZE_ENCODING('utf8'));
            }
            $!buffer ~= nqp::p6box_s($r);
        }

        my $rec;
        if $!buffer.chars > $chars {
            $rec     = substr($!buffer,0,$chars);
            $!buffer = substr($!buffer,$chars);
        } else {
            $rec     = $!buffer;
            $!buffer = '';
        }

        if $bin {
            nqp::encode(nqp::unbox_s($rec), 'binary', buf8.new);
        }
        else {
            $rec
        }
#?endif
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
#?if parrot
        my str $res;
        my str $read;
        repeat {
            my Mu $parrot_buf := pir::new__PS('ByteBuffer');
            pir::set__vPS($parrot_buf, $!PIO.read(nqp::unbox_i($bufsize - nqp::chars($res))));
            $read = $parrot_buf.get_string('binary');
            $res = nqp::concat($res, $read);
        } while nqp::chars($res) < $bufsize && nqp::chars($read);
        nqp::encode(nqp::unbox_s($res), 'binary', buf8.new);
#?endif
#?if !parrot
        my $res = buf8.new();
        my $buf;
        repeat {
            $buf := buf8.new();
            nqp::readfh($!PIO, $buf, nqp::unbox_i($bufsize - $res.elems));
            $res ~= $buf;
        } while $res.elems < $bufsize && $buf.elems;
        $res;
#?endif
    }

    method poll(Int $bitmask, $seconds) {
#?if parrot
        $!PIO.poll(
            nqp::unbox_i($bitmask), nqp::unbox_i($seconds.floor),
            nqp::unbox_i((($seconds - $seconds.floor) * 1000).Int)
        );
#?endif
#?if !parrot
        die 'Socket.poll is NYI on this backend'
#?endif
    }

    method send (Str(Cool) $string) {
        fail("Not connected") unless $!PIO;
#?if parrot
        $!PIO.send(nqp::unbox_s($string)).Bool;
#?endif
#?if !parrot
        nqp::printfh($!PIO, nqp::unbox_s($string));
        True
#?endif
    }

    method write(Blob:D $buf) {
        fail('Socket not available') unless $!PIO;
#?if parrot
        $!PIO.send(nqp::decode(nqp::decont($buf), 'binary')).Bool;
#?endif
#?if !parrot
        nqp::writefh($!PIO, nqp::decont($buf));
        True
#?endif
    }

    method close () {
        fail("Not connected!") unless $!PIO;
#?if parrot
        $!PIO.close().Bool
#?endif
#?if !parrot
        nqp::closefh($!PIO);
        $!PIO := Mu;
        True
#?endif
    }
}

# vim: ft=perl6 expandtab sw=4
