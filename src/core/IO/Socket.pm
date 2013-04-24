my role IO::Socket is IO {
    has $!PIO;
    has $!buffer = '';

    # if bin is true, will return Buf, Str otherwise
    method recv (Cool $chars = $Inf, :$bin? = False) {
        fail('Socket not available') unless $!PIO;

        if $!buffer.chars < $chars {
            my str $r = $!PIO.recv;
            unless $bin {
                my Mu $bb := pir::new__Ps('ByteBuffer');
                pir::set__vPs($bb, $r);
                $r = $bb.get_string(PARROT_ENCODING('utf8'));
            }
            $!buffer ~= nqp::p6box_s($r);
        }

        my $rec;
        if $!buffer.chars > $chars {
            $rec     = $!buffer.substr(0, $chars);
            $!buffer = $!buffer.substr($chars);
        } else {
            $rec     = $!buffer;
            $!buffer = '';
        }

        if $bin {
            my $buf := Buf.new;
            nqp::bindattr_s($buf, Buf, '$!buffer', nqp::unbox_s($rec));
            $buf
        }
        else {
            $rec
        }
    }

    method read(IO::Socket:D: Cool $bufsize as Int) {
        fail('Socket not available') unless $!PIO;
        my $res = Buf.new;
        my $buf;
        repeat {
            $buf := nqp::create(Buf);
            my Mu $parrot_buf := pir::new__PS('ByteBuffer');
            pir::set__vPS($parrot_buf, $!PIO.read(nqp::unbox_i($bufsize - $res.elems)));
            nqp::bindattr_s($buf, Buf, '$!buffer',
                    $parrot_buf.get_string('binary'));
            $res = $res ~ $buf;
        } while $res.elems < $bufsize && $buf.elems;
        $res;
    }

    method poll(Int $bitmask, $seconds) {
        $!PIO.poll(
            nqp::unbox_i($bitmask), nqp::unbox_i($seconds.floor),
            nqp::unbox_i((($seconds - $seconds.floor) * 1000).Int)
        );
    }

    method send (Cool $string as Str) {
        fail("Not connected") unless $!PIO;
        $!PIO.send(nqp::unbox_s($string)).Bool;
    }

    method write(Buf:D $buf) {
        fail('Socket not available') unless $!PIO;
        $!PIO.send(nqp::unbox_s( $buf.decode('binary') )).Bool;
    }

    method close () {
        fail("Not connected!") unless $!PIO;
        $!PIO.close().Bool
    }
}
