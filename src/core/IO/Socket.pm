my role IO::Socket does IO {
#?if parrot
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
                $r = $bb.get_string(NORMALIZE_ENCODING('utf8'));
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
            nqp::encode(nqp::unbox_s($rec), 'binary', buf8.new);
        }
        else {
            $rec
        }
    }

    method read(IO::Socket:D: Cool $bufsize as Int) {
        fail('Socket not available') unless $!PIO;
        my str $res;
        my str $read;
        repeat {
            my Mu $parrot_buf := pir::new__PS('ByteBuffer');
            pir::set__vPS($parrot_buf, $!PIO.read(nqp::unbox_i($bufsize - nqp::chars($res))));
            $read = $parrot_buf.get_string('binary');
            $res = nqp::concat($res, $read);
        } while nqp::chars($res) < $bufsize && nqp::chars($read);
        nqp::encode(nqp::unbox_s($res), 'binary', buf8.new);
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

    method write(Blob:D $buf) {
        fail('Socket not available') unless $!PIO;
        $!PIO.send(nqp::decode(nqp::decont($buf), 'binary')).Bool;
    }

    method close () {
        fail("Not connected!") unless $!PIO;
        $!PIO.close().Bool
    }
#?endif
}
