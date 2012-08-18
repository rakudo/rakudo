my role IO::Socket {
    has $!PIO;
    has $!buffer = '';

    method recv (Cool $chars = $Inf) {
        fail('Socket not available') unless $!PIO;

        if $!buffer.chars < $chars {
            my str $r = $!PIO.recv;
            $!buffer ~= nqp::p6box_s($r);
        }

        if $!buffer.chars > $chars {
            my $rec  = $!buffer.substr(0, $chars);
            $!buffer = $!buffer.substr($chars);
            $rec
        } else {
            my $rec = $!buffer;
            $!buffer = '';
            $rec;
        }
    }

    method read(IO::Socket:D: Cool $bufsize as Int) {
        fail('Socket not available') unless $!PIO;
        my $buf := nqp::create(Buf);
        my Mu $parrot_buf := pir::new__PS('ByteBuffer');
        pir::set__vPS($parrot_buf, $!PIO.read(nqp::unbox_i($bufsize)));
        nqp::bindattr($buf, Buf, '$!buffer', $parrot_buf);
        $buf;
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
        $!PIO.send(nqp::getattr(nqp::p6decont($buf), Buf, '$!buffer').get_string('binary'));
    }

    method close () {
        fail("Not connected!") unless $!PIO;
        $!PIO.close().Bool
    }
}
