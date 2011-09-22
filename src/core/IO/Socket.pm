my role IO::Socket {
    has $!PIO;
    has $!buffer = '';

    method recv (Cool $bufsize as Int = $Inf) {
        fail('Socket not available') unless $!PIO;
        my $received;
        while $bufsize > $!buffer.bytes {
            $received = nqp::p6box_s($!PIO.recv());
            last unless $received.chars;
            $!buffer ~= $received;
        }
        if $bufsize == $Inf {
            $received = $!buffer;
            $!buffer = '';
        } else {
            $received = $!buffer.substr(0, $bufsize);
            $!buffer .= substr($bufsize);
        }
        return $received;
    }

    method read(IO::Socket:D: Cool $bufsize as Int) {
        fail('Socket not available') unless $!PIO;
        my $buf := nqp::create(Buf);
        my Mu $parrot_buf := pir::new__PS('ByteBuffer');
        pir::set__vPS($parrot_buf, $!PIO.read(nqp::unbox_i($bufsize)));
        nqp::bindattr($buf, Buf, '$!buffer', $parrot_buf);
        $buf;
    }

    method send (Cool $string as Str) {
        fail("Not connected") unless $!PIO;
        return nqp::p6bool($!PIO.send($string));
    }

    method close () {
        fail("Not connected!") unless $!PIO;
        return nqp::p6bool($!PIO.close());
    }
}
