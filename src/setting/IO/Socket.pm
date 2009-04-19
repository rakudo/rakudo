use v6;

role IO::Socket {
    has $!PIO;

    method recv () {
        fail('Socket not available') unless $!PIO;
        my $received = $!PIO.recv();
        my $len = $received.chars;
        my $buf;
        while $len > 0 {
            $buf = $!PIO.recv();
            $received ~= $buf;
            $len = $buf.chars;
        }
        return $received;
    }

    method send (Str $string) {
        fail("Not connected") unless $!PIO;
        return $!PIO.send($string);
    }

    method close () {
        fail("Not connected!") unless $!PIO;
        return $!PIO.close();
    }
}
