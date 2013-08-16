my role IO::Socket does IO {
    has Mu $!sock;
    has $!buffer = '';

    # if bin is true, will return Buf, Str otherwise
    method recv (Cool $chars = $Inf, :$bin? = False) {
        fail('Socket not available') unless $!sock;

        if $!buffer.chars < $chars {
            my $bytes = nqp::read($!sock, nqp::decont(blob8.new), 512);
            my $s = $bytes;
            unless $bin {
                $s = $bytes.decode('utf-8');
            }
            $!buffer ~= nqp::p6box_s($s);
        }

        my $rec;
        if $!buffer.chars > $chars {
            $rec     = $!buffer.substr(0, $chars);
            $!buffer = $!buffer.substr($chars);
        } else {
            $rec     = $!buffer;
            $!buffer = '';
        }

        $rec
    }

    method read(IO::Socket:D: Cool $bufsize as Int) {
        fail('Socket not available') unless $!sock;
        my $res = blob8.new;
        my $read;
        repeat {
            my $bytes = nqp::read($!sock, nqp::decont(blob8.new), $bufsize - $res.bytes);
            $read = $bytes;
            $res ~= $read;
        } while $res.bytes < $bufsize && $read.bytes;
        $res
    }

    method poll(Int $bitmask, $seconds) {
        return True; # FIXME
    }

    method send (Cool $string as Str) {
        fail("Not connected") unless $!sock;
        nqp::write($!sock, nqp::decont(nqp::unbox_s($string).encode('utf-8')));
        return True; # FIXME
    }

    method write(Blob:D $buf) {
        fail('Socket not available') unless $!sock;
        nqp::write($!sock, nqp::decont($buf));
        return True; # FIXME
    }

    method close () {
        fail("Not connected!") unless $!sock;
        nqp::closefh($!sock);
        return True; # FIXME
    }
}
