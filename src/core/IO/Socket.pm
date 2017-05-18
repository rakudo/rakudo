my role IO::Socket {
    has $!PIO;
    has Str $.encoding = 'utf8';
    has $.nl-in is rw = ["\n", "\r\n"];
    has Str:D $.nl-out is rw = "\n";

    # if bin is true, will return Buf, Str otherwise
    method recv(Cool $chars = Inf, :$bin? = False) {
        fail('Socket not available') unless $!PIO;
        if $bin {
            nqp::readfh($!PIO, nqp::decont(buf8.new),
                $chars == Inf ?? 1048576 !! $chars.Int);
        }
        else {
            nqp::p6box_s(nqp::readcharsfh($!PIO,
                $chars == Inf ?? 1048576 !! $chars.Int));
        }
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

    method print(Str(Cool) $string --> True) {
        self.write($string.encode($!encoding));
    }

    method put(Str(Cool) $string --> True) {
        self.print($string ~ $!nl-out);
    }

    method write(Blob:D $buf --> True) {
        fail('Socket not available') unless $!PIO;
        nqp::writefh($!PIO, nqp::decont($buf));
    }

    method close(--> True) {
        fail("Not connected!") unless $!PIO;
        nqp::closefh($!PIO);
        $!PIO := nqp::null;
    }

    method native-descriptor(::?CLASS:D:) {
        nqp::filenofh($!PIO)
    }
}

# vim: ft=perl6 expandtab sw=4
