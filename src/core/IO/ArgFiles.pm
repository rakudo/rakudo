my class IO::ArgFiles is IO::Handle {
    has $.args;
    has $.filename;
    has $!io;
    has $.ins;
    has $!nl = "\n";
    has $!has-args;

    method eof() {
        ! $!args && $!io.opened && $!io.eof
    }

    method !next-io() {
        unless $!has-args.defined {
            $!has-args = ?$!args;
        }

        unless $!io.defined && $!io.opened {
            if $!has-args {
                return Str unless $!args;
                $!filename = $!args.shift;
            } else {
                $!filename = '-';
            }

            $!io = open($!filename, :r, :nl($!nl)) ||
                fail "Unable to open file '$!filename'";
        }

        return Str unless $!io.defined and $!io.opened;

        $!io;
    }

    method get() {
        unless $!io.defined and $!io.opened {
            (return $_ unless .defined) given self!next-io;
        }

        my $line;
        repeat {
            $line = $!io.get;
            unless $line.defined {
                $!io.close;
                $!io = IO::Handle;
                (return $_ unless .defined) given self!next-io;
            }
        } until $line.defined;
        $!ins++;
        $line;
    }

    method lines($limit = *) {
        my $l = nqp::istype($limit,Whatever) ?? Inf !! $limit;
        gather while $l-- > 0 {
           take $.get // last;
        }
    }
    method slurp(IO::ArgFiles:D:) {
        my @chunks;
        if $!io && $!io.opened {
            @chunks.push: nqp::p6box_s($!io.readall);
            $!io.close;
        }
        while $!args {
            @chunks.push: slurp $!args.shift;
        }
        return $*IN.slurp-rest unless @chunks;
        @chunks.join;
    }

    method nl is rw {
        Proxy.new(
          FETCH => {
              $!nl
          },
          STORE => -> $, $nl {
              if $!io.defined {
                  nqp::setinputlinesep($!io, nqp::unbox_s($!nl = $nl));
              }
              $!nl = $nl;
          }
        );
    }
}

# vim: ft=perl6 expandtab sw=4
