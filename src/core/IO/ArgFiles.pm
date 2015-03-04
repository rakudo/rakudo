my class IO::ArgFiles {
    has $.args;
    has $.filename;
    has $!io;
    has $.ins;
    has $!nl = "\n";

    method eof() {
        ! $!args && $!io.opened && $!io.eof
    }

    method get() {
        unless $!io.defined && $!io.opened {
            $!filename = $!args ?? $!args.shift !! '-';
            $!io = open($!filename, :r, :nl($!nl)) ||
                fail "Unable to open file '$!filename'";
        }
        my $x = $!io.get;
        while !$x.defined {
            $!io.close;
            $!io = IO::Handle;
            fail "End of argfiles reached" unless $!args;
            $x = self.get;
        }
        $!ins++;
        $x;
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

    method nl {
        Proxy.new(
          FETCH => {
              $!nl
          },
          STORE => -> $, $nl is copy {
              if $!io.defined {
                  nqp::setinputlinesep($!io, nqp::unbox_s($!nl = $nl));
              }
              $!nl = $nl;
          }
        );
    }
}

{
    my @ARGS;
    my Mu $argiter := nqp::getcurhllsym('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    nqp::bindkey(nqp::who(PROCESS), '@ARGS', @ARGS);
    PROCESS::<$ARGFILES> = IO::ArgFiles.new(:args(@ARGS));
}

# vim: ft=perl6 expandtab sw=4
