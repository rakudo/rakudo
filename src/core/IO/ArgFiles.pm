my class IO::ArgFiles is IO::Handle {
    has $.args;
    has $.filename;
    has $!io;
    has $.ins;
    has $!nl-in = ["\x0A", "\r\n"];
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
                return Nil unless $!args;
                $!filename = $!args.shift;
            } else {
                $!filename = '-';
            }

            $!io = open($!filename, :r, :$!nl-in) ||
                fail "Unable to open file '$!filename'";
        }

        return Nil unless $!io.defined and $!io.opened;

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
        Seq.new(class :: does Iterator {
            has $!args;
            has $!iter;
            has $!limit;
            has $!next-io;
            has $!ins;

            method new(\args, \ins, \limit, \next-io) {
                my \iter = self.CREATE;
                nqp::bindattr(iter, self, '$!args', args);
                nqp::bindattr(iter, self, '$!ins', ins);
                nqp::bindattr(iter, self, '$!next-io', next-io);
                my $io = next-io.();
                if $io.defined {
                    nqp::bindattr(iter, self, '$!limit', limit);
                    nqp::bindattr(iter, self, '$!iter', $io.lines(:close).iterator);
                }
                else {
                    return $io if nqp::istype($io, Failure);
                    nqp::bindattr(iter, self, '$!limit', my $ = 0);
                }
                iter
            }

            method pull-one() {
                return IterationEnd if $!limit-- <= 0;
                my \value = $!iter.pull-one;
                if value =:= IterationEnd {
                    my $io = $!next-io.();
                    return $io if nqp::istype($io, Failure);
                    return IterationEnd unless $io.defined;
                    $!iter := $io.lines(:close).iterator;
                    self.pull-one;
                }
                else {
                    $!ins++;
                    value;
                }
            }
        }.new(self, $!ins, $l, -> { self!next-io }));
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
        DEPRECATED('nl-in');
        self.nl-in
    }

    method nl-in is rw {
        Proxy.new(
          FETCH => {
              $!nl-in
          },
          STORE => -> $, $nl-in {
              if $!io.defined {
                  Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!io, $nl-in);
              }
              $!nl-in = $nl-in;
          }
        );
    }
}

# vim: ft=perl6 expandtab sw=4
