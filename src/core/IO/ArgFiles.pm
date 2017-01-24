my class IO::ArgFiles is IO::Handle {
    has $.args;
    has $.filename;
    has $!io;
    has Int $.ins = 0;
    has $!nl-in = ["\x0A", "\r\n"];
    has $!has-args;

    # returns True only if at the end of all input files
    method eof() {
        # make sure $!has-args is in the proper state since !next-io() may not have been called
        $!has-args = ?$!args unless $!has-args.defined;

        if $!has-args {
            # if there are files left it can't be the end
            if $!args {
                False
            }
            # $!io might be opened to the last file so ask it
            elsif $!io.defined && $!io.opened {
                $!io.eof
            }
            # $!args must have been exhausted
            else {
                True
            }
        }
        # TODO should probably get the STDIN filehandle the same way as !next-io()
        elsif $*IN.opened {
            # ask $*IN because there was no args
            $*IN.eof
        }
        else {
            # there is no input available
            True
        }
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

    proto method lines(|) {*}
    multi method lines() {
        Seq.new(class :: does Iterator {
            has $!args;
            has $!iter;
            has $!next-io;
            has Int $!ins;

            method new(\args, \ins, \next-io) {
                my \iter = nqp::create(self);
                nqp::bindattr(iter, self, '$!args', args);
                nqp::bindattr(iter, self, '$!ins', ins);
                nqp::bindattr(iter, self, '$!next-io', next-io);
                my $io = next-io.();
                if $io.defined {
                    nqp::bindattr(iter, self, '$!iter', $io.lines(:close).iterator);
                }
                else {
                    return $io if nqp::istype($io, Failure);
                }
                iter
            }

            method pull-one() {
                nqp::stmts(
                  (nqp::unless(nqp::defined($!iter), return IterationEnd)),
                  (my \value = $!iter.pull-one),
                  nqp::if(nqp::eqaddr(value, IterationEnd),
                    nqp::stmts(
                      (my $io = $!next-io.()),
                      nqp::if(nqp::istype($io, Failure), return $io),
                      nqp::unless(nqp::defined($io), return IterationEnd),
                      ($!iter := $io.lines(:close).iterator),
                      self.pull-one),
                    nqp::stmts(
                      ($!ins = nqp::add_I(nqp::decont($!ins), 1, Int)),
                      value)))
            }
        }.new(self, $!ins, -> { self!next-io }));
    }
    multi method lines($limit) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines
          !! self.lines[ lazy 0 .. $limit.Int - 1 ]
    }

    method slurp(IO::ArgFiles:D: |c) {
        # NOTE: $.filename and $.ins will be incorrect after this is called

        # make sure $!has-args is in the proper state since !next-io() may not have been called
        $!has-args = ?$!args unless $!has-args.defined;

        # TODO should probably get the STDIN filehandle the same way as !next-io()
        return $*IN.slurp-rest(|c) unless $!has-args;

        my @chunks;
        if $!io.defined && $!io.opened {
            @chunks.push: $!io.slurp-rest(:close, |c);
        }

        while $!args {
            @chunks.push: slurp $!args.shift, |c;
        }

        # TODO Should this be a failure?
        return Nil unless @chunks;

        [~] @chunks;
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
