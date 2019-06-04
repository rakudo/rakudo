# An attempt at providing the framework for an introspection interface to
# the files generated with MVM_SPESH_LOG=filename setting.
#
# Such a file consistes of 1 or more sections that consist of a log section,
# an update info section with statistics received, a planning section, and
# zero or more action sections.
#
# Method "bails" provides a Bag of the names of the ops that caused a bail.
#
# When used as a script, it will accept the name of a spesh file and output
# a list of all of the opcodes that caused a bail, ordered by number of bails.
#
#   $ MVM_SPESH_LOG=spesh perl6 yourcode
#   $ perl6 lib/Spesh.pm6 spesh

class Spesh {
    has @.parts;
    has $!bails;
    has $!time;
    has @!cuids;

    method bails() {
        $!bails //= [(+)] @.parts>>.bails;
    }
    method time() {
        $!time //= @.parts>>.time.sum;
    }
    method cuids() {
        @!cuids ?? @!cuids !! do {
            for @.parts -> $part {
                @!cuids[.cuid].push( $_ ) for $part.statistics;
                @!cuids[.cuid].push( $_ ) for $part.actions;
            }
            @!cuids
        }
    }

    role Bails {
        has $!bails;

        method bails(--> Bag:D) {
            $!bails //= $.text
              .comb( /:r bail ':' <-[ \n ]>* / )
              .map( *.words.tail )
              .map( { .starts-with( "(ins=" ) ?? .substr(5,*-1) !! $_ } )
              .Bag
        }
    }

    role Time {
        method time(--> Int:D) {
            $.text.match( / <after " "> \d+ <before us " "> /, :g ).sum // 0
        }
    }

    role Cuid-File-Line {
        method cuid(--> Int:D) {
            $.text.match(
              /:r <after 'cuid: ' > \d+ /
            ).Int
        }
        method file-line(--> Str:D) {
            $.text.match(
              /:r <after 'file: ' > <-[ ) ]>+ <before ')' > /
            ).Str
        }
    }

    my class Received does Time {
        has $.text;
    }

    my class Updated does Time {
        has $.text;

        method frames(--> Int:D) {
            $.text.match(
              /:r \d+ <before " frames" > /
            ).Int
        }
    }

    my class Statistics does Time does Cuid-File-Line {
        has $.text;

        method name(--> Str:D) {
            $.text.match(
              /:r <after "Latest statistics for '" > <-[']>* <before "'" > /
            ).Str
        }
        method total-hits(--> Int:D) {
            $.text.match(
              /:r <after "Total hits: " > \d+ /
            ).Int
        }
    }

    my class Planning does Time {
        has $.text;

        method specializations(--> Int:D) {
            $.text.match(
              /:r \d+ <before " specialization(s)" > /
            ).Int
        }
        method time(--> Int:D) {
            $.text.match( /:r <after "planned in "> \d+ <before "us)"> /).Int
        }
    }

    class Gathering { }  # just used for status
    class Observation does Bails does Time does Cuid-File-Line {
        has $.text;

        method name(--> Str:D) {
            $.text.match(
              /:r <after "Observed type specialization of '" > <-[']>* <before "'" > /
            ).Str
        }
    }

    class GuardTree does Bails does Time does Cuid-File-Line {
        has $.text;

        method name(--> Str:D) {
            $.text.match(
              /:r <after "Latest guard tree for '" > <-[']>* <before "'" > /
            ).Str
        }
    }

    class Certainty does Bails does Time does Cuid-File-Line {
        has $.text;

        method name(--> Str:D) {
            $.text.match(
              /:r <after "Certain specialization of '" > <-[']>* <before "'" > /
            ).Str
        }
    }

    class Specialization does Bails does Time does Cuid-File-Line {
        has $.text;

        method time(--> Int:D) {
            $.text.match(
              /:r <after 'Specialization took ' > \d+ <before 'us' > /
            ).Int
        }
        method jitted(--> Bool:D) {
            $.text.contains('JIT was successful')
        }
        method compilation-time(--> Int:D) {
            $.text.match(
              /:r <after 'compilation took ' > \d+ <before 'us' > /
            ).Int
        }
        method frame-size(--> Int:D) {
            $.text.match(
              /:r <after 'Frame size: ' > \d+ <before ' byte' > /
            ).Int
        }
        method bytecode-size(--> Int:D) {
            $.text.match(
              /:r <after 'Bytecode size: ' > \d+ <before ' byte' > /
            ).Int
        }
        method name(--> Str:D) {
            $.text.match(
              /:r <after "Spesh of '" > <-[']>* <before "' " > /
            ).Str
        }
    }

    class Part {
        has $.received;
        has $.updated;
        has @.statistics;
        has $.planning;
        has @.actions;
        has $.bails;
        has $.time;

        method bails() {
            $!bails //= [(+)] @.actions>>.bails;
        }
        method time(--> Int:D) {
            $!time //=
                $.received.time
              + $.updated.time
              + @.statistics>>.time.sum
              + $.planning.time
              + @.actions>>.time.sum
        }

        method observations()    { @.actions.grep: Observation    }
        method guardtrees()      { @.actions.grep: GuardTree      }
        method certainties()     { @.actions.grep: Certainty      }
        method specializations() { @.actions.grep: Specialization }
    }

    my constant start-Received       = "Received Logs";
    my constant start-Updated        = "Statistics Updated";
    my constant start-Statistics     = "Latest statistics";
    my constant start-Planning       = "Specialization Plan";

    my constant start-Observation    = "Observed type specialization of";
    my constant start-GuardTree      = "Latest guard tree for";
    my constant start-Certainty      = "Certain specialization of";
    my constant start-Specialization = "Specialization of";

    method new($filename --> Spesh:D) {
        my $handle = $filename.IO.open or die "Could not read $filename: $!";

        my $status = Received;

        my @parts;
        my $received;
        my $updated;
        my @statistics;
        my $planning;
        my @actions;

        my $type;
        my @text;

        sub finish() {
            @actions.push(
              $type.new(text => @text.join("\n"))
            );
        }

        sub next($next, $line) {
            @text   = $line;
            $status = Gathering;
            $type   = $next;
        }

        for $handle.lines {
            if $status === Gathering {
                if .starts-with(start-Received) {
                    finish;
                    @parts.push(Part.new(
                      received   => Received.new(text => $received),
                      updated    => Updated.new(text => $updated),
                      statistics => @statistics,
                      planning   => Planning.new(text => $planning),
                      actions    => @actions,
                    ));
                    @text = $_;
                    $status = Received;
                }
                elsif .starts-with(start-Observation) {
                    finish;
                    next(Observation, $_);
                }
                elsif .starts-with(start-GuardTree) {
                    finish;
                    next(GuardTree, $_);
                }
                elsif .starts-with(start-Certainty) {
                    finish;
                    next(Certainty, $_);
                }
                elsif .starts-with(start-Specialization) {
                    finish;
                    next(Specialization, $_);
                }
                else {
                    @text.push($_);
                }
            }

            elsif $status === Planning {
                if .starts-with(start-Observation) {
                    $planning = @text.join("\n");
                    next(Observation, $_);
                }
                elsif .starts-with(start-GuardTree) {
                    $planning = @text.join("\n");
                    next(GuardTree, $_);
                }
                elsif .starts-with(start-Certainty) {
                    $planning = @text.join("\n");
                    next(Certainty, $_);
                }
                elsif .starts-with(start-Specialization) {
                    $planning = @text.join("\n");
                    next(Specialization, $_);
                }
                elsif .starts-with(start-Received) {
                    $planning = @text.join("\n");
                    @parts.push(Part.new(
                      received        => Received.new(text => $received),
                      updated         => Updated.new(text => $updated),
                      statistics      => @statistics,
                      planning        => Planning.new(text => $planning),
                    ));
                    @text   = $_;
                    $status = Received;
                }
                else {
                    @text.push($_);
                }
            }

            elsif $status === Statistics {
                if .starts-with(start-Planning) {
                    @statistics.push(
                      Statistics.new(text => @text.join("\n"))
                    );
                    @actions = ();
                    @text   = $_;
                    $status = Planning;
                }
                elsif .starts-with(start-Statistics) {
                    @statistics.push(
                      Statistics.new(text => @text.join("\n"))
                    );
                    @text = $_;
                }
                else {
                    @text.push($_);
                }
            }

            elsif $status === Updated {
                if .starts-with(start-Statistics) {
                    $updated = @text.join("\n");
                    @statistics = ();
                    @text   = $_;
                    $status = Statistics;
                }
                else {
                    @text.push($_);
                }
            }

            elsif $status === Received {
                if .starts-with(start-Updated) {
                    $received = @text.join("\n");
                    @text   = $_;
                    $status = Updated;
                }
                else {
                    @text.push($_);
                }
            }
            else {
                die;
            }
        }

        $handle.close;

        @parts.push(Part.new(
          received   => Received.new(text => $received),
          updated    => Updated.new(text => $updated),
          statistics => @statistics,
          planning   => Planning.new(text => $planning),
          actions    => @actions,
        ));

        self.bless(:@parts)
    }
}

sub MAIN($filename where *.IO.e) {
    note "Parsing '$filename'";
    my $spesh = Spesh.new($filename);
    note "Finding bailed ops";
    my $bails = $spesh.bails;

    note "Done";
    say "Found $bails.elems() different ops getting bailed:";
    printf("%4d: %s\n", .value, .key) for $bails.sort( *.value );

    say $spesh.cuids[1]>>.file-line.Bag;
}
