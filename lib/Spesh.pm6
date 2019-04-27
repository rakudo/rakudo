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
    has $.bails;

    method bails() {
        $!bails //= [(+)] @.parts>>.bails;
    }

    class Part {
        has $.received;
        has $.updated;
        has @.statistics;
        has $.planning;
        has @.actions;
        has $.bails;

        method bails() {
            $!bails //= [(+)] @.actions>>.bails;
        }
    }

    my class Received   { has $.text }
    my class Updated    { has $.text }
    my class Statistics { has $.text }
    my class Planning   { has $.text }

    role Bails {
        has $.bails;

        method bails() {
            $!bails //= $.text
              .comb( / bail ':' <-[ \n ]>* / )
              .map( *.words.tail )
              .map( { .starts-with( "(ins=" ) ?? .substr(5,*-1) !! $_ } )
              .Bag;
        }
    }

    class Gathering { }  # just used for status
    class Observation does Bails    { has $.text }
    class GuardTree does Bails      { has $.text }
    class Certainty does Bails      { has $.text }
    class Specialization does Bails { has $.text }

    my constant start-Received       = "Received Logs";
    my constant start-Updated        = "Statistics Updated";
    my constant start-Statistics     = "Latest statistics";
    my constant start-Planning       = "Specialization Plan";

    my constant start-Observation    = "Observed type specialization of";
    my constant start-GuardTree      = "Latest guard tree for";
    my constant start-Certainty      = "Certain specialization of";
    my constant start-Specialization = "Specialization of";

    method new($filename) {
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
}
