# An attempt at providing the framework for an introspection interface to
# the files generated with MVM_SPESH_LOG=filename setting.
#
# Such a file consistes of 1 or more sections that consist of a log section,
# an update info section with statistics received, a planning section, and
# zero or more action sections.
#
# Method "bails" provides a Bag of the names of the ops that caused a bail.

class MoarVM::Spesh {
    has @.parts;
    has $!bails;
    has $!time;
    has %!cuids;
    has %!files;
    has %!names;

    method bails() {
        $!bails //= [(+)] @.parts>>.bails;
    }
    method time() {
        $!time //= @.parts>>.time.sum;
    }

    method !process-parts(--> Nil) {
        for @.parts -> $part {
            for $part.statistics {
                %!cuids{.cuid}.push: $_;
                %!names{.name}.push: .cuid;
            }
            for $part.actions {
                %!cuids{.cuid}.push: $_;
                %!names{.name}.push: .cuid;
            }
        }
        for %!names.kv -> $name, @cuids {
            %!names{$name} := @cuids.unique.sort.List;
        }
    }

    method cuids() {
        self!process-parts unless %!cuids;
        %!cuids
    }

    method files() {
        unless %!files {
            self!process-parts;
            for %!cuids.values -> @stuff {
                given @stuff.head {
                    my $file-line = .file-line.subst(/^ 'SETTING::' /);
                    my ($file,$line) = $file-line.split(":");
                    %!files{$file}{$line} = .cuid;
                }
            }
        }
        %!files
    }

    method names() {
        self!process-parts unless %!names;
        %!names
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

    role Cuid-File-Line-Name {
        has $!cuid;
        has $!file-line;
        has $!name;

        method !cfn() {
            $.text.match(
              / (<[ \w < > - ]>*) "' (cuid: " (\d+) ", file: " (<-[ ) ]>+)/
            ).map: *.Str;
            ($!name, $!cuid, $!file-line) = ~$0, +$1, ~$2
        }

        method cuid(--> Int:D) {
            $!cuid // self!cfn[1]
        }
        method file-line(--> Str:D) {
            $!cuid // self!cfn[2]
        }
        method file(--> Str:D) {
            self.file-line.subst(/ ^ 'SETTING::'/).split(":").head
        }
        method line(--> Str:D) {
            self.file-line.subst(/ ^ 'SETTING::'/).split(":").tail
        }
        method name(--> Str:D) {
            $!name // self!cfn[0]
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

    my class Statistics does Time does Cuid-File-Line-Name {
        has $.text;

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
    class Observation does Bails does Time does Cuid-File-Line-Name {
        has $.text;
    }

    class GuardTree does Bails does Time does Cuid-File-Line-Name {
        has $.text;
    }

    class Certainty does Bails does Time does Cuid-File-Line-Name {
        has $.text;
    }

    class Specialization does Bails does Time does Cuid-File-Line-Name {
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

    method new($filename --> MoarVM::Spesh:D) {
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

    method report() {
        my @bails = self.bails.sort: -*.value;

        my str @lines;

        @lines.push: "Spesh Log Report of Process #$*PID ({
            now.DateTime.truncated-to('second')
        })";
        @lines.push: "Executing: " ~ Rakudo::Internals.PROGRAM;
#        @lines.push: "Ran for { self.time / 1000000 } seconds";
        @lines.push: "";

        @lines.push: "@bails.elems() ops got bailed";
        @lines.push: "-" x 80;
        @lines.push: sprintf("%4d: %s", .value, .key) for @bails;
        @lines.push: "-" x 80;

#        @lines.push: .raku for self.cuids>>.text;
#        @lines.push: "-" x 80;

        @lines.join("\n");
    }
}

# vim: expandtab shiftwidth=4
