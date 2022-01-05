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
            $!bails //= @.text
              .map( {
                  if .contains('bail:') {
                      .starts-with( "(ins=" ) ?? .substr(5,*-1) !! $_
                        given .words.tail
                  }
              } )
              .Bag
        }
    }

    role Time {
        method time(--> Int:D) {
            @.text
              .grep( {
                  if .contains('us ') {
                      +.match( / \d+ <before 'us '> / )
                  }
              } )
              .sum // 0
        }
    }

    role Cuid-File-Line-Name {
        has Int $!cuid;
        has Str $!file-line;
        has Str $!name;

        method !cfn() {
            return ($!name, $!cuid, $!file-line) =
              ~$0, +$1, $2.Str.subst(/ ^ 'SETTING::'/)
              if .contains("' (cuid: ") && .match:
              / (<[ \w < > - ]>*) "' (cuid: " (\d+) ", file: " (<-[ ) ]>+)/
              for @.text;

            ($!name, $!cuid, $!file-line) = "", 0, ""
        }

        method cuid(--> Int:D) {
            $!cuid // self!cfn[1]
        }
        method file-line(--> Str:D) {
            $!file-line // self!cfn[2]
        }
        method file(--> Str:D) {
            self.file-line.split(":").head
        }
        method line(--> Int:D) {
            +self.file-line.split(":").tail
        }
        method name(--> Str:D) {
            $!name // self!cfn[0]
        }
    }

    my class Received does Time {
        has @.text;
    }

    my class Updated does Time {
        has @.text;
        has Int $!frames;

        method !frames(--> Int:D) {
            return .match(/:r \d+ <before ' frames'> /).Int
              if .contains(' frames')
              for @!text;
        }
        method frames(--> Int:D) { $!frames //= self!frames }
    }

    my class Statistics does Time does Cuid-File-Line-Name {
        has @.text;

        method total-hits(--> Int:D) {
            return .substr(11).Int
              if .starts-with('Total hits: ')
              for @.text;

            0
        }
    }

    my class Planning does Time {
        has @.text;

        method specializations(--> Int:D) {
            return .words.head.Int
              if .contains(' specialization(s) ')
              for @.text;

            0
        }
        method time(--> Int:D) {
            return .match( /:r <after '(planned in '> \d+ /).Int
              if .contains('(planned in ')
              for @.text;

            0
        }
    }

    class Gathering { }  # just used for status
    class Observation does Bails does Time does Cuid-File-Line-Name {
        has @.text;
    }

    class GuardTree does Bails does Time does Cuid-File-Line-Name {
        has @.text;
    }

    class Certainty does Bails does Time does Cuid-File-Line-Name {
        has @.text;
    }

    class Specialization does Bails does Time does Cuid-File-Line-Name {
        has Str  @.text;
        has Int  $!time;
        has Int  $!total-time;
        has Int  $!compilation-time;
        has Int  $!frame-size;
        has Int  $!bytecode-size;
        has Bool $!jitted;

        method !time(--> Int:D) {
            return .match(/:r <after 'Specialization took '> \d+ /).Int
              if .starts-with('Specialization took ')
              for @!text;
            0
        }
        method time(--> Int:D) { $!time //= self!time }

        method !total-time(--> Int:D) {
            return .match(/:r <after '(total '> \d+ /).Int
              if .contains('(total ')
              for @.text;
            0
        }
        method total-time(--> Int:D) { $!total-time //= self!total-time }

        method !jitted(--> Bool:D) {
            return True
              if .starts-with('JIT was successful')
              for @.text;
            False
        }
        method jitted(--> Bool:D) { $!jitted //= self!jitted }

        method !compilation-time(--> Int:D) {
            return .match(/:r <after 'compilation took ' > \d+ /).Int
              if .starts-with('JIT was ')
              for @.text;
            0
        }
        method compilation-time(--> Int:D) {
            $!compilation-time //= self!compilation-time
        }

        method !frame-size(--> Int:D) {
            return .match(/:r <after 'Frame size: '> \d+ /).Int
              if .starts-with('Frame size: ')
              for @.text;
            0
        }
        method frame-size(--> Int:D) { $!frame-size //= self!frame-size }

        method !bytecode-size(--> Int:D) {
            return .match(/:r <after 'Bytecode size: '> \d+ /).Int
              if .contains('Bytecode size: ')
              for @.text;
            0
        }
        method bytecode-size(--> Int:D) {
            $!bytecode-size //= self!bytecode-size
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

        sub finish(--> Nil) { @actions.push: $type.new(:@text) }

        sub next($next, $line--> Nil) {
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

    method actions()         { self.parts.map: *.actions.Slip       }
    method observations()    { self.actions.grep: Observation    }
    method guardtrees()      { self.actions.grep: GuardTree      }
    method certainties()     { self.actions.grep: Certainty      }
    method specializations() { self.actions.grep: Specialization }

    method report($head = 5) {

        my str @lines;
        @lines.push: "Spesh Log Report of Process #$*PID ({
            now.DateTime.truncated-to('second')
        })";
        @lines.push: "Executing: " ~ Rakudo::Internals.PROGRAM;
#        @lines.push: "Ran for { self.time / 1000000 } seconds";
        @lines.push: "";

        sub add-specializations(@specializations, $not = "") {
            return if !@specializations;

            @lines.push: "S{
                "lowest $head s" if @specializations > $head;
            }pecializations that did$not get JITted (times in us)";
            @lines.push: "-" x 80;
            @lines.push: 'specia | compil |  total | file:line (name)';
            @lines.push: "-------|--------|--------|" ~ "-" x 54;
            @lines.push: sprintf( '%6d | %6d | %6d | %s',
              .time, .compilation-time, .total-time,
              "{.file-line}{ " ({.name})" if .name }"
            ) for @specializations.head($head);
            @lines.push: "-" x 80;
            @lines.push: "";
        }

        my @specializations = self.specializations;
        add-specializations
          @specializations.grep(!*.jitted).sort(-*.compilation-time),
          " *NOT*";

        add-specializations
          @specializations.grep(*.jitted).sort(-*.compilation-time);

        if self.bails.sort: -*.value -> @bails {
            @lines.push: @bails > $head
              ?? "$head most occuring ops that prevented JITting of code"
              !! "Ops that prevented JITting of code";
            @lines.push: "-" x 80;
            @lines.push: sprintf("%4d: %s", .value, .key)
              for @bails.head($head);
            @lines.push: "-" x 80;
        }

#        @lines.push: .raku for self.cuids>>.text;
#        @lines.push: "-" x 80;

        @lines.join("\n");
    }
}

# vim: expandtab shiftwidth=4
