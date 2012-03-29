# TODO:
# * Align number parsing to STD
#   * Rakudo's .Numeric
#     * complex numbers
#     * nums with no integer part (e.g. '.5')
#     * any radix number beyond most basic:
#       - ratios: '0xfeed/0xf00d' or ':16(feed)/:16(f00d)'
#       - nums:   ':16<feed.f00d>'
#       - * base ** exp
#   * Rakudo's grammar
#   * val()
# * Strengthen val()
#   * Check that number in ':30<foo>' radix notation is sane
#   * Make parsing match Rakudo (and STD, where possible)
#   * Make val() available globally
# * $?USAGE
#   * Create $?USAGE at compile time
#   * Make $?USAGE available globally
# * Command-line parsing
#   * Allow both = and space before argument of double-dash args
#   * Comma-separated list values
#   * Allow exact Perl 6 forms, quoted away from shell
# * Fix remaining XXXX

my sub MAIN_HELPER($retval = 0) is hidden_from_backtrace {
    # Do we have a MAIN at all?
    my $m = callframe(1).my<&MAIN>;
    return $retval unless $m;

    # Temporary stand-in for magic val() routine
    my sub hack-val ($v) {
        # Convert to native type if appropriate
        my grammar CLIVal {
            token TOP     { ^ <numlike> $ }

            token numlike {
                [
                | <[+\-]>? <decint> '/' <[+\-]>? <decint>
                | <[+\-]>? <decint> '.' <decint> <escale>
                | <[+\-]>? <decint>              <escale>
                | <[+\-]>? <decint> '.' <decint>
                | <[+\-]>? <integer>
                | <[+\-]>? ':' \d+ '<' <alnumint> '>'
                | <[+\-]>? 'Inf'
                | 'NaN'
                ]
            }

            token binint   { <[ 0..1 ]>+ [ _ <[ 0..1 ]>+ ]* }
            token octint   { <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]* }
            token hexint   { <[ 0..9 a..f A..F ]>+ [ _ <[ 0..9 a..f A..F ]>+ ]* }
            token alnumint { <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* }
            token decint   { \d+ [ _ \d+ ]* }
            token escale   { <[Ee]> <[+\-]>? <decint> }

            token integer {
                [
                | 0 [ b '_'? <binint>
                    | o '_'? <octint>
                    | x '_'? <hexint>
                    | d '_'? <decint>
                    ]
                | <decint>
                ]
            }
        };

        my $val;
        if    $v ~~ /^ 'Bool::'?'False' $/ { $val := Bool::False }
        elsif $v ~~ /^ 'Bool::'?'True'  $/ { $val := Bool::True  }
        elsif CLIVal.parse($v)             { $val := +$v }
        else                               { return $v   }

        # Mix in original stringifications
        my role orig-string[$orig] {
                  method Str  ()      { $orig.Str  }
            multi method gist (Mu:D:) { $orig.gist }
        };
        return $val but orig-string[$v];
    }

    # Convert raw command line args into positional and named args for MAIN
    my sub process-cmd-args (@args is copy) {
        my (@positional-arguments, %named-arguments);
        while (@args) {
            my $passed-value = @args.shift;
            if $passed-value ~~ /^ ( '--' | '-' | ':' ) ('/'?) (<-[0..9\.]> .*) $/ {
                my ($switch, $negate, $arg) = (~$0, ?((~$1).chars), ~$2);

                if $arg.index('=').defined  {
                    my ($name, $value) = $arg.split('=', 2);
                    $value = hack-val($value);
                    $value = $value but False if $negate;
                    %named-arguments.push: $name => $value;
                } else {
                    %named-arguments.push: $arg => !$negate;
                }
            } else {
                @args.unshift($passed-value) unless $passed-value eq '--';
                @positional-arguments.push: @args.map: &hack-val;
                last;
            }
        }

        return @positional-arguments, %named-arguments;
    }

    # Generate $?USAGE string (default usage info for MAIN)
    my sub gen-usage () {
        my @help-msgs;
        my $prog-name = $*PROGRAM_NAME eq '-e' ?? "-e '...'" !! $*PROGRAM_NAME;
        for $m.candidates -> $sub {
            my (@required-named, @optional-named, @positional, $docs);
            for $sub.signature.params -> $param {
                my $argument;
                if $param.named {
                    my @names  = $param.named_names.reverse;
                    $argument  = @names.map({($^n.chars == 1 ?? '-' !! '--') ~ $^n}).join('|');
                    $argument ~= "=<{$param.type.^name}>" unless $param.type === Bool;
                    if $param.optional {
                        @optional-named.push("[$argument]");
                    }
                    else {
                        @required-named.push($argument);
                    }
                }
                else {
                    my $constraints  = ~$param.constraint_list;
                    my $simple-const = $constraints && $constraints !~~ /^_block/;
                    $argument = $param.name   ?? '<' ~ $param.name.substr(1) ~ '>' !!
                                $simple-const ??       $constraints                !!
                                                 '<' ~ $param.type.^name     ~ '>' ;

                    $argument = "[$argument ...]" if $param.slurpy;
                    $argument = "[$argument]"     if $param.optional;
                    @positional.push($argument);
                }
            }
            if $sub.WHY {
                $docs = '-- ' ~ $sub.WHY.content
            }
            my $msg = join(' ', $prog-name, @required-named, @optional-named, @positional, $docs // '');
            @help-msgs.push($msg);
        }
        my $usage = "Usage:\n" ~ @help-msgs.map('  ' ~ *).join("\n");
        return $usage;
    }

    # Process command line arguments
    my ($p, $n) = process-cmd-args(@*ARGS).lol;

    # Generate default $?USAGE message
    my $?USAGE = gen-usage();

    # If dispatch to MAIN is possible, do so
    if $m.candidates_matching(|@($p), |%($n)).elems {
        return $m(|@($p), |%($n));
    }

    # We could not find the correct MAIN to dispatch to!
    # Let's try to run a user defined USAGE sub
    my $h = callframe(1).my<&USAGE>;
    return $h() if $h;

    # We could not find a user defined USAGE sub!
    # Let's display the default USAGE message
    if ($n<help>) {
        $*OUT.say($?USAGE);
        exit 1;
    }
    else {
        $*ERR.say($?USAGE);
        exit 2;
    }
}
