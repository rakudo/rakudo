# TODO:
# * Strengthen val()
#   * Radix-notated Int
#   * Make val() available globally
# * $?USAGE
#   * Create $?USAGE at compile time
#   * Make $?USAGE available globally
# * Command-line parsing
#   * Like -- , first non-switch kills option parsing
#   * Allow : as option indicator (XXXX: no spaces before argument?)
#   * Single-dash options (don't allow spaces before argument)
#   * Allow both = and space before argument of double-dash args
#   * Non-Bool options that get negated become "but False"
#   * Comma-separated list values
#   * Allow exact Perl 6 forms, quoted away from shell
# * Fix remaining XXXX

my sub MAIN_HELPER($retval = 0) {
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
                | <[\-+]>? \d+ '/' <[\-+]>? \d+
                | <[\-+]>? \d+ '.' \d+ 'e' <[\-+]>? \d+
                | <[\-+]>? \d+ 'e' <[\-+]>? \d+
                | <[\-+]>? \d+ '.' \d+
                | <[\-+]>? \d+
                ]
            }
        };

        my $val;
        if   CLIVal.parse($v) { $val := +$v }
        else                  { $val :=  $v }
        return $val if $val ~~ Str;

        # Mix in original stringifications
        my role orig-string[$orig] {
            method Str  { $orig.Str  }
            method gist { $orig.gist }
        };
        return $val but orig-string[$v];
    }

    # Convert raw command line args into positional and named args for MAIN
    my sub process-cmd-args (@args is copy) {
        my (@positional-arguments, %named-arguments);
        while (@args) {
            my $passed_value = @args.shift;
            my $negate = False;
            if $passed_value.substr(0, 2) eq '--' {
                my $arg = $passed_value.substr(2);
                if $arg.substr(0, 1) eq '/' {
                    $arg .= substr(1) ;
                    $negate = True;
                }

                if $arg eq '' {
                    @positional-arguments.push: @args.map: &hack-val;
                    last;
                } elsif $arg.index('=').defined  {
                    my ($name , $value) = $arg.split('=', 2);
                    if $negate {
                        note "Trouble while parsing comand line argument '$arg': Cannot negate something which has an explicit value - ignoring the argument.\n";
                        next;
                    }
                    %named-arguments.push: $name => hack-val($value);
                } else {
                    %named-arguments.push: $arg => !$negate;
                }
            } else {
                # TODO: warn if argument starts with single '-'?
                @positional-arguments.push: hack-val($passed_value);
            }
        }

        return @positional-arguments, %named-arguments
    }

    # Generate $?USAGE string (default usage info for MAIN)
    my sub gen-usage () {
        my @help-msgs;
        my $prog-name = $*PROGRAM_NAME eq '-e' ?? "-e '...'" !! $*PROGRAM_NAME;
        for $m.candidates -> $sub {
            my (@required-named, @optional-named, @positional);
            for $sub.signature.params -> $param {
                my $argument;
                if $param.named {
                    my @names  = $param.named_names.reverse;
                    $argument  = @names.map('--' ~ *).join('|');
                    my $type   = $param.type;
                    $argument ~= "=<$type>" unless $type ~~ 'Bool';
                    if $param.optional {
                        @optional-named.push("[$argument]");
                    }
                    else {
                        @required-named.push($argument);
                    }
                }
                else {
                    my $constraints  = ~$param.constraints;
                    my $simple_const = $constraints && $constraints !~~ /^_block/;
                    $argument = $param.name   ?? '<' ~ $param.name.substr(1) ~ '>' !!
                                $simple_const ??       $constraints                !!
                                                 '<' ~ $param.type           ~ '>' ;

                    $argument = "[$argument ...]" if $param.slurpy;
                    $argument = "[$argument]"     if $param.optional;
                    @positional.push($argument);
                }
            }
            my $msg = join(' ', $prog-name, @required-named, @optional-named, @positional);
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
