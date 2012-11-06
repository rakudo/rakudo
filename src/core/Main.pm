# TODO:
# * Align number parsing to STD
#   * Rakudo's .Numeric
#     * complex numbers
#   * Rakudo's grammar
#   * val()
# * Strengthen val()
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

        my $val;
        if    $v ~~ /^ 'Bool::'?'False' $/ { $val := Bool::False }
        elsif $v ~~ /^ 'Bool::'?'True'  $/ { $val := Bool::True  }
        elsif $v.Numeric.defined           { $val := +$v }
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
        $PROCESS::ARGFILES = IO::ArgFiles.new(:args(@args));
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
                    my $constraints  = $param.constraint_list.map(*.gist).join(' ');
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

    sub has-unexpected-named-arguments($signature, %named-arguments) {
        my @named-params = $signature.params.grep: *.named;
        return False if @named-params.grep: *.slurpy;

        my %accepts-argument = @named-params.map({ .named_names }) Z=> 1 xx *;
        for %named-arguments.keys -> $name {
            return True if !%accepts-argument{$name}
        }

        return False;
    }

    # Process command line arguments
    my ($p, $n) = process-cmd-args(@*ARGS).lol;

    # Generate default $?USAGE message
    my $?USAGE = gen-usage();

    # Get a list of candidates that match according to the dispatcher
    my @matching_candidates = $m.cando(Capture.new(list => $p, hash => $n));
    # Sort out all that would fail due to binding
    @matching_candidates .=grep: {!has-unexpected-named-arguments($_.signature, $n)};
    # If there are still some candidates left, try to dispatch to MAIN
    if +@matching_candidates {
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
