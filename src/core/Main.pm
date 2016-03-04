# TODO:
# * $?USAGE
#   * Create $?USAGE at compile time
#   * Make $?USAGE available globally
# * Command-line parsing
#   * Allow both = and space before argument of double-dash args
#   * Comma-separated list values
#   * Allow exact Perl 6 forms, quoted away from shell
# * Fix remaining XXXX

my sub MAIN_HELPER($retval = 0) {
    # Do we have a MAIN at all?
    my $m = callframe(1).my<&MAIN>;
    return $retval unless $m;

    # Convert raw command line args into positional and named args for MAIN
    my sub process-cmd-args(@args is copy) {
        my (@positional-arguments, %named-arguments);
        my $stopped = False;
        while +@args {
            my $passed-value = @args.shift;
            $stopped = $passed-value eq '--';
            if !$stopped && $passed-value ~~ /^ ( '--' | '-' | ':' ) ('/'?) (<-[0..9\.]> .*) $/ {
                my ($switch, $negate, $arg) = (~$0, ?((~$1).chars), ~$2);

                with $arg.index('=') {
                    my ($name, $value) = $arg.split('=', 2);
                    $value = val($value);
                    $value = $value but False if $negate;
                    %named-arguments.push: $name => $value;
                } else {
                    %named-arguments.push: $arg => !$negate;
                }
            } else {
                @args.unshift($passed-value) unless $passed-value eq '--';
                @positional-arguments.append: @args.map: &val;
                last;
            }
        }
        @positional-arguments, %named-arguments;
    }

    # Generate $?USAGE string (default usage info for MAIN)
    my sub gen-usage() {
        my @help-msgs;
        my Pair @arg-help;

        my sub strip_path_prefix($name) {
            my $SPEC := $*SPEC;
            my ($vol, $dir, $base) = $SPEC.splitpath($name);
            $dir = $SPEC.canonpath($dir);
            for $SPEC.path() -> $elem {
                if $SPEC.catpath($vol, $elem, $base).IO.x {
                    return $base if $SPEC.canonpath($elem) eq $dir;
                    # Shadowed command found in earlier PATH element
                    return $name;
                }
            }
            # Not in PATH
            $name;
        }

        my $prog-name = $*PROGRAM-NAME eq '-e'
          ?? "-e '...'"
          !! strip_path_prefix($*PROGRAM-NAME);
        for $m.candidates -> $sub {
            next if $sub.?is-hidden-from-USAGE;
            my (@required-named, @optional-named, @positional, $docs);
            for $sub.signature.params -> $param {
                my $argument;
                if $param.named {
                    if $param.slurpy {
                        $argument  = "--<$param.usage-name()>=...";
                        @optional-named.push("[$argument]");
                    }
                    else {
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
                }
                else {
                    my $constraints  = $param.constraint_list.map(*.gist).join(' ');
                    my $simple-const = $constraints && $constraints !~~ /^_block/;
                    $argument = $param.name   ?? "<$param.usage-name()>" !!
                                $simple-const ??       $constraints                !!
                                                 '<' ~ $param.type.^name     ~ '>' ;

                    $argument = "[$argument ...]" if $param.slurpy;
                    $argument = "[$argument]"     if $param.optional;
                    @positional.push($argument);
                }
                @arg-help.push($argument => $param.WHY.contents) if $param.WHY and (@arg-help.grep:{ .key eq $argument}) == Empty;  # Use first defined
            }
            if $sub.WHY {
                $docs = '-- ' ~ $sub.WHY.contents
            }
            my $msg = join(' ', $prog-name, @required-named, @optional-named, @positional, $docs // '');
            @help-msgs.push($msg);
        }

        if @arg-help {
            @help-msgs.push('');
            my $offset = max(@arg-help.map: { .key.chars }) + 4;
            @help-msgs.append(@arg-help.map: { '  ' ~ .key ~ ' ' x ($offset - .key.chars) ~ .value });
        }

        my $usage = "Usage:\n" ~ @help-msgs.map('  ' ~ *).join("\n");
        $usage;
    }

    sub has-unexpected-named-arguments($signature, %named-arguments) {
        my @named-params = $signature.params.grep: *.named;
        return False if @named-params.grep: *.slurpy;

        my %accepts-argument = @named-params.map({ .named_names.Slip }) Z=> 1 xx *;
        for %named-arguments.keys -> $name {
            return True if !%accepts-argument{$name}
        }

        False;
    }

    # Process command line arguments
    my ($p, $n) := process-cmd-args(@*ARGS);

    # Generate default $?USAGE message
    my $usage;
    my $?USAGE := Proxy.new(
        FETCH => -> | { $usage || ($usage = gen-usage()) },
        STORE => -> | { }
    );

    # Get a list of candidates that match according to the dispatcher
    my @matching_candidates = $m.cando(Capture.new(list => $p, hash => $n));
    # Sort out all that would fail due to binding
    @matching_candidates .=grep: {!has-unexpected-named-arguments($_.signature, $n)};
    # If there are still some candidates left, try to dispatch to MAIN
    if +@matching_candidates {
        $m(|@($p), |%($n));
        return;
    }

    # We could not find the correct MAIN to dispatch to!
    # Let's try to run a user defined USAGE sub
    my $h = callframe(1).my<&USAGE>;
    if $h {
        $h();
        return;
    }

    # We could not find a user defined USAGE sub!
    # Let's display the default USAGE message
    if $n<help> {
        $*OUT.say($?USAGE);
        exit 1;
    }
    else {
        $*ERR.say($?USAGE);
        exit 2;
    }
}

# vim: ft=perl6 expandtab sw=4
