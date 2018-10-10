# TODO:
# * Command-line parsing
#   * Allow both = and space before argument of double-dash args
#   * Comma-separated list values
#   * Allow exact Perl 6 forms, quoted away from shell
# * Fix remaining XXXX

my sub MAIN_HELPER($IN-as-ARGSFILES, $retval = 0) {
    # Do we have a MAIN at all?
    my $main := callframe(1).my<&MAIN>;
    return $retval unless $main;

    my %SUB-MAIN-OPTS  := %*SUB-MAIN-OPTS // {};
    my $no-named-after := nqp::isfalse(%SUB-MAIN-OPTS<named-anywhere>);

    sub thevalue(\a) {
        ((my \type := ::(a)) andthen Metamodel::EnumHOW.ACCEPTS(type.HOW))
          ?? type
          !! val(a)
    }

    # Convert raw command line args into positional and named args for MAIN
    my sub process-cmd-args(@args is copy --> Capture:D) {
        my $positional := nqp::create(IterationBuffer);
        my %named;

        while @args {
            my str $passed-value = @args.shift;

            # rest considered to be non-parsed
            if nqp::iseq_s($passed-value,'--') {
                nqp::push($positional, thevalue($_)) for @args;
                last;
            }

            # no longer accepting nameds
            elsif $no-named-after && nqp::isgt_i(nqp::elems($positional),0) {
                nqp::push($positional, thevalue($passed-value));
            }

            # named
            elsif $passed-value
              ~~ /^ [ '--' | '-' | ':' ] ('/'?) (<-[0..9\.]> .*) $/ {  # 'hlfix
                my str $arg = $1.Str;
                my $split  := nqp::split("=",$arg);

                # explicit value
                if nqp::isgt_i(nqp::elems($split),1) {
                    my str $name = nqp::shift($split);
                    %named.push: $name => $0.chars
                      ?? thevalue(nqp::join("=",$split)) but False
                      !! thevalue(nqp::join("=",$split));
                }

                # implicit value
                else {
                    %named.push: $arg => !($0.chars);
                }
            }

            # positional
            else {
                nqp::push($positional, thevalue($passed-value));
            }
        }
        Capture.new( list => $positional.List, hash => %named )
    }

    # Select candidates for which to create USAGE string
    sub usage-candidates($capture) {
        my @candidates = $main.candidates;
        my @positionals = $capture.list;

        my @candos;
        while @positionals && !@candos {

            # Find candidates on which all these positionals match
            @candos = @candidates.grep: -> $sub {
                my @params = $sub.signature.params;
                (^@positionals).first( -> int $i {
                    !(@params[$i].constraints.ACCEPTS(@positionals[$i]))
                } ).defined.not;
            }
            @positionals.pop;
        }
        (@candos || $main.candidates)
          .grep: { nqp::not_i(nqp::can($_,'is-hidden-from-USAGE')) }
    }

    # Generate $?USAGE string (default usage info for MAIN)
    my sub gen-usage($capture) {
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

        my $prog-name = %*ENV<PERL6_PROGRAM_NAME> || $*PROGRAM-NAME;
        $prog-name = $prog-name eq '-e'
          ?? "-e '...'"
          !! strip_path_prefix($prog-name);

        for usage-candidates($capture) -> $sub {
            my @required-named;
            my @optional-named;
            my @positional;
            my $docs;

            for $sub.signature.params -> $param {
                my $argument;

                my int $literals-as-constraint = 0;
                my int $total-constraints = 0;
                my $constraints = ~unique $param.constraint_list.map: {
                    ++$total-constraints;
                    nqp::if(
                      nqp::istype($_, Callable),
                      'where { ... }',
                      nqp::stmts(
                        (my \g = .gist),
                        nqp::if(
                          nqp::isconcrete($_),
                          nqp::stmts(
                            ++$literals-as-constraint,
                            g), # we constrained by some literal; gist as is
                          nqp::substr(g, 1, nqp::chars(g)-2))))
                          # ^ remove ( ) parens around name in the gist
                }
                $_ eq 'where { ... }' and $_ = "$param.type.^name() $_"
                    with $constraints;

                if $param.named {
                    if $param.slurpy {
                        if $param.name { # ignore anon *%
                            $argument  = "--<$param.usage-name()>=...";
                            @optional-named.push("[$argument]");
                        }
                    }
                    else {
                        my @names  = $param.named_names.reverse;
                        $argument  = @names.map({($^n.chars == 1 ?? '-' !! '--') ~ $^n}).join('|');
                        if $param.type !=== Bool {
                            $argument ~= "=<{
                                $constraints || $param.type.^name
                            }>";
                            if Metamodel::EnumHOW.ACCEPTS($param.type.HOW) {
                                my $options = $param.type.^enum_values.keys.sort.Str;
                                $argument ~= $options.chars > 50
                                  ?? ' (' ~ substr($options,0,50) ~ '...'
                                  !! " ($options)"
                            }
                        }
                        if $param.optional {
                            @optional-named.push("[$argument]");
                        }
                        else {
                            @required-named.push($argument);
                        }
                    }
                }
                else {
                    $argument = $param.name
                        ?? "<$param.usage-name()>"
                        !! $constraints
                            ?? ($literals-as-constraint == $total-constraints)
                                ?? $constraints
                                !! "<{$constraints}>"
                            !! "<$param.type.^name()>";

                    $argument  = "[$argument ...]" if $param.slurpy;
                    $argument  = "[$argument]"     if $param.optional;
                    if $total-constraints
                    && $literals-as-constraint == $total-constraints {
                        $argument .= trans(["'"] => [q|'"'"'|])
                            if $argument.contains("'");
                        $argument  = "'$argument'"
                            if $argument.contains(' ' | '"');
                    }
                    @positional.push($argument);
                }
                @arg-help.push($argument => $param.WHY.contents) if $param.WHY and (@arg-help.grep:{ .key eq $argument}) == Empty;  # Use first defined
            }
            if $sub.WHY {
                $docs = '-- ' ~ $sub.WHY.contents
            }
            my $msg = $no-named-after
              ?? join(' ', $prog-name, @required-named, @optional-named, @positional, $docs // '')
              !! join(' ', $prog-name, @positional, @required-named, @optional-named, $docs // '');
            @help-msgs.push($msg);
        }

        if @arg-help {
            @help-msgs.push('');
            my $offset = max(@arg-help.map: { .key.chars }) + 4;
            @help-msgs.append(@arg-help.map: { '  ' ~ .key ~ ' ' x ($offset - .key.chars) ~ .value });
        }

        "Usage:\n" ~ @help-msgs.map('  ' ~ *).join("\n")
    }

    sub has-unexpected-named-arguments($signature, %named-arguments) {
        my @named-params = $signature.params.grep: *.named;
        return False if @named-params.first: *.slurpy;

        my %accepts-argument is Set = @named-params.map( *.named_names.Slip );
        return True unless %accepts-argument{$_} for %named-arguments.keys;
        False
    }

    # Process command line arguments
    my $capture := process-cmd-args(@*ARGS);

    # Generate default $?USAGE message
    my $usage;
    my $*USAGE := Proxy.new(
        FETCH => -> | { $usage ||= gen-usage($capture) },
        STORE => -> | {
            die 'Cannot assign to $*USAGE. Please use `sub USAGE {}` to '
                ~ 'output custom usage message'
        }
    );

    # Get a list of candidates that match according to the dispatcher
    my @matching_candidates = $main.cando($capture);

    # Sort out all that would fail due to binding
    @matching_candidates .=
      grep: { !has-unexpected-named-arguments(.signature, $capture.hash) };

    # If there are still some candidates left, try to dispatch to MAIN
    if +@matching_candidates {
        if $IN-as-ARGSFILES {
            my $*ARGFILES := IO::ArgFiles.new: (my $in := $*IN),
                :nl-in($in.nl-in), :chomp($in.chomp), :encoding($in.encoding),
                :bin(nqp::hllbool(nqp::isfalse($in.encoding)));
            $main(|$capture).sink;
        }
        else {
            $main(|$capture).sink;
        }
    }

    # We could not find the correct MAIN to dispatch to!
    # Let's try to run a user defined USAGE sub
    elsif callframe(1).my<&USAGE> -> $usage {
        $usage();
    }

    # We could not find a user defined USAGE sub!
    # Let's display the default USAGE message
    elsif $capture<help> {
        $*OUT.say($*USAGE);
        exit 0;
    }
    else {
        $*ERR.say($*USAGE);
        exit 2;
    }
}

# vim: ft=perl6 expandtab sw=4
