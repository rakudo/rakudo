# TODO:
# * Command-line parsing
#   * Allow both = and space before argument of double-dash args
#   * Comma-separated list values
#   * Allow exact Raku forms, quoted away from shell
# * Fix remaining XXXX

my sub RUN-MAIN(&main, $mainline, :$in-as-argsfiles) {

    # Set up basic info
    my %caller-my := callframe(1).my;
    my $provided-a-to-c := %caller-my<&ARGS-TO-CAPTURE>;
    my $provided-g-u    := %caller-my<&GENERATE-USAGE>;

    my &args-to-capture := $provided-a-to-c // &default-args-to-capture;
    my &generate-usage  := $provided-g-u    // &default-generate-usage;
    my %sub-main-opts   := %*SUB-MAIN-OPTS // {};

    # Set up proxy for default generated usage
    my $usage-produced;
    my $*USAGE := Proxy.new(
        FETCH => -> | {
            $usage-produced //= default-generate-usage(&main)
        },
        STORE => -> | {
            die 'Cannot assign to $*USAGE. Please create a '
                ~ '`sub GENERATE-USAGE {}` to generate custom usage message'
        }
    );

    # Module loaded that depends on the old MAIN_HELPER interface and
    # does not provide the new interface?
    if !$provided-a-to-c && %caller-my<&MAIN_HELPER> -> &main_helper {
        # DEPRECATED message here

        # Make MAIN available at callframe(1) when executing main_helper
        # but return if there is nothing to call (old semantics)
        return $mainline unless my &MAIN := %caller-my<&MAIN>;

        # Call the MAIN_HELPER, it should do everything
        return &main_helper.count == 2
          ?? main_helper($in-as-argsfiles,$mainline)  # post 2018.06 interface
          !! main_helper($mainline)                   # original interface
    }

    # Convert raw command line args into positional and named args for MAIN
    sub default-args-to-capture($, @args is copy --> Capture:D) {
        my $no-named-after = nqp::isfalse(%sub-main-opts<named-anywhere>);

        my $positional := nqp::create(IterationBuffer);
        my %named;

        sub thevalue(\a) {
            ((my \type := ::(a)) andthen Metamodel::EnumHOW.ACCEPTS(type.HOW))
              ?? type
              !! val(a)
        }

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

    # Generate $*USAGE string (default usage info for MAIN)
    sub default-generate-usage(&, |capture) {
        my $no-named-after = nqp::isfalse(%sub-main-opts<named-anywhere>);

        my @help-msgs;
        my Pair @arg-help;

        my sub strip_path_prefix($name) {
            my $SPEC := $*SPEC;
            my ($vol, $dir, $base) = $SPEC.splitpath($name);
            $dir = $SPEC.canonpath($dir);
            for $SPEC.path() -> $elem {
                my $file = $SPEC.catpath($vol, $elem, $base).IO;
                if $file.x && $file.f {
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

        # return the Cool constant if the post_constraints of a Parameter is
        # a single Cool constant, else Nil
        sub cool_constant(Parameter:D $p) {
            nqp::not_i(
              nqp::isnull(
                (my \post_constraints :=
                  nqp::getattr($p,Parameter,'@!post_constraints'))
              )
            ) && nqp::elems(post_constraints) == 1
              && nqp::istype((my \value := nqp::atpos(post_constraints,0)),Cool)
              ?? value
              !! Nil
        }

        # Select candidates for which to create USAGE string
        sub usage-candidates($capture) {
            my @candidates = &main.candidates.grep: { !.?is-hidden-from-USAGE }
            if $capture.list -> @positionals {
                my $first := @positionals[0];
                if @candidates.grep: -> $sub {
                    if $sub.signature.params[0] -> $param {
                        if cool_constant($param) -> $literal {
                            $literal.ACCEPTS($first)
                        }
                    }
                } -> @candos {
                    return @candos;
                }
            }
            @candidates
        }

        for usage-candidates(capture) -> $sub {
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
                        my @names = $param.named_names.reverse;
                        $argument = @names.map({
                            (.chars == 1 ?? '-' !! '--') ~ $_
                        }).join('|');

                        my $type := $param.type;
                        if $type ~~ Positional {
                            $argument ~= "=<{ $constraints || "Any" }> ..."

                        }
                        elsif $type !=== Bool {
                            $argument ~= "=<{
                                $constraints || $type.^name
                            }>";
                            if Metamodel::EnumHOW.ACCEPTS($type.HOW) {
                                my $options = $type.^enum_values.keys.sort.Str;
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
              ?? join(' ', $prog-name, @required-named, @optional-named, @positional, ($docs if $docs))
              !! join(' ', $prog-name, @positional, @required-named, @optional-named, ($docs if $docs));
            @help-msgs.push($msg);
        }

        if @arg-help {
            @help-msgs.push('');
            my $offset = max(@arg-help.map: { .key.chars }) + 4;
            @help-msgs.append(@arg-help.map: { '  ' ~ .key ~ ' ' x ($offset - .key.chars) ~ .value });
        }

        @help-msgs
          ?? "Usage:\n" ~ @help-msgs.map('  ' ~ *).join("\n")
          !! "No usage information could be determined"
    }

    sub has-unexpected-named-arguments($signature, %named-arguments) {
        return False if $signature.params.first: *.capture;
        my @named-params = $signature.params.grep: *.named;
        return False if @named-params.first: *.slurpy;

        my %accepts-argument is Set = @named-params.map( *.named_names.Slip );
        return True unless %accepts-argument{$_} for %named-arguments.keys;
        False
    }

    sub find-candidates($capture) {
        &main
          # Get a list of candidates that match according to the dispatcher
          .cando($capture)
          # Sort out all that would fail due to binding
          .grep: { !has-unexpected-named-arguments(.signature, $capture.hash) }
    }

    # turn scalar values of nameds into 1 element arrays, return new capture
    sub scalars-into-arrays($capture) {
        my %hash = $capture.hash.map: {
            nqp::istype(.value,Positional) ?? $_ !! Pair.new(.key,[.value])
        }
        Capture.new( :list($capture.list), :%hash)
    }

    # set up other new style dynamic variables
    my &*ARGS-TO-CAPTURE := &default-args-to-capture;
    my &*GENERATE-USAGE  := &default-generate-usage;

    # Process command line arguments
    my $capture := args-to-capture(&main, @*ARGS);

    # Get a list of candidates that match according to the dispatcher
    my @candidates = find-candidates($capture);
    if !@candidates {
        my $alternate = scalars-into-arrays($capture);
        if find-candidates($alternate) -> @alternates {
            $capture   := $alternate;
            @candidates = @alternates;
        }
    }

    # If there are still some candidates left, try to dispatch to MAIN
    if @candidates {
        if $in-as-argsfiles {
            my $*ARGFILES := IO::ArgFiles.new: (my $in := $*IN),
                :nl-in($in.nl-in), :chomp($in.chomp), :encoding($in.encoding),
                :bin(nqp::hllbool(nqp::isfalse($in.encoding)));
            main(|$capture).sink;
        }
        else {
            main(|$capture).sink;
        }
    }
    # We could not find the correct MAIN to dispatch to!

    # No new-style GENERATE-USAGE was provided, and no new style
    # ARGS-TO-CAPTURE was provided either, so try to run a user defined
    # USAGE sub of the old interface.
    elsif !$provided-g-u && !$provided-a-to-c && %caller-my<&USAGE> -> &usage {
        # DEPRECATED message here
        usage;
    }

    # Display the default USAGE message on either STDOUT/STDERR
    elsif $capture<help> {
        $*OUT.say: generate-usage(&main,|$capture);
        exit 0;
    }
    else {
        $*ERR.say: generate-usage(&main,|$capture);
        exit 2;
    }
}

# vim: ft=perl6 expandtab sw=4
