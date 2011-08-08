my sub MAIN_HELPER($retval = 0) {
    my $m = callframe(1).my<&MAIN>;
    return $retval unless $m;

    # We found MAIN, let's process the command line arguments accordingly
    my sub process-cmd-args (@args is copy) {
        my (@positional-arguments, %named-arguments);
        while (@args) {
            my $passed_value = @args.shift;
            if $passed_value.substr(0, 1) eq '-'
            && $passed_value.substr(1, 1) ne '-' {
                # TODO: warn?
                @positional-arguments.push: $passed_value;
            }

            my $negate = False;
            if $passed_value.substr(0, 2) eq '--' {
                my $arg = $passed_value.substr(2);
                if $arg.substr(0, 1) eq '/' {
                    $arg .= substr(1) ;
                    $negate = True;
                }

                if $arg eq '' {
                    @positional-arguments.push: @args;
                    last;
                } elsif $arg.index('=').defined  {
                    my ($name , $value) = $arg.split('=', 2);
                    if $negate {
                        note "Trouble while parsing comand line argument '$arg': Cannot negate something which has an explicit value - ignoring the argument.\n";
                        next;
                    }
                    %named-arguments.push: $name => $value;
                } else {
                    %named-arguments.push: $arg => !$negate;
                }
            } else {
                @positional-arguments.push: $passed_value;
            }
        }

        return @positional-arguments, %named-arguments
    }

    my ($p, $n) = process-cmd-args(@*ARGS);

    # TODO: check if a dispatch is possible, and if not,
    # do some USAGE magic.
    return $m(|@($p), |%($n));

#    # We could not find the correct main to dispatch to!
#    # Let's try to run the user defined USAGE sub
#    my $h = callframe(1).my<&USAGE>;
#    return $h() if $h;
#
#    # We could not find a user defined USAGE sub!
#    # Let's display a default USAGE message
#    my @aliases;
#    my @help-msgs;
#    for $m.candidates -> $sub {
#        my $sig = $sub.signature;
#        my @arguments = ();
#        for $sig.params -> $param {
#            my $argument;
#            if $param.named {
#                my $param-name = $param.name.substr(1); # Remove $
#                my %alias = $param.named_names.elems == 2
#                            ?? get-aliases($param.named_names.hash)
#                            !! ();
#
#                my $long-name = %alias{$param-name} // $param-name;
#                $argument = "--$long-name"
#                            ~ ($param.type ~~ Bool
#                               ?? '' !! "=value-of-$long-name");
#
#                if %alias {
#                    @aliases.push: "  -" ~ %alias.pairs[0].key()
#                                   ~ " instead of --$long-name"
#                }
#            } else {
#                $argument = $param.name ?? $param.name.substr(1)
#                                        !! ~$param.constraints;
#                #TODO: fixme
#                $argument = 'param' if $argument ~~ /^_block\d+$/;
#
#                if $param.slurpy {
#                    $argument ~= " [more [...]]";
#                }
#            }
#            $argument = "[$argument]" if $param.optional;
#            if $param.named {
#                @arguments.unshift($argument);
#            } else {
#                @arguments.push($argument);
#            }
#        }
#        @help-msgs.push(
#            ($*PROGRAM_NAME eq '-e' ?? "-e '...'" !! $*PROGRAM_NAME)
#            ~ ' ' ~ @arguments.join(' ');
#        );
#    }
#    my $msg = "Usage:\n" ~ @help-msgs.join("\nor\n");
#    if @aliases {
#        $msg ~= "\nYou can use\n" ~ @aliases.join("\n")
#    }
#    if @*ARGS ~~ ['--help'] {
#        $*OUT.say($msg);
#    } else {
#        $*ERR.say($msg);
#        exit 29; #TODO: Better return value
#    }
}
