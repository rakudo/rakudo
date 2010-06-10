our sub MAIN_HELPER() {
    my $m = Q:PIR {
        $P0 = getinterp
        $P0 = $P0['lexpad';1]
        $P0 = $P0['&MAIN']
        unless null $P0 goto has_main
        %r  = get_hll_global "Any"
        goto done
    has_main:
        %r  = $P0
    done:
    };
    unless $m {
        return;
    }
    
    # We found MAIN, let's process the command line arguments accordingly
    my $process-cmd-args =  sub (@args is copy, %named) {
        my (@positional-arguments, %named-arguments , $negate);
        while ( @args )  {
            my $passed_value = @args.shift;
            if substr($passed_value,0,2) eq '--' {
                my $arg = $passed_value.substr(2);
                if $arg.match(/^\//) {
                    $arg .= substr(1) ;
                    $negate = $arg;
                }

                if $arg eq '' {
                    @positional-arguments.push: @args;
                    last;
                } elsif %named{$arg} ~~ Bool {
                    %named-arguments{$arg}=True;
                } elsif %named{$arg} ~~ Array || ($passed_value.match( /\=/ ) &&  %named{$arg.split('=', 2)[0]} ~~ Array ) {
                    if $passed_value.match( /\=/ ) {
                        my ($name , $value) = $arg.split('=', 2);
                        if $negate {$negate=$name;}
                        %named-arguments{$name} = [$value.split(',')];
                    } else {
                        %named-arguments{$arg} = [@args.shift.split(',')];
                    }
                } elsif $passed_value.match( /\=/ ) {
                    my ($name , $value) = $arg.split('=', 2);
                    if $negate {$negate=$name;}
                    if ($value.match(/^\'.*\'$/) || $value.match(/^\".*\"$/) ) {
                        %named-arguments{$name} = $value.substr(1,-1);
                    } elsif $value.match( /.\,./ ) { #--separator=, should not be an array by default but --values=1,2,3 should be
                        %named-arguments{$name} = [$value.split(',')];
                    } else {
                        %named-arguments{$name} = $value;
                    }
                } elsif $negate {
                    %named-arguments{$arg} = False;
                    $negate='';
                } else {
                    %named-arguments{$arg}=@args.shift;
                }
            } else {
                @positional-arguments.push: $passed_value;
            }

            if $negate {
                %named-arguments{$negate} does False;
                $negate = '';
            }
        }

        return @positional-arguments, %named-arguments;
    };
    
    my @subs = $m ~~ Multi  ?? $m.candidates !! ($m);
    #TODO: We are calling the FIRST matching MAIN sub, we should be calling the BEST matching MAIN sub.
    for @subs -> $main {
        my @named-params = $main.signature.params.grep: {.named && .type ~~ Bool};
        my %named-params = @named-params».name».substr(1) Z=> @named-params».type;
        my @positional = $process-cmd-args(@*ARGS, %named-params);
        my %named = @positional.pop;
        if Capture.new(|@positional, |%named) ~~ $main.signature { 
            $main(|@positional, |%named);
            return ;
        }
    }
    
    #We could not find the correct main to dispatch to! Let's try to run the user defined USAGE sub
    my $h = Q:PIR {
        $P0 = getinterp
        $P0 = $P0['lexpad';1]
        $P0 = $P0['&USAGE']
        unless null $P0 goto has_usage
        %r  = get_hll_global "Any"
        goto done_usage
    has_usage:
        %r  = $P0
    done_usage:
    };
    return $h() if $h ;
    
    #We could not find a user defined USAGE sub! Let's display a default USAGE message
    my @help-msgs;
    my @mains = $m ~~ Multi  ?? $m.candidates !! ($m);
    for @mains -> $sub {
        my $sig = $sub.signature;
        my @arguments , @help-msgs;
        for $sig.params -> $param {
            my $argument;
            if ($param.named) {
                $argument = "--"
                        ~ $param.name.substr(1)
                        ~ ($param.type ~~ Bool ?? '' !! "=value-of-{$param.name.substr(1)}")
                        ;
            } else {
                $argument = ($param.name ?? $param.name.substr(1) !! ~$param.constraints  );
                $argument = 'param' if $argument.match(/^_block\d+$/) ; #TODO: fixme
                if ($param.slurpy) {
                    $argument ~= " [more [...]]";
                }
            }
            $argument = "[$argument]" if $param.optional;
            if ($param.named) {
                  @arguments.unshift($argument);
            } else {
                  @arguments.push($argument);
            }
        }
        my $msg = ($*PROGRAM_NAME eq '-e' ?? "-e '...'" !! $*PROGRAM_NAME )~ ' '  ~ @arguments.join(' ');
        @help-msgs.push( $msg );
    }
    ("Usage:\n" ~ @help-msgs.join("\nor\n") ).say;
    return 0; #TODO: Better return value
}
