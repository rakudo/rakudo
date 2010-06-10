our sub USAGE ($sub=&MAIN) { #The user can still overwrite this in his program to get customized USAGE message
    my @subs = $sub ~~ Multi  ?? $sub.candidates !! ($sub);
    
    my $USAGE-one-sub = sub ($sub=&MAIN) {
        my $sig = $sub.signature;
        my @arguments;
        for $sig.params -> $param {
            my $argument;
            if ($param.named) {
                $argument = "--"
                        ~ $param.name.substr(1)
                        ~ ($param.type ~~ Bool ?? '' !! "=value-of-{$param.name.substr(1)}")
                        ;
            } else {
                $argument = $param.name.substr(1);
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
        return  ($*PROGRAM_NAME eq '-e' ?? "-e '...'" !! $*PROGRAM_NAME )~ ' '  ~ @arguments.join(' ');
    };
    
    my @help-msgs = @subs.map( { $USAGE-one-sub($_) } ) ;
    return  "Usage\n" ~ @help-msgs.join("\nor\n");
}

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
    
    my $correct-main-found = False;
    my @subs = $m ~~ Multi  ?? $m.candidates !! ($m);
    for @subs -> $main {
        my @named-params = $main.signature.params.grep: {.named && .type ~~ Bool};
        my %named-params = @named-params».name».substr(1) Z=> @named-params».type;
        my @positional = $process-cmd-args(@*ARGS, %named-params);
        my %named = @positional.pop;
        try { 
            $main(|@positional, |%named);
            $correct-main-found = True;
        }
        return if $correct-main-found;
    }
    my $help = USAGE($m);
    $help.say;
}
