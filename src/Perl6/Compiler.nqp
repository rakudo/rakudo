use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;

my sub sorted_set_insert(@values, $value) {
    my $low        := 0;
    my $high       := nqp::elems(@values) - 1;
    my $insert_pos := 0;

    while $low <= $high {
        my $middle := nqp::floor_n($low + ($high - $low) / 2);

        my $middle_elem := nqp::atpos(@values, $middle);

        if $middle == nqp::elems(@values) - 1 {
            if $value eq $middle_elem {
                return;
            } elsif $value lt $middle_elem {
                $high := $middle - 1;
            } else {
                $insert_pos := nqp::elems(@values);
                last;
            }
        } else {
            my $middle_plus_one_elem := nqp::atpos(@values, $middle + 1);

            if $value eq $middle_elem || $value eq $middle_plus_one_elem {
                return;
            } elsif $value lt $middle_elem {
                $high := $middle - 1;
            } elsif $value gt $middle_plus_one_elem {
                $low := $middle + 1;
            } else {
                $insert_pos := $middle + 1;
                last;
            }
        }
    }

    nqp::splice(@values, nqp::list($value), $insert_pos, 0);
}

class Perl6::Compiler is HLL::Compiler {
    has $!linenoise;
    has $!linenoise_add_history;
    has $!completions;

    method command_eval(*@args, *%options) {
        if nqp::existskey(%options, 'doc') && !%options<doc> {
            %options<doc> := 'Text';
        }

        my $argiter := nqp::iterator(@args);
        nqp::shift($argiter) if $argiter && !nqp::defined(%options<e>);
        nqp::bindhllsym('perl6', '$!ARGITER', $argiter);
        my $super := nqp::findmethod(HLL::Compiler, 'command_eval');
        my %*COMPILING;
        %*COMPILING<%?OPTIONS> := %options;
        $super(self, |@args, |%options);
    }

    method optimize($past, *%adverbs) {
        # Apply optimizations.
        my $result := %adverbs<optimize> eq 'off'
            ?? $past
            !! Perl6::Optimizer.new.optimize($past, |%adverbs);

        # Apply world clean-up tasks, we we will not trigger any more dynamic
        # compilation beyond this point.
        $past.ann('W').cleanup();

        $result;
    }

    method syntaxcheck($past, *%adverbs) {
        if %adverbs<c> {
            say("Syntax OK");
            nqp::exit(0);
        }
        $past;
    }
    
    method interactive_result($value) {
        CATCH { nqp::say($_) }
        if nqp::can($value, 'gist') {
            nqp::say(nqp::unbox_s($value.gist));
        } else {
            nqp::say(~$value);
        }
    }

    method interactive(*%adverbs) {
        try {
            my @symbols := self.eval("use nqp; use Linenoise; nqp::list(&linenoise, &linenoiseHistoryAdd, &linenoiseSetCompletionCallback, &linenoiseAddCompletion)", :outer_ctx(nqp::null()));

            $!linenoise := @symbols[0];
            $!linenoise_add_history := @symbols[1];
            my $linenoise_set_completion_callback := @symbols[2];
            my $linenoise_add_completion := @symbols[3];

            $!completions := nqp::list();

            my $core_keys := self.eval('CORE::.keys', :outer_ctx(nqp::null()));

            my int $i := 0;
            my $core_elems := $core_keys.elems();

            while $i < $core_elems {
                my $e := $core_keys.AT-POS($i);
                $i := $i + 1;

                my $m := $e ~~ /^ "&"? $<word>=[\w+] $/;
                if $m {
                    my $word := $m<word>;
                    unless $word ~~ /^ "&" <.upper>+ $/ {
                        sorted_set_insert($!completions, $word);
                    }
                }
            }

            $linenoise_set_completion_callback(sub ($line, $c) {
                my $m := $line ~~ /^ $<prefix>=[.*?] <|w>$<last_word>=[\w*]$/;

                my $prefix    := $m ?? ~$m<prefix>    !! '';
                my $last_word := $m ?? ~$m<last_word> !! '';

                my $it := nqp::iterator($!completions);

                while $it {
                    my $k := nqp::shift($it);

                    if $k ~~ /^ $last_word / {
                        $linenoise_add_completion($c, $prefix ~ $k);
                    }
                }
            });

            CATCH {} # it's ok if we can't load Linenoise
        }

        my $*moreinput := sub ($cursor) {
            my str $more := self.readline(nqp::getstdin(), nqp::getstdout(), '* ');
            if nqp::isnull_s($more) || $more eq '' {
                $more := ';';
            }

            $cursor."!APPEND_TO_ORIG"($more);
        }

        my $super := nqp::findmethod(HLL::Compiler, 'interactive');
        $super(self, :interactive(1), |%adverbs);
    }
    
    method interactive_exception($ex) {
        my $payload := nqp::getpayload($ex);
        if nqp::can($payload, 'gist') {
            nqp::say(nqp::unbox_s($payload.gist));
        }
        else {
            nqp::say(~$ex)
        }
        CATCH { nqp::say(~$ex) }
    }
    
    method usage($name?) {
        say(($name ?? $name !! "") ~ " [switches] [--] [programfile] [arguments]
 
        With no arguments, enters a REPL. With a \"[programfile]\" or the
        \"-e\" option, compiles the given program and by default also
        executes the compiled code.
 
          -c                   check syntax only (runs BEGIN and CHECK blocks)
          --doc                extract documentation and print it as text
          -e program           one line of program, strict is enabled by default
          -h, --help           display this help text
          -n                   run program once for each line of input
          -p                   same as -n, but also print \$_ at the end of lines
          -I path              adds the path to the module search path
          -M module            loads the module prior to running the program
          --target=[stage]     specify compilation stage to emit
          --optimize=[level]   use the given level of optimization (0..3)
          --encoding=[mode]    specify string encoding mode
          -o, --output=[name]  specify name of output file
          -v, --version        display version information
          --stagestats         display time spent in the compilation stages
          --ll-exception       display a low level backtrace on errors
          --profile            write profile information as HTML file (MoarVM)
          --profile-filename   provide a different filename (also allows .json)
          --doc=[module]       Use Pod::To::[module] to render inline documentation.


        Note that only boolean single-letter options may be bundled.

        To modify the include path, you can set the PERL6LIB environment variable:
        
        PERL6LIB=\"lib\" perl6 example.pl
        
        For more information, see the perl6(1) man page.\n"); 
        nqp::exit(0);
    }

    method readline($stdin, $stdout, $prompt) {
        if $!linenoise {
            my $ctx := self.context();
            if $ctx {
                my $pad := nqp::ctxlexpad($ctx);
                my $it := nqp::iterator($pad);

                while $it {
                    my $e := nqp::shift($it);
                    my $k := nqp::iterkey_s($e);
                    my $m := $k ~~ /^ "&"? $<word>=[\w+] $/;
                    if $m {
                        my $word := $m<word>;
                        unless $word ~~ /^ "&" <.upper>+ $/ {
                            sorted_set_insert($!completions, $word);
                        }
                    }
                }

                my $our := nqp::getlexrel($ctx, '$?PACKAGE').WHO;
                my $EnumMap := self.eval('EnumMap', :outer_ctx(nqp::null()));
                my $storage := nqp::getattr($our, $EnumMap, '$!storage');

                $it := nqp::iterator($storage);

                while $it {
                    my $e := nqp::shift($it);
                    my $k := nqp::iterkey_s($e);
                    sorted_set_insert($!completions, $k);
                }
            }

            my $line := $!linenoise($prompt);
            if $line.defined {
                $!linenoise_add_history($line);
                $line
            } else {
                nqp::null_s()
            }
        } else {
            my $super := nqp::findmethod(HLL::Compiler, 'readline');
            $super(self, $stdin, $stdout, $prompt);
        }
    }

}
