use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;

class Perl6::Compiler is HLL::Compiler {
    method command_eval(*@args, *%options) {
        if nqp::existskey(%options, 'doc') && !%options<doc> {
            %options<doc> := 'Text';
        }

        my $hll_ns := pir::get_root_global__Ps('perl6');
        my $argiter := nqp::iterator(@args);
        nqp::shift($argiter) if $argiter && !pir::defined(%options<e>);
        $hll_ns<$!ARGITER> := $argiter;
        my $super := pir::find_method__PPs(HLL::Compiler, 'command_eval');
        my %*COMPILING;
        %*COMPILING<%?OPTIONS> := %options;
        $super(self, |@args, |%options);
    }

    method optimize($past, *%adverbs) {
        %adverbs<optimize> eq 'off' ??
            $past !!
            Perl6::Optimizer.new.optimize($past, |%adverbs)
    }

    method syntaxcheck($past, *%adverbs) {
        if %adverbs<c> {
            say("Syntax OK");
            nqp::exit(0);
        }
        $past;
    }

    method autoprint($value) {
        unless pir::getinterp__P().stdout_handle().tell() > $*AUTOPRINTPOS {
            CATCH { nqp::say($_) }
            if nqp::can($value, 'gist') {
                nqp::say(nqp::unbox_s($value.gist));
            } else {
                nqp::say(~$value);
            }
        }
    }
    
    method usage($name?) {
        say(($name ?? $name !! "") ~ " [switches] [--] [programfile] [arguments]
 
        With no arguments, enters a REPL. With a \"[programfile]\" or the
        \"-e\" option, compiles the given program and by default also
        executes the compiled code.
 
          -c                   check syntax only (runs BEGIN and CHECK blocks)
          --doc                extract documentation and print it as text
          -e program           one line of program
          -h, --help           display this help text
          -n                   run program once for each line of input
          -p                   same as -n, but also print \$_ at the end of lines
          --target=[stage]     specify compilation stage to emit
          --optimize=[level]   use the given level of optimization (0..3)
          -t, --trace=[flags]  enable trace flags, see 'parrot --help-debug'
          --encoding=[mode]    specify string encoding mode
          -o, --output=[name]  specify name of output file
          -v, --version        display version information
          --stagestats         display time spent in the compilation stages
          --ll-exception       display a low level backtrace on errors
          --profile            print profile information to standard error


        Note that only boolean single-letter options may be bundled.

        Output from --profile can be visualized by kcachegrind.


        To modify the include path, you can set the PERL6LIB environment variable:
        
        PERL6LIB=\"lib\" perl6 example.pl
        
        For more information, see the perl6(1) man page.\n"); 
        nqp::exit(0);
    }
}
