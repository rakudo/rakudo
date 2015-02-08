use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;

class Perl6::Compiler is HLL::Compiler {
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
          -e program           one line of program
          -h, --help           display this help text
          -n                   run program once for each line of input
          -p                   same as -n, but also print \$_ at the end of lines
          -I path              adds the path to the module search path
          -M module            loads the module prior to running the program
          --target=[stage]     specify compilation stage to emit
          --optimize=[level]   use the given level of optimization (0..3)
          -t, --trace=[flags]  enable trace flags, see 'parrot --help-debug'
          --encoding=[mode]    specify string encoding mode
          -o, --output=[name]  specify name of output file
          -v, --version        display version information
          --stagestats         display time spent in the compilation stages
          --ll-exception       display a low level backtrace on errors
          --profile            print profile information to standard error (Parrot)
                               write profile information as HTML file (MoarVM)
          --doc=[module]       Use Pod::To::[module] to render inline documentation.


        Note that only boolean single-letter options may be bundled.

        Output from --profile can be visualized by kcachegrind for the Parrot backend.


        To modify the include path, you can set the PERL6LIB environment variable:
        
        PERL6LIB=\"lib\" perl6 example.pl
        
        For more information, see the perl6(1) man page.\n"); 
        nqp::exit(0);
    }
}
