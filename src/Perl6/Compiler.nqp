use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;

class Perl6::Compiler is HLL::Compiler {
    method compilation-id() {
        my class IDHolder { }
        BEGIN { (IDHolder.WHO)<$ID> := $*W.handle }
        $IDHolder::ID
    }

    method implementation()   { self.config<implementation> }
    method language_name()    { 'Perl' }
    method language_version() { self.config<language_version> }

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

    method interactive(*%adverbs) {
        nqp::say("To exit type 'exit' or '^D'");
        my $p6repl;

        try {
            my $repl-class := self.eval('REPL', :outer_ctx(nqp::null()), |%adverbs);
            $p6repl := $repl-class.new(self, %adverbs);

            CATCH {
                nqp::die("couldn't load REPL.pm: $_");
            }
        }

        my $stdin    := nqp::getstdin();
        my $encoding := ~%adverbs<encoding>;
        if $encoding && $encoding ne 'fixed_8' {
            nqp::setencoding($stdin, $encoding);
        }

        my $result := $p6repl.repl-loop(:interactive(1), |%adverbs);
        $result
    }

    method usage($name?) {
        say(($name ?? $name !! "") ~ " [switches] [--] [programfile] [arguments]

    With no arguments, enters a REPL. With a \"[programfile]\" or the \"-e\"
    option, compiles the given program and, by default, also executes the
    compiled code.

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
}
