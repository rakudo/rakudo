use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;

class Perl6::Compiler is HLL::Compiler {
    has $!language_version;

    method compilation-id() {
        my class IDHolder { }
        BEGIN { (IDHolder.WHO)<$ID> := $*W.handle }
        $IDHolder::ID
    }

    method implementation()   { self.config<implementation> }
    method language_name()    { 'Perl' }
    method reset_language_version() {
        $!language_version := NQPMu;
    }
    method set_language_version($version) {
        $!language_version := $version;
    }
    method language_version() {
        if nqp::defined($!language_version) {
            $!language_version
        }
        else {
            $!language_version := self.config<language_version>
        }
    }

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

        # Apply world clean-up tasks, we will not trigger any more dynamic
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
        my $p6repl;

        my $repl-class := self.eval('REPL', :outer_ctx(nqp::null()), |%adverbs);
        $p6repl := $repl-class.new(self, %adverbs);
        my $stdin    := nqp::getstdin();
        my $encoding := ~%adverbs<encoding>;
        if $encoding && $encoding ne 'fixed_8' {
            nqp::setencoding($stdin, $encoding);
        }

        $p6repl.repl-loop(:interactive(1), |%adverbs)
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
  --target=stage       specify compilation stage to emit
  --optimize=level     use the given level of optimization (0..3)
  --encoding=mode      specify string encoding mode
  -o, --output=name    specify name of output file
  -v, --version        display version information
  --stagestats         display time spent in the compilation stages
  --ll-exception       display a low level backtrace on errors
  --profile[=kind]     write profile information to an HTML file (MoarVM)
                         instrumented - performance measurements (default)
                         heap - record heap snapshots after every
                                garbage collector run
  --profile-filename=name
                       provide a different filename for profile. Extension
                       controls format:
                            .json outputs in JSON
                            .sql  outputs in SQL
                            any other extension outputs in HTML
  --doc=module         use Pod::To::[module] to render inline documentation


Note that only boolean single-letter options may be bundled.

To modify the include path, you can set the PERL6LIB environment variable:

PERL6LIB=\"lib\" perl6 example.pl
"); # end of usage statement

        nqp::exit(0);

        # TODO: create and install a man page for Perl 6; then add the following
        #       line to the end of the usage text above:
        #
        #  For more information, see the perl6(1) man page.\n");
    }
}
