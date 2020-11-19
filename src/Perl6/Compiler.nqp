use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;

class Perl6::Compiler is HLL::Compiler {
    has $!language_version;  # Default language version in form 6.c
    has $!language_modifier; # Active language modifier; PREVIEW mostly.
    has $!language_revisions; # Hash of language revision letters. See gen/<vm>/main-version.nqp
    has $!can_language_versions; # List of valid language version
    has $!rakudo-home;

    method config() {
        nqp::gethllsym('default', 'SysConfig').rakudo-build-config();
    }

    method compilation-id() {
        my class IDHolder { }
        BEGIN { (IDHolder.WHO)<$ID> := $*W.handle }
        $IDHolder::ID
    }

    method version() {
        nqp::say(self.version_string);
        nqp::exit(0);
    }

    method version_string(:$shorten-versions) {
        my $config-version  := self.config()<version>;
        my $backend-version := nqp::getattr(self,HLL::Compiler,'$!backend').version_string;

        my $raku;
        my $rakudo;
        if $shorten-versions {
            my $index := nqp::index($config-version,"-");
            $config-version := nqp::substr($config-version,0,$index)
              unless $index == -1;

            $index := nqp::index($backend-version,"-");
            $backend-version := nqp::substr($backend-version,0,$index)
              unless $index == -1;

            $raku   := "𝐑𝐚𝐤𝐮™";
            $rakudo := "𝐑𝐚𝐤𝐮𝐝𝐨™";
        }
        else {
            $raku   := "Raku(tm)";
            $rakudo := "Rakudo(tm)";
        }

        "Welcome to "
          ~ $rakudo
          ~ " v"
          ~ $config-version
          ~ ".\nImplementing the "
          ~ $raku
          ~ " programming language v"
          ~ self.language_version()
          ~ ".\nBuilt on "
          ~ $backend-version
          ~ "."
    }


    method implementation()   { self.config<implementation> }
    method language_name()    { 'Raku' }
    method reset_language_version() {
        $!language_version := NQPMu;
        $!language_modifier := NQPMu;
    }
    method set_language_version($version) {
        $!language_version := $version;
    }
    method set_language_modifier($modifier) {
        $!language_modifier := $modifier;
    }
    method language_version() {
        if nqp::defined($!language_version) {
            $!language_version
        }
        else {
            $!language_version := %*COMPILING<%?OPTIONS><language_version> || self.config<language-version>
        }
    }
    method language_revision() {
        nqp::substr(self.language_version,2,1)
    }
    method language_modifier() {
        $!language_modifier
    }
    method    can_language_versions() {
            $!can_language_versions
        ??  $!can_language_versions
        !! ($!can_language_versions := self.config<can-language-versions>)
    }
    method language_revisions() {
           $!language_revisions
        ?? $!language_revisions
        !! ($!language_revisions := self.config<language-revisions>)
    }

    method command_eval(*@args, *%options) {
        if nqp::existskey(%options, 'doc') && !%options<doc> {
            %options<doc> := 'Text';
        }

        if nqp::existskey(%options, 'nqp-lib') {
            note('Option `--nqp-lib` is deprecated, has no effect and will be removed in 2021.06.');
        }

        my $argiter := nqp::iterator(@args);
        nqp::shift($argiter) if $argiter && !nqp::defined(%options<e>);
        nqp::bindhllsym('Raku', '$!ARGITER', $argiter);
        my $super := nqp::findmethod(HLL::Compiler, 'command_eval');
        my %*COMPILING := nqp::clone(nqp::ifnull(nqp::getlexdyn('%*COMPILING'), nqp::hash()));
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

    method verbose-config() {
        self.eval('Compiler.verbose-config(:say)');
        nqp::exit(0);
    }

    method interactive(*%adverbs) {
        my $p6repl;

        my $repl-class := self.eval('REPL', :outer_ctx(nqp::null()), |%adverbs);
        $p6repl := $repl-class.new(self, %adverbs);
        my $stdin    := stdin();

        $p6repl.repl-loop(:interactive(1), |%adverbs)
    }

    method usage($name?, :$use-stderr = False) {
	my $print-func := $use-stderr ?? &note !! &say;
    my $compiler := nqp::getcomp("Raku").backend.name;
    my $moar-options := '';
    if nqp::getcomp("Raku").backend.name eq 'moar' {
        $moar-options := q♥  --profile[=name]     write profile information to a file
                       Extension controls format:
                           .json outputs in JSON
                           .sql  outputs in SQL
                           any other extension outputs in HTML
  --profile-compile[=name]
                       write compile-time profile information to a file
                       Extension controls format:
                         .json outputs in JSON
                         .sql  outputs in SQL
                         any other extension outputs in HTML
  --profile-kind[=name]
                       choose the type of profile to generate
                         instrumented - performance measurements (default)
                         heap - record heap snapshots after every garbage
                         collector run
  --profile-filename=name
                       provide a different filename for profile.
                       Extension controls format:
                         .json outputs in JSON
                         .sql  outputs in SQL
                         any other extension outputs in HTML
                       This option will go away in a future Rakudo release
  --profile-stage=stage
                       write profile information for the given compilation
                       stage to a file. Use --profile-compile to set name
                       and format
  --full-cleanup       try to free all memory and exit cleanly
  --debug-port=port    listen for incoming debugger connections
  --debug-suspend      pause execution at the entry point
  --tracing            output a line to stderr on every interpreter instr (only if
                       enabled in MoarVM)
♥;
    }
    $print-func(($name ?? $name !! "") ~ qq♥ [switches] [--] [programfile] [arguments]

With no arguments, enters a REPL (see --repl-mode option).
With a "[programfile]" or the "-e" option, compiles the given program
and, by default, also executes the compiled code.

  -                    read program source from STDIN or start REPL if a TTY
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
  --rakudo-home=path   Override the path of the Rakudo runtime files
  -o, --output=name    specify name of output file
  -v, --version        display version information
  -V                   print configuration summary
  --stagestats         display time spent in the compilation stages
  --ll-exception       display a low level backtrace on errors
  --doc=module         use Pod::To::[module] to render inline documentation
  --repl-mode=interactive|non-interactive
                       when running without "-e" or filename arguments,
                       a REPL is started. By default, if STDIN is a TTY,
                       "interactive" REPL is started that shows extra messages and
                       prompts, otherwise a "non-interactive" mode is used where
                       STDIN is read entirely and evaluated as if it were a program,
                       without any extra output (in fact, no REPL machinery is even
                       loaded). This option allows to bypass TTY detection and
                       force one of the REPL modes.
$moar-options
Note that only boolean single-letter options may be bundled.

The following environment variables are respected:

  RAKULIB     Modify the module search path
  PERL6LIB    Modify the module search path # to be deprecated
  RAKUDO_HOME Override the path of the Rakudo runtime files
  NQP_HOME    Override the path of the NQP runtime files

♥); # end of usage statement

        nqp::exit(0);

        # TODO: create and install a man page for Raku; then add the following
        #       line to the end of the usage text above:
        #
        #  For more information, see the raku(1) man page.\n");
    }
}

# vim: expandtab sw=4
