use NQPP6QRegex;
use QRegex;
use Perl6::Optimizer;
use RakuAST::Optimizer;

class Perl6::Compiler is HLL::Compiler {
    has @!language_version;  # Default language revision, 1 stands for v6.c
    has $!language_revisions; # Hash of language revision properties. See gen/<vm>/main-version.nqp
    has $!can_language_versions; # List of valid language versions
    has $!rakudo-home;

    class LanguageVersionServices {
        method p6rev(int $internal-revision) {
            $internal-revision < 1
                ?? nqp::die("Internal revision " ~ $internal-revision ~ " cannot be converted into Perl6 representation")
                !! nqp::chr(98 + $internal-revision)
        }

        method internal-from-p6(str $p6rev) {
            nqp::chars($p6rev) > 1
                ?? nqp::die("Perl6 revision can only be a single letter, got '$p6rev'")
                !! (my $irev := nqp::ord($p6rev) - 98) < 1
                    ?? nqp::die("'$p6rev' cannot be a Perl6 revision")
                    !! $irev
        }

        my sub as(@parts, :$plus, :$as-str, :$as-version) {
            if $as-str || $as-version {
                my $i := -1;
                while ++$i < +@parts {
                    @parts[$i] := ~@parts[$i];
                }

                my $vstr := join(".", @parts) ~ $plus;

                return $vstr unless $as-version;

                my $Version := nqp::gethllsym('Raku', 'Version');
                if nqp::isnull($Version) {
                    nqp::die("Symbol 'Version' is not available at this time; is BOOTSTRAP loaded?")
                }

                # This can be micro-optimized by using nqp::create + nqp::bindattr, but does it make any sense?
#?if jvm
                # Version.new tries to coerce argument to Str, and that doesn't work with BOOTStr.
                return $Version.new(nqp::hllizefor($vstr,'Raku'));
#?endif
#?if !jvm
                return $Version.new($vstr);
#?endif
            }
            @parts
        }

        # Get language version representation, guess its format, and return it as internal. with :as-version
        # will try to get Version class from HLL symbols and return an instance of it.
        method from-public-repr(str $repr, :$as-str, :$as-version) {
            my $config := nqp::gethllsym('default', 'SysConfig');
            my @parts;
            my $plus := '';

            if !$repr || $repr eq 'v6' {
                # Default revision implied
                @parts.push: nqp::gethllsym('default', 'SysConfig').rakudo-build-config<language-revision>;
            }
            else {
                my $m := $repr ~~ /^ v? $<vstr>=[ [ \w+ | '*' ]+ % '.' ] $<plus>='+'? $/;
                @parts := nqp::split('.', $m<vstr>);
                $plus := ~$m<plus>;
                if $repr ~~ /^ v? 6 [\D | $]/ {
                    # Perl6 representation. Skip the starting '6' first
                    if @parts[0] eq '6' {
                        # Since we excluded the 'v6' variant then @parts would never end up being empty here
                        @parts.shift;
                    }
                    else {
                        @parts[0] := nqp::substr(@parts[0], 1);
                    }

                    if @parts[0] ne '*' {
                        @parts[0] := self.internal-from-p6(@parts[0]);
                    }
                }
                else {
                    nqp::die("Requested language version '$repr' is not valid");
                }
            }

            as(@parts, :$plus, :$as-str, :$as-version)
        }

        method as-perl6($internal, :$as-str, :$as-version) {
            my $Version := nqp::gethllsym('Raku', 'Version');
            my $primspec := nqp::objprimspec($internal);
            my $is-list := nqp::islist($internal);
            my $is-version := !nqp::isnull($Version) && nqp::istype($internal, $Version);
            my @parts;
            if $primspec == nqp::const::BIND_VAL_STR || $is-list || $is-version {
                # A string
                @parts := $is-version
                    ?? nqp::clone(nqp::getattr($internal, $Version, '$!parts'))
                    !! $is-list
                        ?? nqp::clone($internal)
                        !! nqp::split(".", $internal);
                my $rev := @parts[0];
                if nqp::objprimspec($rev) == nqp::const::BIND_VAL_STR
                  && $rev ~~ /^ v? $<vnum>=\d+ $<plus>='+'? $/ -> $m {
                    # Turn internal revision number info a Perl6-revision char
                    @parts[0] := self.p6rev(+$m<vnum>) ~ $m<plus>;
                }
                else {
                    @parts[0] := self.p6rev($rev);
                }
            }
            elsif $primspec == nqp::const::BIND_VAL_INT
              || $primspec == nqp::const::BIND_VAL_UINT {
                @parts.push: self.p6rev($internal);
            }
            else {
                nqp::die("Don't know how to create Perl6-style language version from " ~ $internal.HOW.name($internal));
            }

            @parts.unshift('6');

            as(@parts, :$as-str, :$as-version)
        }

        method as-public-repr($internal, :$as-str, :$as-version) {
            # When v6.x is replaced with another representation this method will change too.
            self.as-perl6($internal, :$as-str, :$as-version);
        }
    }

    method config() {
        nqp::gethllsym('default', 'SysConfig').rakudo-build-config();
    }

    method version() {
        nqp::say(self.version_string);
        nqp::exit(0);
    }

    method lvs() { LanguageVersionServices }

    method version_string(:$shorten-versions, :$no-unicode) {
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
        }

        if $no-unicode {
            $raku   := "Raku(R)";
            $rakudo := "Rakudo(tm)";
        }
        else {
            $raku   := "Raku®";
            $rakudo := "Rakudo™";
        }
        my $flavor := #RAKUDO_FLAVOR#;
        $flavor := " $flavor" if $flavor;

        "Welcome to "
          ~ $rakudo
          ~ $flavor
          ~ " v"
          ~ $config-version
          ~ ".\nImplementing the "
          ~ $raku
          ~ " Programming Language v"
          ~ self.language_version()
          ~ ".\nBuilt on "
          ~ $backend-version
          ~ "."
    }


    method implementation()   { self.config<implementation> }
    method language_name()    { 'Raku' }
    method reset_language_version() {
        @!language_version := [];
    }
    method set_language_version($version) {
        @!language_version := nqp::islist($version)
            ?? $version
            !! self.lvs.from-public-repr($version);
    }
    method set_language_revision(int $rev) {
        @!language_version := [$rev];
    }
    method language_version_parts() {
        unless nqp::defined(@!language_version) && @!language_version {
            @!language_version := self.lvs.from-public-repr(%*COMPILING<%?OPTIONS><language_version> // '');
        }
        @!language_version
    }
    method language_version() {
        self.set_language_version('') unless @!language_version;
        LanguageVersionServices.as-public-repr: @!language_version, :as-str
    }
    method language_revision() {
        self.language_version_parts[0]
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

    method ast($match, *%adverbs) {
        my $ast := $match.ast;
        self.panic("Unable to obtain AST from parse result")
            unless nqp::isconcrete($ast);
        $ast
    }

    method qast($rakuast, *%adverbs) {
        # Apply RakuAST optimizations.
        if %adverbs<optimize> ne 'off' && nqp::existskey(%*ENV, 'RAKUDO_RAKUAST') {
            # Create optimizer instance and apply optimizations
            $rakuast := RakuAST::Optimizer.new.optimize($rakuast, |%adverbs);
        }
        
        # Convert to QAST compilation unit
        my $comp_unit := $rakuast.IMPL-TO-QAST-COMP-UNIT;
        $rakuast.cleanup();
        $comp_unit;
    }

    method verbose-config() {
        self.eval('Compiler.verbose-config(:say)');
        nqp::exit(0);
    }

    method interactive(*@args, *%adverbs) {
        my $p6repl;

        nqp::bindhllsym('Raku', '$!ARGITER', nqp::iterator(@args));

        my $repl-class := self.eval('REPL', :outer_ctx(nqp::null()), |%adverbs);
        $p6repl := $repl-class.new(self, %adverbs);
        $p6repl.repl-loop(:interactive(1), |%adverbs);
    }

    method usage($name?, :$use-stderr = False) {
	my $print-func := $use-stderr ?? &note !! &say;
    $print-func(($name ?? $name !! "") ~ qq♥ [switches] [--] [programfile] [arguments]

With no arguments, enters a REPL (see --repl-mode option).
With a "[programfile]" or the "-e" option, compiles the given program
and, by default, also executes the compiled code.

  -                    read program source from STDIN or start REPL if a TTY
  -c                   check syntax only (runs BEGIN and CHECK blocks)
  --rakudoc            extract documentation and print it as text
  --rakudoc=module     use RakuDoc::To::[module] to render inline documentation
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
  --doc                extract documentation and print it as text
  --doc=module         use Pod::To::[module] to render inline documentation
  --repl-mode=tty|process|disabled
                       when running without "-e", a REPL is started.
                       In this scenario, the repl-mode is automatically set
                       to 'tty'. A user may choose to specify 'tty' explicitly
                       in order to ensure that the REPL is only run under a TTY.
                       In cases where a REPL session may be running outside
                       of a TTY, such as in a spawned sub-process, the
                       user should specify a repl-mode of 'process'.
                       If the user desires to have no REPL machinery loaded
                       at all, the repl-mode can be set to 'disabled'.
                       With this setting STDIN is read entirely (until EOF)
                       and evaluated as if it were a program, without any
                       extra output.
                       Both 'process' and 'disabled' options bypass
                       TTY detection.
#?if moar
  --profile[=name]     write profile information to a file
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
  --tracing            output a line to stderr on every interpreter instr
                       (only if enabled in MoarVM)
#?endif
Note that only boolean single-letter options may be bundled.

The following environment variables are respected:

  RAKULIB       Modify the module search path
  PERL6LIB      Modify the module search path (DEPRECATED)
  NQP_HOME      Override the path of the NQP runtime files
  RAKUDO_HOME   Override the path of the Rakudo runtime files


♥); # end of usage statement

        nqp::exit(0);

        # TODO: create and install a man page for Raku; then add the following
        #       line to the end of the usage text above:
        #
        #  For more information, see the raku(1) man page.\n");
    }
}

# vim: expandtab sw=4
