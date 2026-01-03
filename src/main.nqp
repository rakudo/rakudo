use Perl6::Grammar;
use Perl6::Actions;
use Raku::Grammar;
use Raku::Actions;
use Perl6::Compiler;
use Perl6::SysConfig;

# Initialize Rakudo runtime support.
nqp::p6init();

my %rakudo-build-config := nqp::hash();
hll-config(%rakudo-build-config);
nqp::bindhllsym('default', 'SysConfig', Perl6::SysConfig.new(%rakudo-build-config));

# Create and configure compiler object.
my $comp := Perl6::Compiler.new();
$comp.language('Raku');
if nqp::getenvhash()<RAKUDO_RAKUAST> {
    $comp.parsegrammar(Raku::Grammar);
    $comp.parseactions(Raku::Actions);
    $comp.addstage('syntaxcheck', :before<ast>);
    $comp.addstage('qast', :after<ast>);
}
else {
    $comp.parsegrammar(Perl6::Grammar);
    $comp.parseactions(Perl6::Actions);
    $comp.addstage('syntaxcheck', :before<ast>);
    $comp.addstage('optimize', :after<ast>);
}

my $*OMIT-SOURCE := nqp::getenvhash()<RAKUDO_OMIT_SOURCE>;

# Add extra command line options.
my @clo := $comp.commandline_options();
@clo.push('setting=s');
@clo.push('n');
@clo.push('p');
@clo.push('doc=s?');
@clo.push('rakudoc=s?');
@clo.push('optimize=s?');
@clo.push('c');
@clo.push('I=s');
@clo.push('M=s');
@clo.push('rakudo-home=s');
@clo.push('disable-rakudo-opt');

#?if js
@clo.push('beautify');
#?endif

# Make Raku grammar / actions visible to HLL
nqp::bindhllsym('Raku', 'Grammar', Raku::Grammar);
nqp::bindhllsym('Raku', 'Actions', Raku::Actions);

# Set up END block list, which we'll run at exit.
nqp::bindhllsym('Raku', '@END_PHASERS', []);

# In an embedding environment, let @*ARGS be empty instead of crashing
nqp::bindhllsym('Raku', '$!ARGITER', 0);

#?if jvm
sub MAIN(*@ARGS) {
#?endif
#?if moar
sub MAIN(@ARGS) {
#?endif
#?if js
sub MAIN(*@ARGS) {
#?endif

    # Check standard options specified
    if nqp::getenvhash<RAKUDO_OPT> -> $opts {
        my @raw-opts := nqp::split(" ",$opts);

        # Check if RAKUDO_OPT is disabled: if so, simply remove the options
        for @ARGS {
            if $_ eq '--disable-rakudo-opt' {
                @raw-opts := ();
                last;
            }
        }

        # Simple chopper of last char
        my sub chop(str $it) { nqp::substr($it,0,nqp::chars($it) - 1) }

        # Converts any raw-opts to actual opts, handling "\ " expansion
        # in arguments.
        my @opts;
        while @raw-opts {
            my $arg := nqp::shift(@raw-opts);

            # Needs expanding
            if nqp::eqat($arg,'\\',-1) && @raw-opts {
                my str $final := chop($arg);
                $arg := nqp::shift(@raw-opts);
                while nqp::eqat($arg,'\\',-1) && @raw-opts {
                    $final := $final ~ " " ~ chop($arg);
                    $arg   := nqp::shift(@raw-opts);
                }
                nqp::push(@opts,$final ~ " " ~ $arg);
            }

            # Nothing special
            else {
                nqp::push(@opts,$arg);
            }
        }

        # Check all of the specified options
        my @ok;
        while @opts {
            my $flag := nqp::shift(@opts);
            my int $ok;

            # Test the allowed ones that may take an argument
            for <
              -I -M --optimize --rakudo-home --debug-port --repl-mode
              --profile --profile-compile --profile-kind --profile-stage
            > {
                if nqp::eqat($flag,$_,0) {
                    nqp::push(@ok,$flag);
                    nqp::push(@ok,nqp::shift(@opts)) if $flag eq $_ && @opts;
                    $ok := 1;
                    last;
                }
            }

            # Test the allowed ones that do not take an argument
            for <--stagestats --ll-exception --full-cleanup --debug-suspend> {
                if $_ eq $flag {
                    nqp::push(@ok,$flag);
                    $ok := 1;
                    last;
                }
            }
            unless $ok {
                nqp::say("Not allowed to use '$flag' as an argument in RAKUDO_OPT");
                nqp::exit(1);
            }
        }
        nqp::splice(@ARGS,@ok,1,0);
    }

    # Enter the compiler.
    my $*STACK-ID := 0;
    $comp.command_line(@ARGS, :encoding('utf8'));

    # do all the necessary actions at the end, if any
    if nqp::gethllsym('Raku', '&THE_END') -> $THE_END {
        $THE_END()
    }
}

# vim: expandtab sw=4
