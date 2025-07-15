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
    my $loader := nqp::getcurhllsym('ModuleLoader');

    my $RakuGrammarModule := $loader.load_module("Raku::Grammar");
    my $RakuActionsModule := $loader.load_module("Raku::Actions");

    my $globalish := $RakuGrammarModule<GLOBALish>.WHO;

    $comp.parsegrammar($globalish<Raku>.WHO<Grammar>);
    $comp.parseactions($globalish<Raku>.WHO<Actions>);

    # Make Raku grammar / actions visible to HLL
    nqp::bindhllsym('Raku', 'Grammar', $globalish<Raku>.WHO<Grammar>);
    nqp::bindhllsym('Raku', 'Actions', $globalish<Raku>.WHO<Actions>);

    $comp.addstage('syntaxcheck', :before<ast>);
    $comp.addstage('qast', :after<ast>);
}
else {
    my $loader := nqp::getcurhllsym('ModuleLoader');
    my $Perl6GrammarModule := $loader.load_module("Perl6::Grammar");
    my $Perl6ActionsModule := $loader.load_module("Perl6::Actions");

    my $globalish := $Perl6GrammarModule<GLOBALish>.WHO;

    $comp.parsegrammar($globalish<Perl6>.WHO<Grammar>);
    $comp.parseactions($globalish<Perl6>.WHO<Actions>);

    $comp.addstage('syntaxcheck', :before<ast>);
    $comp.addstage('optimize', :after<ast>);
}

my $*OMIT-SOURCE := nqp::getenvhash()<RAKUDO_OMIT_SOURCE>;

# Add extra command line options.
my @clo := $comp.commandline_options();
@clo.push('parsetrace');
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

#?if js
@clo.push('beautify');
#?endif

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
    # Enter the compiler.
    my %defaults;
    if nqp::existskey(nqp::getenvhash, 'RAKUDO_OPT') {
        my @env-args := nqp::split(" ", nqp::getenvhash<RAKUDO_OPT>);
        my $p := HLL::CommandLine::Parser.new($comp.commandline_options);
        $p.add-stopper('-e');
        $p.stop-after-first-arg;
        my $res := $p.parse(@env-args);
        if $res {
            %defaults := $res.options;
        }
    }
    my $*STACK-ID := 0;
    $comp.command_line(@ARGS, :encoding('utf8'), |%defaults);

    # do all the necessary actions at the end, if any
    if nqp::gethllsym('Raku', '&THE_END') -> $THE_END {
        $THE_END()
    }
}

# vim: expandtab sw=4
