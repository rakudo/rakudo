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
    # Enter the compiler.
    if nqp::getenvhash<RAKUDO_OPT> -> $opts {
        nqp::splice(@ARGS,nqp::split(" ",$opts),1,0);
    }
    my $*STACK-ID := 0;
    $comp.command_line(@ARGS, :encoding('utf8'));

    # do all the necessary actions at the end, if any
    if nqp::gethllsym('Raku', '&THE_END') -> $THE_END {
        $THE_END()
    }
}

# vim: expandtab sw=4
