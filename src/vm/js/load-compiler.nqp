use Perl6::Grammar;
use Perl6::Actions;
use Perl6::Compiler;
use Perl6::SysConfig;

my %rakudo-build-config := nqp::hash();
hll-config(%rakudo-build-config);
nqp::bindhllsym('default', 'SysConfig', Perl6::SysConfig.new(%rakudo-build-config));

# Create and configure compiler object.
my $comp := Perl6::Compiler.new();
$comp.language('Raku');
$comp.parsegrammar(Perl6::Grammar);
$comp.parseactions(Perl6::Actions);
$comp.addstage('syntaxcheck', :before<ast>);
$comp.addstage('optimize', :after<ast>);


nqp::bindattr($comp, HLL::Compiler, '%!cli-options', nqp::hash());
nqp::bindattr($comp, HLL::Compiler, '$!user_progname', nqp::gethllsym('Raku', 'progname'));

$comp.set_language_version('6.d');

sub hll-config($config) {
    $config<implementation>   := 'Rakudo';
    $config<version>          := '2018.03-1433-g602ca5bd3';
    $config<release-number>   := '';
    $config<codename>         := '';
    $config<language-version> := '6.d';
    $config<can-language-versions>
        := nqp::list('6.c', '6.d', '6.d.PREVIEW');
    $config<prefix>           := '';
    $config<libdir>           := '';
    $config<source-digest>    := '907e209148676a368121c7c2ca5cdab8b5c77c66';
}

# Set up END block list, which we'll run at exit.
nqp::bindhllsym('Raku', '@END_PHASERS', []);

# In an embedding environment, let @*ARGS be empty instead of crashing
nqp::bindhllsym('Raku', '$!ARGITER', 0);

nqp::sethllconfig('Raku', nqp::hash(
    'uncaught_control', -> $exception {
        nqp::getcomp('Raku').handle-control($exception);
    },
    'uncaught_exception', -> $exception {
        nqp::getcomp('Raku').handle-exception($exception);
    }
));

# vim: expandtab sw=4
