use Perl6::Grammar;
use Perl6::Actions;
use Perl6::Compiler;

# Create and configure compiler object.
my $comp := Perl6::Compiler.new();
$comp.language('perl6');
$comp.parsegrammar(Perl6::Grammar);
$comp.parseactions(Perl6::Actions);
$comp.addstage('syntaxcheck', :before<ast>);
$comp.addstage('optimize', :after<ast>);


$comp.set_language_version('6.d');

sub hll-config($config) {
    $config<implementation>   := 'Rakudo';
    $config<version>          := '2018.03-1433-g602ca5bd3';
    $config<release-number>   := '';
    $config<codename>         := '';
    $config<build-date>       := '2018-09-30T11:43:50Z';
    $config<language-version> := '6.d';
    $config<can-language-versions>
        := nqp::list('6.c', '6.d', '6.d.PREVIEW');
    $config<prefix>           := '';
    $config<libdir>           := '';
    $config<source-digest>    := '907e209148676a368121c7c2ca5cdab8b5c77c66';
}

hll-config($comp.config);

nqp::bindhllsym('perl6', '$COMPILER_CONFIG', $comp.config);


# Set up END block list, which we'll run at exit.
nqp::bindhllsym('perl6', '@END_PHASERS', []);

# In an embedding environment, let @*ARGS be empty instead of crashing
nqp::bindhllsym('perl6', '$!ARGITER', 0);
