use Perl6::Grammar;
use Perl6::Actions;
use Perl6::Compiler;


# Initialize Rakudo runtime support.
nqp::p6init();

# Create and configure compiler object.
my $comp := Perl6::Compiler.new();

$comp.language('Raku');
$comp.parsegrammar(Perl6::Grammar);
$comp.parseactions(Perl6::Actions);
$comp.addstage('syntaxcheck', :before<ast>);
$comp.addstage('optimize', :after<ast>);
hll-config($comp.config);
nqp::bindhllsym('Raku', '$COMPILER_CONFIG', $comp.config);


# Determine Perl6 and NQP dirs.
my $config := nqp::backendconfig();
my $sep := $config<osname> eq 'MSWin32' ?? '\\' !! '/';
#?if jvm
my $execname := nqp::atkey(nqp::jvmgetproperties,'perl6.execname');
#?endif
#?if !jvm
my $execname := nqp::execname();
#?endif
my $install-dir := $execname eq ''
    ?? $comp.config<prefix>
    !! nqp::substr($execname, 0, nqp::rindex($execname, $sep, nqp::rindex($execname, $sep) - 1));

my $rakudo-home := $comp.config<static-rakudo-home>
    || nqp::getenvhash()<RAKUDO_HOME>
    // nqp::getenvhash()<PERL6_HOME>
    // $install-dir ~ '/share/perl6';
if nqp::substr($rakudo-home, nqp::chars($rakudo-home) - 1) eq $sep {
    $rakudo-home := nqp::substr($rakudo-home, 0, nqp::chars($rakudo-home) - 1);
}

my $nqp-home := $comp.config<static-nqp-home>
    || nqp::getenvhash()<NQP_HOME>
    // $install-dir ~ '/share/nqp';
if nqp::substr($nqp-home, nqp::chars($nqp-home) - 1) eq $sep {
    $nqp-home := nqp::substr($nqp-home, 0, nqp::chars($nqp-home) - 1);
}

nqp::bindhllsym('Raku', '$RAKUDO_HOME', $rakudo-home);
nqp::bindhllsym('Raku', '$NQP_HOME', $nqp-home);


# Add extra command line options.
my @clo := $comp.commandline_options();
@clo.push('parsetrace');
@clo.push('setting=s');
@clo.push('n');
@clo.push('p');
@clo.push('doc=s?');
@clo.push('optimize=s?');
@clo.push('c');
@clo.push('I=s');
@clo.push('M=s');
@clo.push('nqp-lib=s');

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
    $comp.command_line(@ARGS, :encoding('utf8'), :transcode('ascii iso-8859-1'));

    # do all the necessary actions at the end, if any
    if nqp::gethllsym('Raku', '&THE_END') -> $THE_END {
        $THE_END()
    }
}
