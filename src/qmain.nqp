use QPerl6::Grammar;
use QPerl6::Actions;
use QPerl6::Compiler;

# Need the old Perl6::World for a dependency issue.
use Perl6::World;

sub MAIN(@ARGS) {
    # Initialize dynops.
    pir::rakudo_dynop_setup__v();

    # Bump up Parrot's recursion limit
    pir::getinterp__P().recursion_limit(100000);

    # Create and configure compiler object.
    my $comp := QPerl6::Compiler.new();
    $comp.language('perl6');
    $comp.parsegrammar(QPerl6::Grammar);
    $comp.parseactions(QPerl6::Actions);
    $comp.addstage('syntaxcheck', :before<past>);
    $comp.addstage('optimize', :before<post>);
    hll-config($comp.config);
    my $COMPILER_CONFIG := $comp.config;
    
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

    # Set up module loading trace
    my @*MODULES := [];
    
    # Set up END block list, which we'll run at exit.
    my @*END_PHASERS := [];

    # Enter the compiler.
    $comp.command_line(@ARGS, :encoding('utf8'), :transcode('ascii iso-8859-1'));
    
    # Run any END blocks before exiting.
    for @*END_PHASERS { $_() }
}
