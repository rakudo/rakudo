use Perl6::Grammar;
use Perl6::Actions;
use Perl6::Compiler;

sub MAIN(@ARGS) {
    # Initialize dynops.
    pir::rakudo_dynop_setup__v();

    # Create and configure compiler object.
    my $comp := Perl6::Compiler.new();
    $comp.language('perl6');
    $comp.parsegrammar(Perl6::Grammar);
    $comp.parseactions(Perl6::Actions);
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
    
    # Set up END block list, which we'll run at exit.
    my @*END_PHASERS := [];

    # Enter the compiler.
    $comp.command_line(@ARGS, :encoding('utf8'), :transcode('ascii iso-8859-1'));
    
    # Run any END blocks before exiting.
    for @*END_PHASERS { $_() }
}
