#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Getopt::Long;

MAIN: {
    my %options;
    GetOptions(\%options, 'help!', 'parrot-config=s',
               'gen-parrot!', 'gen-parrot-option=s@');

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }

    # Determine the revision of Parrot we require
    open my $REQ, "build/PARROT_REVISION"
      || die "cannot open build/PARROT_REVISION\n";
    my $required = 0+<$REQ>;
    close $REQ;

    # Update/generate parrot build if needed
    if ($options{'gen-parrot'}) {
        my @opts    = @{ $options{'gen-parrot-option'} || [] };
        my @command = ($^X, "build/gen_parrot.pl", @opts);

        print "Generating Parrot ...\n";
        print "@command\n\n";
        system @command;
    }

    # Get a list of parrot-configs to invoke.
    my @parrot_config_exe = qw(
        parrot/parrot_config
        ../../parrot_config
        parrot_config
    );

    if ($options{'parrot-config'} && $options{'parrot-config'} ne '1') {
        @parrot_config_exe = ($options{'parrot-config'});
    }

    #  Get configuration information from parrot_config
    my %config = read_parrot_config(@parrot_config_exe);

    my $parrot_errors = '';
    if (!%config) { 
        $parrot_errors .= "Unable to locate parrot_config\n"; 
    }
    elsif ($required > $config{'revision'}) {
        $parrot_errors .= "Parrot revision r$required required (currently r$config{'revision'})\n";
    }

    if ($parrot_errors) {
        die <<"END";
$parrot_errors
To automatically checkout (svn) and build a copy of parrot r$required,
try re-running Configure.pl with the '--gen-parrot' option.
Or, use the '--parrot-config' option to explicitly specify
the location of parrot_config to be used to build Rakudo Perl.
END
    }

#  Create the Makefile using the information we just got
    create_makefile(%config);
    my $make = $config{'make'};

    {
        no warnings;
        print "Cleaning up ...\n";
        if (open my $CLEAN, '-|', "$make clean") { 
            my @slurp = <$CLEAN>;
            close $CLEAN; 
        }
    }

    print <<"END";

You can now use '$make' to build Rakudo Perl.
After that, you can use '$make test' to run some local tests,
or '$make spectest' to check out (via svn) a copy of the Perl 6
official test suite and run its tests.

END
    exit 0;

}


sub read_parrot_config {
    my @parrot_config_exe = @_;
    my %config = ();
    for my $exe (@parrot_config_exe) {
        no warnings;
        if (open my $PARROT_CONFIG, '-|', "$exe --dump") {
            print "\nReading configuration information from $exe ...\n";
            while (<$PARROT_CONFIG>) {
                if (/(\w+) => '(.*)'/) { $config{$1} = $2 }
            }
            close $PARROT_CONFIG or die $!;
            last if %config;
        }
    }
    return %config;
}


#  Generate a Makefile from a configuration
sub create_makefile {
    my %config = @_;

    my $maketext = slurp( 'build/Makefile.in' );

    $config{'win32_libparrot_copy'} = $^O eq 'MSWin32' ? 'copy $(BUILD_DIR)\libparrot.dll .' : '';
    $maketext =~ s/@(\w+)@/$config{$1}/g;
    if ($^O eq 'MSWin32') {
        $maketext =~ s{/}{\\}g;
        $maketext =~ s{\\\*}{\\\\*}g;
        $maketext =~ s{http:\S+}{ do {my $t = $&; $t =~ s'\\'/'g; $t} }eg;
    }

    my $outfile = 'Makefile';
    print "\nCreating $outfile ...\n";
    open(my $MAKEOUT, '>', $outfile) ||
        die "Unable to write $outfile\n";
    print {$MAKEOUT} $maketext;
    close $MAKEOUT or die $!;

    return;
}

sub slurp {
    my $filename = shift;

    open my $fh, '<', $filename or die "Unable to read $filename\n";
    local $/ = undef;
    my $maketext = <$fh>;
    close $fh or die $!;

    return $maketext;
}


#  Print some help text.
sub print_help {
    print <<'END';
Configure.pl - Rakudo Configure

General Options:
    --help             Show this text
    --gen-parrot       Download and build a copy of Parrot to use
    --gen-parrot-option='--option=value'
                       Set parrot config option when using --gen-parrot
    --parrot-config=(config)
                       Use configuration information from config
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
