#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Getopt::Long;
use Cwd;

MAIN: {
    my %options;
    GetOptions(\%options, 'help!', 'parrot-config=s',
               'gen-parrot!', 'gen-parrot-prefix=s', 'gen-parrot-option=s@');

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }

    # Determine the revision of Parrot we require
    open my $REQ, "build/PARROT_REVISION"
      || die "cannot open build/PARROT_REVISION\n";
    my ($reqsvn, $reqpar) = split(' ', <$REQ>);
    $reqsvn += 0;
    close $REQ;

    # Update/generate parrot build if needed
    if ($options{'gen-parrot'}) {
        my @opts    = @{ $options{'gen-parrot-option'} || [] };
        my $prefix  = $options{'gen-parrot-prefix'} || cwd()."/parrot_install";
        my @command = ($^X, "build/gen_parrot.pl", "--prefix=$prefix", @opts);

        print "Generating Parrot ...\n";
        print "@command\n\n";
        system @command;
    }

    # Get a list of parrot-configs to invoke.
    my @parrot_config_exe = qw(
        parrot_install/bin/parrot_config
        ../../parrot_config
        parrot_config
    );

    if ($options{'parrot-config'} && $options{'parrot-config'} ne '1') {
        @parrot_config_exe = ($options{'parrot-config'});
    }

    # Get configuration information from parrot_config
    my %config = read_parrot_config(@parrot_config_exe);

    my $parrot_errors = '';
    if (!%config) { 
        $parrot_errors .= "Unable to locate parrot_config\n"; 
    }
    elsif ($reqsvn > $config{'revision'} &&
            ($reqpar eq '' || version_int($reqpar) > version_int($config{'VERSION'}))) {
        $parrot_errors .= "Parrot revision r$reqsvn required (currently r$config{'revision'})\n";
    }

    if ($parrot_errors) {
        die <<"END";
===SORRY!===
$parrot_errors
To automatically checkout (svn) and build a copy of parrot r$reqsvn,
try re-running Configure.pl with the '--gen-parrot' option.
Or, use the '--parrot-config' option to explicitly specify
the location of parrot_config to be used to build Rakudo Perl.

END
    }

    # Verify the Parrot installation is sufficient for building Rakudo
    verify_parrot(%config);

    # Create the Makefile using the information we just got
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


sub verify_parrot {
    print "Verifying Parrot installation...\n";
    my %config = @_;
    my $PARROT_VERSION = $config{'versiondir'};
    my $PARROT_LIB_DIR = $config{'libdir'}.$PARROT_VERSION;
    my $PARROT_SRC_DIR = $config{'srcdir'}.$PARROT_VERSION;
    my $PARROT_INCLUDE_DIR = $config{'includedir'}.$PARROT_VERSION;
    my $PARROT_TOOLS_DIR = "$PARROT_LIB_DIR/tools";
    my @required_files = (
        "$PARROT_LIB_DIR/library/PGE/Perl6Grammar.pbc",
        "$PARROT_LIB_DIR/library/PCT/HLLCompiler.pbc",
        "$PARROT_LIB_DIR/languages/nqp/nqp.pbc",
        "$PARROT_TOOLS_DIR/build/ops2c.pl",
        "$PARROT_TOOLS_DIR/build/pmc2c.pl",
        "$PARROT_SRC_DIR",
        "$PARROT_SRC_DIR/pmc",
        "$PARROT_INCLUDE_DIR",
        "$PARROT_INCLUDE_DIR/pmc",
    );
    my @missing;
    for my $reqfile (@required_files) {
        push @missing, "    $reqfile" unless -e $reqfile;
    }
    if (@missing) {
        my $missing = join("\n", @missing);
        die <<"END";

===SORRY!===
I'm missing some needed files from the Parrot installation:
$missing
(Perhaps you need to use Parrot's "make install-dev" or
install the "parrot-devel" package for your system?)

END
    }
}

#  Generate a Makefile from a configuration
sub create_makefile {
    my %config = @_;

    my $maketext = slurp( 'build/Makefile.in' );

    $config{'win32_libparrot_copy'} = $^O eq 'MSWin32' ? 'copy $(PARROT_BIN_DIR)\libparrot.dll .' : '';
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

sub version_int {
    sprintf('%d%03d%03d', split(/\./, $_[0]))
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
