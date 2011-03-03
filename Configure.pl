#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Getopt::Long;
use Cwd;
use lib "build/lib";
use Parrot::CompareRevisions qw(compare_revs parse_revision_file read_config version_from_git_describe);

MAIN: {
    my %options;
    GetOptions(\%options, 'help!', 'parrot-config=s', 'makefile-timing!',
               'gen-parrot!', 'gen-parrot-prefix=s', 'gen-parrot-option=s@');

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }

    # Update/generate parrot build if needed
    if ($options{'gen-parrot'}) {
        my @opts    = @{ $options{'gen-parrot-option'} || [] };
        my $prefix  = $options{'gen-parrot-prefix'} || cwd()."/parrot_install";
        # parrot's Configure.pl mishandles win32 backslashes in --prefix
        $prefix =~ s{\\}{/}g;
        my @command = ($^X, "build/gen_parrot.pl", "--prefix=$prefix", ($^O !~ /win32/i ? "--optimize" : ()), @opts);

        print "Generating Parrot ...\n";
        print "@command\n\n";
        system(@command) == 0
            or die "Error while executing @command; aborting\n";
    }

    # Get a list of parrot-configs to invoke.
    my @parrot_config_exe = qw(
        parrot_install/bin/parrot_config
        ../../parrot_config
        parrot_config
    );
    if (exists $options{'gen-parrot-prefix'}) {
        @parrot_config_exe =
                $options{'gen-parrot-prefix'} . '/bin/parrot_config';
    }

    if ($options{'parrot-config'} && $options{'parrot-config'} ne '1') {
        @parrot_config_exe = ($options{'parrot-config'});
    }

    # Get configuration information from parrot_config
    my %config = read_config(@parrot_config_exe);

    # Determine the revision of Parrot we require
    my $git_describe = parse_revision_file;
    my $parrot_version = version_from_git_describe($git_describe);

    my $parrot_errors = '';
    if (!%config) {
        $parrot_errors .= "Unable to locate parrot_config\n"; 
    }
    else {
        if ($config{git_describe}) {
            # a parrot built from git
            if (compare_revs($git_describe, $config{'git_describe'}) > 0) {
                $parrot_errors .= "Parrot revision $git_describe required (currently $config{'git_describe'})\n";
            }
        }
        else {
            # not built from a git repo - let's assume it's a release
            if (version_int($parrot_version) > version_int($config{'VERSION'})) {
                $parrot_errors .= "Parrot version $parrot_version required (currently $config{VERSION})\n";
            }
        }
    }

    if ($parrot_errors) {
        die <<"END";
===SORRY!===
$parrot_errors
To automatically clone (git) and build a copy of parrot $git_describe,
try re-running Configure.pl with the '--gen-parrot' option.
Or, use the '--parrot-config' option to explicitly specify
the location of parrot_config to be used to build Rakudo Perl.

END
    }

    # Verify the Parrot installation is sufficient for building Rakudo
    verify_parrot(%config);

    # Create the Makefile using the information we just got
    create_makefile($options{'makefile-timing'}, %config);
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
or '$make spectest' to check out (via git) a copy of the Perl 6
official test suite and run its tests.

END
    exit 0;

}


sub verify_parrot {
    print "Verifying Parrot installation...\n";
    my %config = @_;
    my $EXE            = $config{'exe'};
    my $PARROT_BIN_DIR = $config{'bindir'};
    my $PARROT_VERSION = $config{'versiondir'};
    my $PARROT_LIB_DIR = $config{'libdir'}.$PARROT_VERSION;
    my $PARROT_SRC_DIR = $config{'srcdir'}.$PARROT_VERSION;
    my $PARROT_INCLUDE_DIR = $config{'includedir'}.$PARROT_VERSION;
    my $PARROT_TOOLS_DIR = "$PARROT_LIB_DIR/tools";
    my @required_files = (
        "$PARROT_LIB_DIR/library/PGE/Perl6Grammar.pbc",
        "$PARROT_LIB_DIR/library/PCT/HLLCompiler.pbc",
        "$PARROT_BIN_DIR/ops2c".$EXE,
        "$PARROT_TOOLS_DIR/build/pmc2c.pl",
        "$PARROT_SRC_DIR",
        "$PARROT_SRC_DIR/pmc",
        "$PARROT_INCLUDE_DIR",
        "$PARROT_INCLUDE_DIR/pmc",
    );
    my @missing = map { "    $_" } grep { ! -e } @required_files;
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
    my ($makefile_timing, %config) = @_;

    my $maketext = slurp( 'build/Makefile.in' );

    $config{'stagestats'} = $makefile_timing ? '--stagestats' : '';
    $config{'win32_libparrot_copy'} = $^O eq 'MSWin32' ? 'copy $(PARROT_BIN_DIR)\libparrot.dll .' : '';
    $maketext =~ s/@(\w+)@/$config{$1}/g;
    if ($^O eq 'MSWin32') {
        $maketext =~ s{/}{\\}g;
        $maketext =~ s{\\\*}{\\\\*}g;
        $maketext =~ s{(?:git|http):\S+}{ do {my $t = $&; $t =~ s'\\'/'g; $t} }eg;
        $maketext =~ s/.*curl.*/do {my $t = $&; $t =~ s'%'%%'g; $t}/meg;
    }

    if ($makefile_timing) {
        $maketext =~ s{(?<!\\\n)^\t(?!\s*-?cd)(?=[^\n]*\S)}{\ttime }mg;
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
    --parrot-config=/path/to/parrot_config
                       Use config information from parrot_config executable
Experimental developer's options:
    --makefile-timing  Insert 'time' command all over in the Makefile
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
