#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Getopt::Long;
use Cwd;
use lib "tools/lib";
use NQP::Configure qw(sorry slurp gen_nqp read_config fill_template_file);


MAIN: {
    my %config;
    $config{'rakudo_config_status'} = join(' ', map { "\"$_\""} @ARGV);

    my %options;
    GetOptions(\%options, 'help!', 'prefix=s',
               'with-nqp=s', 'gen-nqp:s',
               'with-parrot=s', 'gen-parrot:s',
               'make-install!', 'makefile-timing!',
               );

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }

    my $with_nqp    = $options{'with-nqp'};
    my $gen_nqp     = $options{'gen-nqp'};
    my $with_parrot = $options{'with-parrot'};
    my $gen_parrot  = $options{'gen-parrot'};

    # Save options in config.status
    unlink('config.status');
    if (open(my $CONFIG_STATUS, '>', 'config.status')) {
        print $CONFIG_STATUS
            "$^X Configure.pl $config{'rakudo_config_status'} \$*\n";
        close($CONFIG_STATUS);
    }

    # --with-parrot and --gen-parrot imply --gen-nqp
    if (!defined $gen_nqp && (defined $with_parrot || defined $gen_parrot)) {
        $gen_nqp = '';
    }

    # determine the version of NQP we want
    my ($nqp_want) = split(' ', slurp('tools/build/NQP_REVISION'));

    if (defined $gen_nqp) {
        $with_nqp = gen_nqp($nqp_want, %options);
    }

    my %nqp_config = read_config($with_nqp);
    my $nqp_have   = $nqp_config{'nqp::version'};
    my @errors;
    if (!%nqp_config) {
        push @errors, "Unable to locate a valid NQP executable.";
    }
    elsif (cmp_rev($nqp_have, $nqp_want) < 0) {
        push @errors, "NQP revision $nqp_want required (currently $nqp_have).";
    }

    %config = (%config, %nqp_config);

    if (@errors && !defined $gen_nqp) {
        push @errors, 
          "To automatically clone (git) and build a copy of NQP $nqp_want,",
          "try re-running Configure.pl with the '--gen-nqp' or '--gen-parrot'",
          "options.  Or, use '--with-nqp=' or '--with-parrot=' to explicitly",
          "specify the NQP or Parrot executable to use to build Rakudo.";
    }

    sorry(@errors) if @errors;

    print "Using $with_nqp (version $config{'nqp::version'}).\n";

    $config{'makefile-timing'} = $options{'makefile-timing'};
    $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
    $config{'shell'} = 'sh';
    if ($^O eq 'MSWin32') {
        $config{'shell'} = 'cmd';
        $config{'win32_libparrot_copy'} = 
            'copy $(PARROT_BIN_DIR)\libparrot.dll .';
    }

    fill_template_file('tools/build/Makefile.in', 'Makefile', %config);

    exit 0;
}


#  Print some help text.
sub print_help {
    print <<'END';
Configure.pl - NQP Configure

General Options:
    --help             Show this text
    --prefix=dir       Install files in dir
    --with-nqp=path/to/bin/nqp
                       NQP executable to use to build Rakudo
    --gen-nqp[=branch]
                       Download and build a copy of NQP
        --with-parrot=path/to/bin/parrot
                       Parrot executable to use to build NQP
        --gen-parrot[=branch]
                       Download and build a copy of Parrot
    --makefile-timing  Enable timing of individual makefile commands
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
