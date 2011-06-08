#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Getopt::Long;
use Cwd;
use lib "tools/lib";
use NQP::Configure qw(slurp gen_nqp read_config fill_template_file);


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

    my $with_nqp = $options{'with_nqp'};

    # Save options in config.status
    unlink('config.status');
    if (open(my $CONFIG_STATUS, '>', 'config.status')) {
        print $CONFIG_STATUS
            "$^X Configure.pl $config{'rakudo_config_status'} \$*\n";
        close($CONFIG_STATUS);
    }

    my ($nqp_want) = split(' ', slurp('tools/build/NQP_REVISION'));
    if (defined $options{'gen-parrot'}) {
        $with_nqp = gen_nqp($nqp_want, %options);
    }

    %config = (%config, read_config($with_nqp));
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
    --with-parrot=path/to/bin/parrot
                       Parrot executable to use to build NQP
    --gen-parrot       Download and build a copy of Parrot to use
    --parrot-option='--option=value'
                       Options to pass to parrot configuration for --gen-parrot
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
