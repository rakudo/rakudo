#!/usr/bin/perl
# Copyright (C) 2008-2011, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use lib 'tools/lib';
use NQP::Configure qw(slurp cmp_rev read_config);

if (-M 'Makefile' > -M 'tools/build/Makefile.in') {
    die <<EOM
Makefile is older than tools/build/Makefile.in, run something like

    perl Configure.pl

with --help or options as needed

EOM
}

my %nqp_config = read_config($ARGV[0]);
my $nqp_have = $nqp_config{'nqp::version'} || '';
my ($nqp_want) = split(' ', slurp('tools/build/NQP_REVISION'));

if (!$nqp_have || cmp_rev($nqp_have, $nqp_want) < 0) {
    my $parrot_option = '--gen-parrot or --with-parrot=path/to/bin/parrot';
    if (-x 'install/bin/parrot') {
        use Cwd 'abs_path';
        my $path = abs_path;
        $parrot_option = "--with-parrot=$path/install/bin/parrot";
    }
    die <<EOM
NQP $nqp_have is too old ($nqp_want required), run something like

    perl Configure.pl --gen-nqp $parrot_option

EOM
}

1; # versions are OK
