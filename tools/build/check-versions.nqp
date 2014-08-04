#!nqp
# Copyright (C) 2014, The Perl Foundation.

sub read_config($prog) {
    my $tmpfile := nqp::sha(nqp::time_i());

    nqp::shell("$prog --show-config  > $tmpfile 2>&1", nqp::cwd(), nqp::getenvhash());
    my $fh := open($tmpfile, :r);
    my %config;
    while my $line := nqp::readlinefh($fh) {
        my @match := $line ~~ / ^ (<-[ \s = ]>+) '=' (.*) /;
        %config{@match[0]} := @match[1] if ?@match;
    }
    close($fh);
    unlink($tmpfile);
    return %config;
}

# TODO test this!
sub parse_revision($rev) {
    my $sep := regex { <[_.]> };
    my @matches := $rev ~~ / (\d+) $sep (\d+) [ $sep (\d+) ]?[ '-' (\d+) '-g' <[0..9a..f]>* ]?$ /;
    return @matches;
}

# TODO test this!
sub cmp_rev($rev-a, $rev-b) {
    my @a := parse-revision($rev-a);
    my @b := parse-revision($rev-b);
    my $cmp := 0;
    for 0..3 -> $i {
        $cmp := nqp::cmp_i(@a[$i],@b[$i]);
        last if $cmp;
    }
    return $cmp;
}

sub MAIN(*@ARGS) {
    my $m1 := nqp::stat('Makefile', nqp::const::STAT_MODIFYTIME);
    my $m2 := nqp::stat('tools/build/Makefile-Parrot.in', nqp::const::STAT_MODIFYTIME);
    if $m1 > $m2 {
        nqp::die(qq«
Makefile is older than tools/build/Makefile-Parrot.in, run something like

    perl Configure.pl

with --help or options as needed

»);
    }

    my %nqp-config := read_config(@ARGS[0]);
    my $nqp-have := %nqp-config<nqp::version> || '';
    my $nqp-revision = slurp('tools/build/NQP_REVISION')
    $nqp-revision := subst($nqp-revision, / \n $ /, '');
    my @nqp-want := split(' ', $nqp-revision);
    my $nwp-want := @nqp-want[0];

    if !$nqp_have || cmp_rev($nqp_have, $nqp_want) < 0 {
        my $parrot-option := '--gen-parrot or --prefix=$prefix';
        if nqp::stat('install/bin/parrot', nqp::const::STAT_ISEXECUTABLE) {     # TODO Make this work!
            my $path := abs_path;                                               # TODO create abs_path
            $parrot-option := "--prefix=$path/install";
        }
        nqp::die(qq«
NQP $nqp_have is too old ($nqp_want required), run something like

    perl Configure.pl --gen-nqp $parrot_option

»);
    }

    return 1;
}
