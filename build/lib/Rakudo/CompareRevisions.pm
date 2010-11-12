package Rakudo::CompareRevision;
use strict;
use warnings;

sub parse_parrot_git_describe {
    my $g = shift;
    my $sep = qr/[_\W]/;
    $g =~ /^REL(?:EASE)?$sep(\d+)$sep(\d+)$sep(\d+)(?:-(\d+)-g[a-f0-9]*)?$/
        or die "Invalid revision specifier: '$g' "
               ."(expected something of format RELEASE_1_2_3-123-gdeadbee)\n";
    my @c = ($1, $2, $3, $4 || 0);
    return @c;
}

sub compare_parrot_revs {
    my ($aa, $bb) = @_;
    return  1 if $bb =~ /^r?\d+$/;
    return -1 if $aa =~ /^r?\d+$/;
    my @a = parse_git_describe($aa);
    my @b = parse_git_describe($bb);
    for (0..3) {
        my $cmp = $a[$_] <=> $b[$_];
        return $cmp if $cmp;
    }
    return 0;
}

1;
