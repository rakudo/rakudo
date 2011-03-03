package Parrot::CompareRevisions;
use strict;
use warnings;

use base qw(Exporter);
our @EXPORT_OK = qw(
    parse_revision_file
    parse_git_describe
    version_from_git_describe
    compare_revs
    read_config
);

sub config_check {

}

sub parse_revision_file {
    my $filename = shift || 'build/PARROT_REVISION';
    open my $REQ, '<', $filename
      or die "cannot open '$filename' for reading: $!\n";
    local $/;
    my $str  = <$REQ>;
    close $REQ;
    $str =~ s/\A\s+//;
    $str =~ s/\s*\z//;

    return $str;
}

sub parse_git_describe {
    my $g = shift;
    my $sep = qr/[_\W]/;
    $g =~ /^REL(?:EASE)?$sep(\d+)(?:$sep(\d+)$sep(\d+)(?:-(\d+)-g[a-f0-9]*)?)?$/
        or die "Invalid revision specifier: '$g' "
               ."(expected something of format RELEASE_1_2_3-123-gdeadbee)\n";
    my @c = ($1, $2 || 0, $3 || 0, $4 || 0);
    return @c;
}

sub version_from_git_describe {
    my @chunks = (parse_git_describe @_)[0..2];
    return join '.', @chunks;
}

sub compare_revs {
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


sub read_config {
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

1;
