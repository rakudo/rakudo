#! perl

=head1 TITLE

gen-version.pl -- script to generate version information for HLL compilers

=cut

use POSIX 'strftime';

my $prefix = shift;

open(my $fh, '<', 'VERSION') or die $!;
my $VERSION = <$fh>;
close($fh);
chomp $VERSION;
my ($version, $release, $codename) = split(' ', $VERSION, 3);

if (-d '.git' && open(my $GIT, '-|', q|git describe --match "2*"|)) {
    $version = <$GIT>;
    close($GIT);
}

chomp $version;

my $builddate = strftime('%Y-%m-%dT%H:%M:%SZ', gmtime);

print <<"END_VERSION";
sub hll-config(\$config) {
    \$config<implementation>   := 'Rakudo';
    \$config<version>          := '$version';
    \$config<release-number>   := '$release';
    \$config<codename>         := '$codename';
    \$config<build-date>       := '$builddate';
    \$config<language_version> := '6.c';
    \$config<prefix>           := '$prefix';
}
END_VERSION

0;

