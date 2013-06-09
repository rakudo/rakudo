#! perl

=head1 TITLE

gen-version.pl -- script to generate version information for HLL compilers

=cut

use POSIX 'strftime';

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
    \$config<name>           := 'rakudo';
    \$config<version>        := '$version';
    \$config<release-number> := '$release';
    \$config<codename>       := '$codename';
    \$config<build-date>     := '$builddate';
}
END_VERSION

0;

