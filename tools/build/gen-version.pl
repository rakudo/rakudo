#! perl

=head1 TITLE

gen-version.pl -- script to generate version information for HLL compilers

=cut

use Digest::SHA;
use File::Find;
use POSIX 'strftime';

my $prefix = shift;
my $libdir = shift;

open(my $fh, '<', 'VERSION') or die $!;
my $VERSION = <$fh>;
close($fh);
chomp $VERSION;
my ($version, $release, $codename) = split(' ', $VERSION, 3);

if (-d '.git' && open(my $GIT, '-|', q|git describe --match "2*"|)) {
    my $git_version = <$GIT>; # may be empty if we didn't fetch any tags
    $version = $git_version || "$version.0000.1";
    close($GIT);
}

chomp $version;

my $builddate = strftime('%Y-%m-%dT%H:%M:%SZ', gmtime);

my $sha = Digest::SHA->new;
find(sub { next unless /\.(nqp|pm6)\z/; $sha->addfile($_) }, "src");
my $source_digest = $sha->hexdigest;

print <<"END_VERSION";
sub hll-config(\$config) {
    \$config<implementation>   := 'Rakudo';
    \$config<version>          := '$version';
    \$config<release-number>   := '$release';
    \$config<codename>         := '$codename';
    \$config<build-date>       := '$builddate';
    \$config<language_version> := '6.c';
    \$config<prefix>           := '$prefix';
    \$config<libdir>           := '$libdir';
    \$config<source-digest>    := '$source_digest';
}
END_VERSION

0;
