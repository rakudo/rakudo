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
$sha->addfile('gen/nqp-version');
my $source_digest = $sha->hexdigest;

print <<"END_VERSION";
sub hll-config(\$config) {
    \$config<implementation>   := 'Rakudo';
    \$config<version>          := '$version';
    \$config<release-number>   := '$release';
    \$config<codename>         := '$codename';
    \$config<language-version> := '6.d';
    \$config<can-language-versions>
        := nqp::list('6.c', '6.d', '6.d.PREVIEW');
    \$config<prefix>           := '$prefix';
    \$config<libdir>           := '$libdir';
    \$config<source-digest>    := '$source_digest';
}
END_VERSION

0;
