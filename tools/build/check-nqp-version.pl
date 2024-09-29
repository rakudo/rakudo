#! perl

=head1 TITLE

check-nqp-version.pl -- script to trigger a rebuild whenever nqp changes

=cut

use strict;
use warnings;
use IPC::Cmd qw<run>;

my $nqp_version_file = shift;
my $nqp = shift;

my ($ok, $err, $config) = run( command => [$nqp, '--show-config' ]);
foreach (@$config) {
    chomp;
    if (my ($digest) = /nqp::source-digest=(.*)/) {
        if (-e $nqp_version_file) {
            open my $version_file, '<', $nqp_version_file;
            my $old = <$version_file>;
            close $version_file;
            chomp($old);
            exit if $old eq $digest; # no change - nothing to do
        }
        open my $version_file, '>', $nqp_version_file;
        print { $version_file } $digest;
        close $version_file;
        exit;
    }
}

# vim: expandtab sw=4
