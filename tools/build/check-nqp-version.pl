#! perl

=head1 TITLE

check-nqp-version.pl -- script to trigger a rebuild whenever nqp changes

=cut

use strict;
use warnings;
use IPC::Cmd qw<run>;

use constant NQP_VERSION_FILE => 'gen/nqp-version';

my $nqp = shift;

my ($ok, $err, $config) = run( command => [$nqp, '--show-config' ]);
foreach (@$config) {
    chomp;
    if (my ($digest) = /nqp::source-digest=(.*)/) {
        if (-e NQP_VERSION_FILE) {
            open my $version_file, '<', NQP_VERSION_FILE;
            my $old = <$version_file>;
            close $version_file;
            chomp($old);
            exit if $old eq $digest; # no change - nothing to do
        }
        open my $version_file, '>', NQP_VERSION_FILE;
        print { $version_file } $digest;
        close $version_file;
        exit;
    }
}

# vim: expandtab sw=4
