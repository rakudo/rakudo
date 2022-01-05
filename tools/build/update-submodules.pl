#!/usr/bin/env perl
# Copyright (C) 2009-2021 The Perl Foundation

use 5.10.1;
use strict;
use warnings;
use Getopt::Long;
use Cwd;
use File::Spec;

my $repo = shift @ARGV;
chdir $repo;

exit 0 if !-d '.git';

my $git_cache_dir;
Getopt::Long::Configure("pass_through");
Getopt::Long::GetOptions('git-cache-dir=s' => \$git_cache_dir);

print 'Updating submodules .................................... ';

exec_and_check('git', 'submodule', 'sync', '--quiet', 'Submodule sync failed for an unknown reason.');
exec_and_check('git', 'submodule', '--quiet', 'init', 'Submodule init failed for an unknown reason.');

if ($git_cache_dir) {
    my $out = exec_with_output('git', 'submodule', 'status');
    if ($? >> 8 != 0) {
        print "\n===SORRY=== ERROR: Submodule status failed for an unknown reason.\n";
        print "The error message was: $out\n";
        exit 1;
    }
    for my $smodline (split(/^/m, $out)) {
        chomp $smodline;
        if ($smodline !~ /^.[0-9a-f]+ ([^ ]+)(?:$| )/) {
            print "\n===SORRY=== ERROR: "
              . "Submodule status output looks unexpected: '$smodline'";
            exit 1;
        }
        my $smodpath = $1;
        my $smodname = (File::Spec->splitdir($smodpath))[-1];
        my $modrefdir = File::Spec->catdir($git_cache_dir, $smodname);
        my $url = exec_with_output('git', 'config', "submodule.$smodpath.url");
        chomp $url;
        if (!$url) {
            print "Couldn't retrieve submodule URL for submodule $smodname\n";
            exit 1;
        }
        if (!-e $modrefdir) {
            exec_and_check('git', 'clone', '--quiet', '--bare', $url, $modrefdir, "Git clone of $url failed.");
        }
        else {
            my $back = Cwd::cwd();
            chdir $modrefdir;
            exec_and_check('git', 'fetch', '--quiet', '--all', "Git fetch in $modrefdir failed.");
            chdir $back;
        }
        exec_and_check('git', 'submodule', '--quiet', 'update', '--reference', $modrefdir, $smodpath, 'Git submodule update failed.');
    }
}
else {
    exec_and_check('git', 'submodule', '--quiet', 'update', 'Git submodule update failed.');
}

print "OK\n";


# Helper subs.

sub exec_with_output {
    my @command = @_;
    open(my $handle, '-|', @command);
    my $out;
    while(<$handle>) {
        $out .= $_;
    }
    close $handle;
    return $out;
}

sub exec_and_check {
    my $msg = pop;
    my @command = @_;
    my $out = exec_with_output(@command);
    if ($? >> 8 != 0) {
        print "\n===SORRY=== ERROR: $msg\n";
        print "The programs output was: $out\n";
        exit 1;
    }
}

# vim: expandtab sw=4
