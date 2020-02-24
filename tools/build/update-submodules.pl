#!/usr/bin/env perl
# Copyright (C) 2009-2019 The Perl Foundation

use 5.10.1;
use strict;
use warnings;
use Getopt::Long;
use Cwd;
use File::Spec;

my $msg;

my $repo = shift @ARGV;
chdir $repo;

exit 0 if !-d '.git';

my $git_cache_dir;
Getopt::Long::Configure("pass_through");
Getopt::Long::GetOptions('git-cache-dir=s' => \$git_cache_dir);

print 'Updating submodules .................................... ';

exec_and_check('git submodule sync --quiet 2>&1', 'Submodule sync failed for an unknown reason.');
exec_and_check('git submodule --quiet init 2>&1', 'Submodule init failed for an unknown reason.');

if ($git_cache_dir) {
    my $out = qx{git submodule status 2>&1};
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
        my $url = qx{git config submodule.$smodpath.url 2>&1};
        chomp $url;
        if (!$url) {
            print "Couldn't retrieve submodule URL for submodule $smodname\n";
            exit 1;
        }
        if (!-e $modrefdir) {
            exec_and_check("git clone --quiet --bare $url $modrefdir", "Got clone of $url failed.");
        }
        else {
            my $back = Cwd::cwd();
            chdir $modrefdir;
            exec_and_check('git fetch --quiet --all', "Got fetch in $modrefdir failed.");
            chdir $back;
        }
        $msg = qx{git submodule --quiet update --reference "$modrefdir" $smodpath 2>&1};
        check_update_ok($?, $msg);
    }
}
else {
    $msg = qx{git submodule --quiet update 2>&1};
    check_update_ok($?, $msg);
}

print "OK\n";


# Helper subs.

sub exec_and_check {
    my ($command, $msg) = @_;
    my $out = qx{$command};
    if ($? >> 8 != 0) {
        print "\n===SORRY=== ERROR: $msg\n";
        print "The error message was: $out\n";
        exit 1;
    }
}

sub check_update_ok {
    my ($code, $msg) = @_;
    if ($code >> 8 != 0) {
        if ( $msg =~
            /[']([^']+)[']\s+already exists and is not an empty/ )
        {
            print "\n===SORRY=== ERROR: "
              . "Cannot update submodule because directory exists and is not empty.\n"
              . ">>> Please delete the following folder and try again:\n$1\n\n";
        }
        else {
            print "\n===SORRY=== ERROR: "
              . "Updating the submodule failed for an unknown reason. The error message was:\n"
              . $msg;
        }
        exit 1;
    }
}

