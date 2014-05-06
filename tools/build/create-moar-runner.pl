#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;

my ($moar, $mbc, $install_to, $p6_mbc_path, @libpaths) = @ARGV;
$p6_mbc_path = File::Spec->rel2abs($p6_mbc_path || '.');

if ($^O eq 'MSWin32') {
    $install_to .= '.bat';
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh q[@ "%s" --execname="%%~dpf0" --libpath="%s" %s\\%s %%*] . "\n",
            $moar, join('" --libpath="', @libpaths), $p6_mbc_path, $mbc;
    close $fh
        or die "Could not close $install_to: $!";
}
else {
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh <<'EOS', $moar, join('" --libpath="', @libpaths), $p6_mbc_path, $mbc;
#!/bin/sh
exec %s  --execname="$0" --libpath="%s" %s/%s "$@"
EOS
    close $fh
        or die "Could not close $install_to: $!";
    chmod 0755, $install_to;
}
