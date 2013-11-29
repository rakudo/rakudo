#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;

my ($moar, $libpath) = @ARGV;

if ($^O eq 'MSWin32') {
    my $install_to = 'perl6-m.bat';    
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh q[@ "%s" --libpath="%s" perl6.moarvm %%*] . "\n",
            $moar, $libpath;
    close $fh
        or die "Could not close $install_to: $!";
}
else {
    my $install_to = 'perl6-m';
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh <<'EOS', $moar, $libpath;
#!/bin/sh
exec %s --libpath=%s perl6.moarvm "$@"
EOS
    close $fh
        or die "Could not close $install_to: $!";
    chmod 0755, $install_to;
}
