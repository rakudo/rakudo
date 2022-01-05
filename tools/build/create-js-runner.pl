#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;

my $bat   = $^O eq 'MSWin32' ? '.bat' : '';

my $preamble = $^O eq 'MSWin32' ? '@' : "#!/bin/sh
exec ";
my $postamble = $^O eq 'MSWin32' ? ' %*' : ' "$@"';

my $install_to = "perl6-js$bat";
open my $fh, ">", $install_to or die "open: $!";
print $fh $preamble, "node perl6.js -Ilib --source-map", $postamble, "\n" or die "print: $!";
close $fh or die "close: $!";

chmod 0755, $install_to if $^O ne 'MSWin32';

# vim: expandtab sw=4
