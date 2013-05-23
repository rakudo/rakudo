#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;

my ($prefix, $nqpprefix, $thirdpartyjars) = @ARGV;

my $cpsep = $^O eq 'MSWin32' ? ';' : ':';
my $install_to = File::Spec->catfile($prefix, $^O eq 'MSWin32' ? 'perl6.bat' : 'perl6');

my $preamble = $^O eq 'MSWin32' ? '@' : '#!/bin/sh
exec ';
my $postamble = $^O eq 'MSWin32' ? '%*' : '"$@"';

open my $fh, ">", $install_to;
print $fh $preamble;
print $fh 'java -Xms100m -Xbootclasspath/a:' . $prefix . $cpsep . $thirdpartyjars . $cpsep . 'rakudo-runtime.jar'
        . ' -cp ' . $nqpprefix . ' perl6 ';
print $fh $postamble;
print $fh "\n";
close $fh;

chmod 0755, $install_to if $^O ne 'MSWin32';
