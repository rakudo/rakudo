#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;
use File::Copy 'cp';

my $USAGE = "Usage: $0 <prefix> <nqp prefix> <third party jars>\n";

my ($prefix, $nqpprefix, $thirdpartyjars) = @ARGV
    or die $USAGE;

my $cpsep = $^O eq 'MSWin32' ? ';' : ':';
my $bat   = $^O eq 'MSWin32' ? '.bat' : '';

my $preamble = $^O eq 'MSWin32' ? '@' : '#!/bin/sh
exec ';
my $postamble = $^O eq 'MSWin32' ? ' %*' : ' "$@"';

sub install {
    my ($name, $command) = @_;

    my $install_to = File::Spec->catfile($prefix, "$name$bat");

    open my $fh, ">", $install_to or die "open: $!";
    print $fh $preamble, $command, $postamble, "\n" or die "print: $!";
    close $fh or die "close: $!";

    chmod 0755, $install_to if $^O ne 'MSWin32';
}

my $jopts = '-Xms100m -Xbootclasspath/a:' . $prefix . $cpsep . $thirdpartyjars . $cpsep . "rakudo-runtime.jar${cpsep}perl6.jar -cp " . $nqpprefix;

install "perl6", "java $jopts perl6";
install "perl6-jdb-server", "java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n $jopts perl6";
install "perl6-eval-server", "java $jopts org.perl6.nqp.runtime.EvalServer TESTTOKEN " . File::Spec->catfile($prefix,"perl6.class");
cp(File::Spec->catfile($nqpprefix,"eval-client.pl"), ".")
    or die "Couldn't copy 'eval-client.pl' into $nqpprefix: $!";
