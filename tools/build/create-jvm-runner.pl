#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;
use File::Copy 'cp';

my $USAGE = "Usage: $0 <type> <prefix> <nqp prefix> <third party jars>\n";

my ($type, $prefix, $nqpprefix, $thirdpartyjars) = @ARGV
    or die $USAGE;

die "Invalid target type $type" unless $type eq 'dev' || $type eq 'install';

my $cpsep = $^O eq 'MSWin32' ? ';' : ':';
my $bat   = $^O eq 'MSWin32' ? '.bat' : '';

my $preamble = $^O eq 'MSWin32' ? '@' : '#!/bin/sh
exec ';
my $postamble = $^O eq 'MSWin32' ? ' %*' : ' "$@"';

my $bindir = $type eq 'install' ? File::Spec->catfile($prefix, 'bin') : $prefix;
my $jardir = $type eq 'install' ? File::Spec->catfile($prefix, 'languages', 'perl6', 'runtime') : $prefix;
my $libdir = $type eq 'install' ? File::Spec->catfile($prefix, 'languages', 'perl6', 'lib') : 'blib';

sub install {
    my ($name, $command) = @_;

    my $install_to = File::Spec->catfile($bindir, "$name$bat");

    open my $fh, ">", $install_to or die "open: $!";
    print $fh $preamble, $command, $postamble, "\n" or die "print: $!";
    close $fh or die "close: $!";

    chmod 0755, $install_to if $^O ne 'MSWin32';
}

my $bootclasspath = join($cpsep,
    ($thirdpartyjars,
    File::Spec->catfile($jardir, 'rakudo-runtime.jar'),
    File::Spec->catfile($jardir, 'perl6.jar'),
    $jardir,
    $libdir));
    
my $classpath = join($cpsep, ($jardir, $libdir, $nqpprefix));

my $jopts = '-Xms100m -Xbootclasspath/a:' . $bootclasspath . ' -cp ' . $classpath;

install "perl6", "java $jopts perl6";
install "perl6-jdb-server", "java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n $jopts perl6";
install "perl6-eval-server", "java $jopts org.perl6.nqp.tools.EvalServer";
cp(File::Spec->catfile($nqpprefix,"eval-client.pl"), ".")
    or die "Couldn't copy 'eval-client.pl' into $nqpprefix: $!";
