#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;
use File::Copy 'cp';

my $USAGE = "Usage: $0 <type> <destdir> <prefix> <nqp prefix> <blib> <third party jars>\n";

my ($type, $destdir, $prefix, $nqpprefix, $blib, $thirdpartyjars) = @ARGV
    or die $USAGE;

my $debugger = 0;
if ($type =~ /^(\w+)\-debug$/) {
    $type = $1;
    $debugger = 1;
}

die "Invalid target type $type" unless $type eq 'dev' || $type eq 'install';

my $cpsep = $^O eq 'MSWin32' ? ';' : ':';
my $bat   = $^O eq 'MSWin32' ? '.bat' : '';

my $nqpdir = File::Spec->catfile($nqpprefix, 'share', 'nqp');
my $nqplibdir = $^O eq 'MSWin32' ? File::Spec->catfile($nqpdir, 'lib') : File::Spec->catfile('${NQP_DIR}', 'lib');
my $nqpjars = $^O eq 'MSWin32' ? $thirdpartyjars : join( $cpsep, map { $_ =~ s,$nqpdir,\${NQP_DIR},g; $_ } split($cpsep, $thirdpartyjars) );
my $bindir = $type eq 'install' ? File::Spec->catfile($prefix, 'bin') : $prefix;
my $perl6dir = $type eq 'install' ? File::Spec->catfile($prefix, 'share', 'perl6') : $prefix;
my $jardir = $type eq 'install' ? File::Spec->catfile($^O eq 'MSWin32' ? $perl6dir : '${PERL6_DIR}', 'runtime') : $prefix;
my $libdir = $type eq 'install' ? File::Spec->catfile($^O eq 'MSWin32' ? $perl6dir : '${PERL6_DIR}', 'lib') : 'blib';
my $sharedir = File::Spec->catfile($prefix, 'share', 'perl6', 'site', 'lib');
my $perl6jars = join( $cpsep,
    $^O eq 'MSWin32' ? $nqpjars : '${NQP_JARS}',
    File::Spec->catfile($jardir, 'rakudo-runtime.jar'),
    File::Spec->catfile($jardir, $debugger ? 'perl6-debug.jar' : 'perl6.jar'));

my $preamble = $^O eq 'MSWin32' ? '@' : "#!/bin/sh
: \${NQP_DIR:=\"$nqpdir\"}
: \${NQP_JARS:=\"$nqpjars\"}
: \${PERL6_DIR:=\"$perl6dir\"}
: \${PERL6_JARS:=\"$perl6jars\"}
exec ";
my $postamble = $^O eq 'MSWin32' ? ' %*' : ' "$@"';

sub install {
    my ($name, $command) = @_;

    my $install_to = $destdir
        ? File::Spec->catfile($destdir, $bindir, "$name$bat")
        : File::Spec->catfile($bindir, "$name$bat");

    print "Creating '$install_to'\n";
    open my $fh, ">", $install_to or die "open: $!";
    print $fh $preamble, $command, $postamble, "\n" or die "print: $!";
    close $fh or die "close: $!";

    chmod 0755, $install_to if $^O ne 'MSWin32';
}

my $classpath = join($cpsep, ($perl6jars, $jardir, $libdir, $nqplibdir));
my $jopts = '-noverify -Xms100m'
          . ' -cp ' . ($^O eq 'MSWin32' ? '"%CLASSPATH%";' : '$CLASSPATH:') . $classpath
          . ' -Dperl6.prefix=' . $prefix
          . ' -Djna.library.path=' . $sharedir
          . ($^O eq 'MSWin32' ? ' -Dperl6.execname="%~dpf0"' : ' -Dperl6.execname="$0"');
my $jdbopts = '-Xdebug -Xrunjdwp:transport=dt_socket,address=' 
            . ($^O eq 'MSWin32' ? '8000' : '${RAKUDO_JDB_PORT:=8000}') 
            . ',server=y,suspend=y';

if ($debugger) {
    install "perl6-debug-j", "java $jopts perl6-debug";
}
else {
    install "perl6-j", "java $jopts perl6 $blib";
    install "perl6-jdb-server", "java $jdbopts $jopts perl6 $blib";
    install "perl6-eval-server", "java -Xmx3000m -XX:MaxPermSize=200m $jopts org.perl6.nqp.tools.EvalServer";
}
