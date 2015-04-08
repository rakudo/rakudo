#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;
use File::Spec;

my ($moar, $mbc, $install_to, $p6_mbc_path, $toolchain, @libpaths) = @ARGV;
$p6_mbc_path = File::Spec->rel2abs($p6_mbc_path || '.');

if ($^O eq 'MSWin32') {
    return if $toolchain;
    $install_to .= '.bat';
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh q[@ "%s" --execname="%%~dpf0" --libpath="%s" %s\\%s %%*] . "\n",
            $moar, join('" --libpath="', @libpaths), $p6_mbc_path, $mbc;
    close $fh
        or die "Could not close $install_to: $!";
}
elsif ($toolchain eq 'gdb') {
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh <<'EOS', ($moar, join('" --libpath="', @libpaths), $p6_mbc_path, $mbc) x 2;
#!/bin/sh
%s --execname="$0" --libpath="%s" %s/%s -e '
say "=" x 96;

say "This is Rakudo Perl 6 running in the GNU debugger, which often allwos to generate useful back-\ntraces to debug or report issues in Rakudo, the MoarVM backend or the currently running code.\n";

if $*VM.config<ccdebugflags> { say "The currently used MoarVM backend is not compiled with debugging symbols, you might want to\nreconfigure and reinstall MoarVM with --debug enabled.\n" }

say "This Rakudo version is $*PERL.compiler.version() built on MoarVM version $*VM.version(),";
say "running on $*DISTRO.gist() / $*KERNEL.gist()\n";

say "Type `bt full` to generate a backtrace if applicable, type `q` to quite or `help` for help.";

say "-" x 96;'
gdb --quiet --ex=run --args %s --execname="$0" --libpath="%s" %s/%s "$@"
EOS
    close $fh
        or die "Could not close $install_to: $!";
    chmod 0755, $install_to;
}
elsif ($toolchain eq 'valgrind') {
    open my $fh, ">", $install_to
        or die "Could not open $install_to: $!";
    printf $fh <<'EOS', ($moar, join('" --libpath="', @libpaths), $p6_mbc_path, $mbc) x 2;
#!/bin/sh
%s --execname="$0" --libpath="%s" %s/%s -e '
say "=" x 96;

say "This is Rakudo Perl 6 running in valgrind, a tool for debugging and profiling programs.\nRunning a program in valgrind usually takes *a lot* more time than running it directly,\nso please be patient.";

say "This Rakudo version is $*PERL.compiler.version() built on MoarVM version $*VM.version(),";
say "running on $*DISTRO.gist() / $*KERNEL.gist()";

say "-" x 96;'
valgrind %s --execname="$0" --libpath="%s" %s/%s "$@"
EOS
    close $fh
        or die "Could not close $install_to: $!";
    chmod 0755, $install_to;
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
