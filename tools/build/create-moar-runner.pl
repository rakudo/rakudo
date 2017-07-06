#! perl6
# Copyright (C) 2013-2017, The Perl Foundation.

use v6;
my ($moar, $mbc, $install_to, $p6_mbc_path, $toolchain, $blib, @libpaths) = @*ARGS;

$p6_mbc_path = $*SPEC.rel2abs($p6_mbc_path || $*SPEC.curdir);
$blib = ' ' ~ $blib if $blib;
my $libpaths = '--libpath="%s"'.sprintf: @libpaths.join('" --libpath="');
my $libpath-line = '%s %s/%s%s'.sprintf: $libpaths, $p6_mbc_path, $mbc, $blibx;
if $*DISTRO eq 'mswin32' {
    exit if $toolchain;
    $install_to ~= '.bat';
    my $fh = open $install_to, :w;
    $fh.print(sprintf(qq[@ "%s" --execname="%%\~dpf0" %s %s\\%s%s %%*\n],
            $moar, $libpaths, $p6_mbc_path, $mbc, $blib));
    $fh.close;
}
elsif $toolchain eq any('gdb','lldb') {
    my $fh = open $install_to, :w;
    my $cmdline = $toolchain eq 'gdb'  ?? 'gdb --quiet --ex=run --args %s --execname="$0" %s "$@"'.sprintf($moar, $libpath-line)
               !! $toolchain eq 'lldb' ?? 'lldb %s -- --execname="$0" %s "$@"'.sprintf($moar, $libpath-line) !! die;
    my $debugger-name = $toolchain eq 'gdb'  ?? 'GNU'
                     !! $toolchain eq 'lldb' ?? 'LLVM' !! die;
    $fh.print(sprintf(q:to/EOS/, $moar, $libpath-line, $debugger-name, $cmdline));
#!/bin/sh
%s --execname="$0" %s -e '
say "=" x 96;

say "This is Rakudo Perl 6 running in the %s debugger, which often allows the user to generate useful back-\ntraces to debug or report issues in Rakudo, the MoarVM backend or the currently running code.\n";

unless $*VM.config<ccdebugflags> { say "The currently used MoarVM backend is not compiled with debugging symbols, you might want to\nreconfigure and reinstall MoarVM with --debug enabled.\n" }

say "This Rakudo version is $*PERL.compiler.version() built on MoarVM version $*VM.version(),";
say "running on $*DISTRO.gist() / $*KERNEL.gist()\n";

say "Type `bt full` to generate a backtrace if applicable, type `q` to quit or `help` for help.";

say "-" x 96;'
%s
EOS
    $fh.close;
    chmod(0o755, $install_to);
}
elsif ($toolchain eq 'valgrind') {
    my $fh = open $install_to, :w;
    $fh.print(sprintf(q:to/EOS/, ($moar, $libpath-line) xx 2));
#!/bin/sh
%s --execname="$0" %s -e '
say "=" x 96;

say "This is Rakudo Perl 6 running in valgrind, a tool for debugging and profiling programs.\nRunning a program in valgrind usually takes *a lot* more time than running it directly,\nso please be patient.";

say "This Rakudo version is $*PERL.compiler.version() built on MoarVM version $*VM.version(),";
say "running on $*DISTRO.gist() / $*KERNEL.gist()";

say "-" x 96;'
valgrind %s --full-cleanup --execname="$0" %s "$@"
EOS
    $fh.close;
    chmod(0o755, $install_to);
}
else {
    my $fh = open $install_to, :w;
    $fh.print(sprintf(q:to/EOS/, $moar, $libpath-line));
#!/bin/sh
exec %s  --execname="$0" %s "$@"
EOS
    $fh.close;
    chmod(0o755, $install_to);
}
