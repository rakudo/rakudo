#! perl6
# Copyright (C) 2013-2018, The Perl Foundation.

use v6;

multi sub MAIN("perl6", $perl6, $install-to, $toolchain, $ld-lib-path, $perl6-home, $nqp-home) {
    exit if $*DISTRO eq 'mswin32';

    my $env-vars = '';
    $env-vars ~= "LD_LIBRARY_PATH=$ld-lib-path " if $ld-lib-path;
    $env-vars ~= "PERL6_HOME=$perl6-home " if $perl6-home;
    $env-vars ~= "NQP_HOME=$nqp-home " if $nqp-home;

    my $fh = open $install-to, :w;

    if $toolchain eq any('gdb','lldb') {
        $fh.print(get-perl6-debug-runner($toolchain, $perl6, $env-vars));
    }
    elsif ($toolchain eq 'valgrind') {
        $fh.print(get-perl6-valgrind-runner($perl6, $env-vars));
    }
    else {
        $fh.print(get-perl6-runner($perl6, $env-vars));
    }

    $fh.close;

    chmod(0o755, $install-to) if $*DISTRO ne 'mswin32';
}

multi sub MAIN("moar", $moar, $install-to is copy, $mbc, $p6-mbc-path, $toolchain, $perl6-home is copy, $nqp-home is copy, $blib is copy, *@libpaths) {
    $perl6-home = "PERL6_HOME=$perl6-home" if $perl6-home;
    $nqp-home = "NQP_HOME=$nqp-home" if $nqp-home;
    my $env-vars = join(' ', $nqp-home, $perl6-home).trim;

    $blib = ' ' ~ $blib if $blib;
    my $libpaths = '--libpath="%s"'.sprintf: @libpaths.join('" --libpath="');
    my $libpath-line = '%s %s/%s%s'.sprintf: $libpaths, $p6-mbc-path, $mbc, $blib;
    
    $install-to ~= '.bat' if $*DISTRO eq 'mswin32';

    my $fh = open $install-to, :w;

    if $*DISTRO eq 'mswin32' {
        $fh.print(get-moar-win-runner($moar, $libpaths, $p6-mbc-path, $mbc, $blib));
    }
    elsif $toolchain eq any('gdb','lldb') {
        $fh.print(get-moar-debug-runner($toolchain, $moar, $libpath-line));
    }
    elsif ($toolchain eq 'valgrind') {
        $fh.print(get-moar-valgrind-runner($moar, $libpath-line));
    }
    else {
        $fh.print(get-moar-runner($moar, $libpath-line));
    }

    $fh.close;

    chmod(0o755, $install-to) if $*DISTRO ne 'mswin32';
}

my $bash-prelude = q:to/EOS/;
    #!/bin/bash

    # Sourced from https://stackoverflow.com/a/246128/1975049
    SOURCE="${BASH_SOURCE[0]}"
    while [ -h "$SOURCE" ]; do
      DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"
      SOURCE="$(readlink "$SOURCE")"
      [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
    done
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"
    EOS

sub get-moar-win-runner($moar, $libpaths, $p6-mbc-path, $mbc, $blib) {
    return sprintf(qq[@ "%s" --execname="%%\~dpf0" %s %s\\%s%s %%*\n],
            $moar, $libpaths, $p6-mbc-path, $mbc, $blib);
}

sub get-perl6-runner($perl6, $env-vars) {
    return sprintf(q:to/EOS/, $bash-prelude, $env-vars, $perl6);
    %s

    %sexec $DIR/%s "$@"
    EOS
}

sub get-moar-runner($moar, $libpath-line) {
    return sprintf(q:to/EOS/, $moar, $libpath-line);
    #!/bin/sh
    exec %s --execname="$0" %s "$@"
    EOS
}

sub get-debugger-text($toolchain) {
    my $debugger-name = $toolchain eq 'gdb'  ?? 'GNU'
                     !! $toolchain eq 'lldb' ?? 'LLVM' !! die;
    return sprintf(q:to/EOS/, $debugger-name);
    say "=" x 96;

    say "This is Rakudo Perl 6 running in the %s debugger, which often allows the user to generate useful back-\ntraces to debug or report issues in Rakudo, the MoarVM backend or the currently running code.\n";

    unless $*VM.config<ccdebugflags> { say "The currently used MoarVM backend is not compiled with debugging symbols, you might want to\nreconfigure and reinstall MoarVM with --debug enabled.\n" }

    say "This Rakudo version is $*PERL.compiler.version() built on MoarVM version $*VM.version(),";
    say "running on $*DISTRO.gist() / $*KERNEL.gist()\n";

    say "Type `bt full` to generate a backtrace if applicable, type `q` to quit or `help` for help.";

    say "-" x 96;
    EOS
}

sub get-perl6-debug-runner($toolchain, $perl6, $env-vars) {
    my $cmdline = $toolchain eq 'gdb'  ?? '%sgdb --quiet --ex=run --args %s "$@"'.sprintf($env-vars, $perl6)
               !! $toolchain eq 'lldb' ?? '%slldb %s -- "$@"'.sprintf($env-vars, $perl6) !! die;
    return sprintf(q:to/EOS/, $bash-prelude, $env-vars, $perl6, get-debugger-text($toolchain), $cmdline);
    %s

    %s$DIR/%s -e '%s'
    %s
    EOS
}

sub get-moar-debug-runner($toolchain, $moar, $libpath-line) {
    my $cmdline = $toolchain eq 'gdb'  ?? 'gdb --quiet --ex=run --args %s --execname="$0" %s "$@"'.sprintf($moar, $libpath-line)
               !! $toolchain eq 'lldb' ?? 'lldb %s -- --execname="$0" %s "$@"'.sprintf($moar, $libpath-line) !! die;
    return sprintf(q:to/EOS/, $moar, $libpath-line, get-debugger-text($toolchain), $cmdline);
    #!/bin/sh
    %s --execname="$0" %s -e '%s'
    %s
    EOS
}


my $valgrind-text = q:to/EOS/;
    say "=" x 96;

    say qq:to/END/;
    This is Rakudo Perl 6 running in valgrind, a tool for debugging and profiling programs.
    Running a program in valgrind usually takes *a lot* more time than running it directly,
    so please be patient.
    Valgrind options can be added with MVM_VALGRIND_OPTS environment variable.
    END
    say "This Rakudo version is $*PERL.compiler.version() built on MoarVM version $*VM.version(),";
    say "running on $*DISTRO.gist() / $*KERNEL.gist()";

    say "-" x 96;
    EOS

sub get-perl6-valgrind-runner($perl6, $env-vars) {
    return sprintf(q:to/EOS/, $bash-prelude, $env-vars, $perl6, $valgrind-text, $env-vars, $perl6);
    %s

    %s$DIR/%s -e '%s'
    %svalgrind ${MVM_VALGRIND_OPTS} $DIR/%s "$@"
    EOS
}

sub get-moar-valgrind-runner($moar, $libpath-line) {
    return sprintf(q:to/EOS/, $moar, $libpath-line, $valgrind-text, $moar, $libpath-line);
    #!/bin/sh
    %s --execname="$0" %s -e '%s'
    valgrind ${MVM_VALGRIND_OPTS} %s --execname="$0" %s "$@"
    EOS
}

