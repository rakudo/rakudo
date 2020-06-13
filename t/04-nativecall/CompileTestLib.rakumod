unit module CompileTestLib;

my @cleanup;  # files to be cleaned up afterwards

sub compile_test_lib($name) is export {
    my ($c_line, $l_line);
    my $VM  := $*VM;
    my $cfg := $VM.config;
    my $libname = $VM.platform-library-name($name.IO);
    if $VM.name eq 'moar' {
        my $o  = $cfg<obj>;

        # MoarVM exposes exposes GNU make directives here, but we cannot pass this to gcc directly.
        my $ldshared = $cfg<ldshared>.subst(/'--out-implib,lib$(notdir $@).a'/, "--out-implib,$libname.a");

        $c_line = "$cfg<cc> -c $cfg<ccshared> $cfg<ccout>$name$o $cfg<cflags> t/04-nativecall/$name.c";
        $l_line = "$cfg<ld> $ldshared $cfg<ldflags> $cfg<ldlibs> $cfg<ldout>$libname $name$o";
        @cleanup = << "$libname" "$name$o" >>;
    }
    elsif $VM.name eq 'jvm' || $VM.name eq 'js' {
        $c_line = "$cfg<nativecall.cc> -c $cfg<nativecall.ccdlflags> -o$name$cfg<nativecall.o> $cfg<nativecall.ccflags> t/04-nativecall/$name.c";
        $l_line = "$cfg<nativecall.ld> $cfg<nativecall.perllibs> $cfg<nativecall.lddlflags> $cfg<nativecall.ldflags> $cfg<nativecall.ldout>$libname $name$cfg<nativecall.o>";
        @cleanup = << $libname "$name$cfg<nativecall.o>" >>;
    }
    else {
        die "Unknown VM; don't know how to compile test libraries";
    }
    shell($c_line);
    shell($l_line);
}

sub compile_cpp_test_lib($name) is export {
    my @cmds;
    my $VM  := $*VM;
    my $cfg := $VM.config;
    my $libname = $VM.platform-library-name($name.IO);
    @cleanup = $libname;
    if $*DISTRO.is-win {
        @cmds    = "cl /LD /EHsc /Fe$libname t/04-nativecall/$name.cpp",
                   "g++ --shared -fPIC -o $libname t/04-nativecall/$name.cpp",
    }
    else {
        @cmds    = "g++ --shared -fPIC -o $libname t/04-nativecall/$name.cpp",
                   "clang++ -stdlib=libc++ --shared -fPIC -o $libname t/04-nativecall/$name.cpp",
    }

    my (@fails, $succeeded);
    for @cmds -> $cmd {
        my $handle = shell("$cmd 2>&1", :out);
        my $output = $handle.out.slurp-rest;
        if $handle.out.close.status {
            @fails.push: "Running '$cmd':\n$output"
        }
        else {
            $succeeded = 1;
            last
        }
    }
    fail @fails.join('=' x 80 ~ "\n") unless $succeeded;
}

END {
#    say "cleaning up @cleanup[]";
    unlink @cleanup;
}

# vim: expandtab shiftwidth=4
