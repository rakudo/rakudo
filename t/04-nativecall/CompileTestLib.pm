unit module CompileTestLib;

my @cleanup;  # files to be cleaned up afterwards

sub compile_test_lib($name) is export {
    my ($c_line, $l_line);
    my $VM  := $*VM;
    my $cfg := $VM.config;
    if $VM.name eq 'moar' {
        my $o  = $cfg<obj>;
        my $so = $cfg<dll>;
        $so ~~ s/^.*\%s//;
        $c_line = "$cfg<cc> -c $cfg<ccshared> $cfg<ccout>$name$o $cfg<cflags> t/04-nativecall/$name.c";
        $l_line = "$cfg<ld> $cfg<ldshared> $cfg<ldflags> " ~
            "$cfg<ldlibs> $cfg<ldout>$name$so $name$o";
        @cleanup = << "$name$so" "$name$o" >>;
    }
    elsif $VM.name eq 'jvm' {
        $c_line = "$cfg<nativecall.cc> -c $cfg<nativecall.ccdlflags> -o$name$cfg<nativecall.o> $cfg<nativecall.ccflags> t/04-nativecall/$name.c";
        $l_line = "$cfg<nativecall.ld> $cfg<nativecall.perllibs> $cfg<nativecall.lddlflags> $cfg<nativecall.ldflags> $cfg<nativecall.ldout>$name.$cfg<nativecall.so> $name$cfg<nativecall.o>";
        @cleanup = << "$name.$cfg<nativecall.so>" "$name$cfg<nativecall.o>" >>;
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
    my $so   = $VM.name eq 'moar'
            ?? $cfg<dll>.subst(/^.*\./, '')
            !! $cfg<nativecall.so>;
    @cleanup = "$name.$so";
    if $*DISTRO.is-win {
        @cmds    = "cl /LD /EHsc /Fe$name.$so t/04-nativecall/$name.cpp",
                   "g++ --shared -fPIC -o $name.$so t/04-nativecall/$name.cpp",
    }
    else {
        @cmds    = "g++ --shared -fPIC -o $name.$so t/04-nativecall/$name.cpp",
                   "clang++ -stdlib=libc++ --shared -fPIC -o $name.$so t/04-nativecall/$name.cpp",
    }

    my @fails;
    for @cmds -> $cmd {
        my $handle = shell("$cmd 2>&1", :out);
        my $output = $handle.out.slurp-rest;
        if $handle.out.close.status {
            @fails.push: "Running '$cmd':\n$output"
        }
        else {
            last
        }
    }
    fail @fails.join('=' x 80 ~ "\n") if @fails;
}

END {
#    say "cleaning up @cleanup[]";
    #~ unlink @cleanup;
}
