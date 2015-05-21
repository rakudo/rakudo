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

END {
#    say "cleaning up @cleanup[]";
    unlink @cleanup;
}
