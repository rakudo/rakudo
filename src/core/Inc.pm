{
    my @INC;
    my %CUSTOM_LIB;

#?if jvm
    my $pathsep := $*VM.properties<path.separator>;
#?endif
#?if !jvm
    my $pathsep := $*VM.config<osname> eq 'MSWin32' ?? ';' !! ':';
#?endif
    @INC.push(CompUnitRepo::Local::File.new(%*ENV<RAKUDOLIB>.split($pathsep))) if %*ENV<RAKUDOLIB>;
    @INC.push(CompUnitRepo::Local::File.new(%*ENV<PERL6LIB>.split($pathsep))) if %*ENV<PERL6LIB>;

#?if jvm
    for nqp::jvmclasspaths() -> $path {
        @INC.push(CompUnitRepo::Local::File.new($path)) if nqp::stat($path, nqp::const::STAT_ISDIR);
    }
#?endif

    my $prefix :=
#?if jvm
        $*VM.properties<perl6.prefix>
#?endif
#?if parrot
        $*VM.config<libdir> ~ $*VM.config<versiondir>
#?endif
#?if moar
        $*VM.config<prefix>
#?endif
         ~ '/languages/perl6';

    if "$prefix/share/libraries.json".IO.e {
        my $config = "$prefix/share/libraries.json".IO.e ?? from-json( slurp "$prefix/share/libraries.json" ) !! [];

        my @all_paths;
        sub make-cur($class, *@paths) {
            @all_paths.push: @paths;
            if $class eq 'CompUnitRepo::Local::File' {
                CompUnitRepo::Local::File.new(|@paths);
            }
            elsif $class eq 'CompUnitRepo::Local::Installation' {
                CompUnitRepo::Local::Installation.new(|@paths);
            }
            else {
                my $name = 'lib/' ~ $class.split('::').join('/') ~ '.pm';
                for @all_paths -> $path {
                    if "$path/$name".IO.e {
                        require "$path/$name";
                        return ::($class).new(|@paths);
                    }
                }
            }
        }

        for $config.list -> @group {
            my @cur_group;
            for @group>>.kv -> $class, $props {
                for $props.list -> $prop {
                    if $prop ~~ Associative {
                        my $cur = make-cur($class, $prop.value.flat);
                        @cur_group.push: $cur;
                        %CUSTOM_LIB{$prop.key} = $cur;
                    }
                    else {
                        @cur_group.push: make-cur($class, $prop.flat)
                    }
                }
            }
            @INC.push: +@cur_group > 1 ?? [@cur_group] !! @cur_group
        }
    }
    # There is no config file, so pick sane defaults.
    else {
        # XXX Various issues with this stuff on JVM
        my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');  # TEMPORARY
        my @cur_inst;
        try {
            my $home := %*ENV<HOME> // %*ENV<HOMEDRIVE> ~ %*ENV<HOMEPATH>;
            my $ver  := nqp::p6box_s(nqp::atkey($compiler, 'version'));
            @INC.push(CompUnitRepo::Local::File.new("$home/.perl6/$ver/lib"));
            @cur_inst.push(%CUSTOM_LIB<home> = CompUnitRepo::Local::Installation.new("$home/.perl6/$ver"));
        }
        @INC.push(CompUnitRepo::Local::File.new("$prefix/lib"));
        @INC.push(CompUnitRepo::Local::File.new("$prefix/vendor/lib"));
        @INC.push(CompUnitRepo::Local::File.new("$prefix/site/lib"));
        @cur_inst.push(%CUSTOM_LIB<perl>   = CompUnitRepo::Local::Installation.new($prefix));
        @cur_inst.push(%CUSTOM_LIB<vendor> = CompUnitRepo::Local::Installation.new("$prefix/vendor"));
        @cur_inst.push(%CUSTOM_LIB<site>   = CompUnitRepo::Local::Installation.new("$prefix/site"));
        @INC.push([@cur_inst]);
    }

    # TODO Allow to pass Repo class to -I as well as the path.
    my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
    if nqp::defined($I) {
        if nqp::islist($I) {
            my Mu $iter := nqp::iterator($I);
            @INC.unshift: CompUnitRepo::Local::File.new(nqp::p6box_s(nqp::shift($iter))) while $iter;
        }
        else {
            @INC.unshift: CompUnitRepo::Local::File.new(nqp::p6box_s($I));
        }
    }

    PROCESS::<@INC>        := @INC;
    PROCESS::<%CUSTOM_LIB> := %CUSTOM_LIB;

    nqp::bindhllsym('perl6', 'ModuleLoader', CompUnitRepo);
}

# vim: ft=perl6 expandtab sw=4
