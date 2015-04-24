{
    my @INC;
    my %CUSTOM_LIB;

    my @all_paths;
    sub make-cur($class, $path) {
        @all_paths.push: $path;
        if nqp::istype($class,CompUnitRepo::Locally) {
            $class.new($path);
        }
        elsif $class eq 'CompUnitRepo::Local::File' {
            CompUnitRepo::Local::File.new($path);
        }
        elsif $class eq 'CompUnitRepo::Local::Installation' {
            CompUnitRepo::Local::Installation.new($path);
        }
        else {
            my $name = 'lib/' ~ $class.split('::').join('/') ~ '.pm';
            for @all_paths -> $previous_path {
                if "$previous_path/$name".IO.e {
                    require "$previous_path/$name";
                    return ::($class).new($path);
                }
            }
        }
    }

    # Add CompUnitRepos to @*INC and also to %*CUSTOM_LIB if they have a name.
    sub add-curs($line, $sep = ',') {
        my %options;
        if $line && $line ~~ /^
            [ $<class>=[ <.ident>+ % '::' ]
              [ ':'
                $<n>=\w+
                  <[ < ( [ ]> $<v>=<[\w-]>+ <[ > ) \] ]>
                { %options{$<n>} = $<v> }
              ]*
              '='
            ]?
            $<path>=.+
        $/ {
            my $class = $<class> ?? ~$<class> !! 'CompUnitRepo::Local::File';
            my @paths = $<path>.split($sep);
            for @paths -> $path {
                if make-cur($class, $path) -> $cur {
                    %CUSTOM_LIB{~%options<name>} = $cur if %options<name>;
                    @INC.push: $cur
                }
                elsif %options<name> {
                    %CUSTOM_LIB{~%options<name>} = $path; # prime it
                }
            }
        }
    }

    # starting up for creating precomp
    if %*ENV<RAKUDO_PRECOMP_WITH> -> \specs {
        for PARSE-INCLUDE-SPEC(specs) -> \spec {
            @INC.push: make-cur(spec[0],spec[1]);
        }
        $*VM;  # apparently we need $*VM to be populated for valid precomps
    }

    # normal start up
    else {
        my %ENV := %*ENV; # only look up environment once
        my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
        if nqp::defined($I) {
            if nqp::islist($I) {
                my Mu $iter := nqp::iterator($I);
                add-curs(nqp::p6box_s(nqp::shift($iter))) while $iter;
            }
            else {
                add-curs(nqp::p6box_s($I));
            }
        }

#?if !jvm
        if %ENV<RAKUDOLIB> || %ENV<PERL6LIB> {
            my $path-sep := $*DISTRO.path-sep;
            add-curs(%ENV<RAKUDOLIB>, $path-sep) if %ENV<RAKUDOLIB>;
            add-curs(%ENV<PERL6LIB>, $path-sep)  if %ENV<PERL6LIB>;
        }
#?endif
#?if jvm
        my $path-sep := $*DISTRO.path-sep;
        if %ENV<RAKUDOLIB> || %ENV<PERL6LIB> {
            add-curs(%ENV<RAKUDOLIB>, $path-sep) if %ENV<RAKUDOLIB>;
            add-curs(%ENV<PERL6LIB>, $path-sep)  if %ENV<PERL6LIB>;
        }
        for nqp::jvmclasspaths() -> $path {
            add-curs($path, $path-sep)
              if nqp::stat($path, nqp::const::STAT_ISDIR);
        }
#?endif

        my $prefix  := $*VM.prefix ~ '/share/perl6';
        my $abspath := "$prefix/share/libraries.json";
        if IO::Path.new-from-absolute-path($abspath).e {
            my $config = from-json( slurp $abspath );

            for $config.list -> @group {
                for @group>>.kv -> $class, $props {
                    for $props.list -> $prop {
                        if nqp::istype($prop,Associative) {
                            for $prop.value.flat -> $path {
                                if make-cur($class, $path) -> $cur {
                                    @INC.push: $cur;
                                    %CUSTOM_LIB{$prop.key} = $cur;
                                }
                                else {
                                    %CUSTOM_LIB{$prop.key} = $path; # prime it
                                }
                            }
                        }
                        else {
                            for $prop.flat -> $path {
                                if make-cur($class, $path) -> $cur {
                                    @INC.push: $cur;
                                }
                            }
                        }
                    }
                }
            }
        }
        # There is no config file, so pick sane defaults.
        else {
            # XXX Various issues with this stuff on JVM
            my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');  # TEMPORARY
            try {
                if %ENV<HOME>
                  // (%ENV<HOMEDRIVE> // '') ~ (%ENV<HOMEPATH> // '') -> $home {
                    my $ver := nqp::p6box_s(nqp::atkey($compiler, 'version'));
                    if CompUnitRepo::Local::File.new("$home/.perl6/$ver/lib") -> $cur {
                        @INC.push: $cur;
                    }
                    if CompUnitRepo::Local::Installation.new("$home/.perl6/$ver") -> $cur {
                        @INC.push: %CUSTOM_LIB<home> = $cur;
                    }
                    else {
                        %CUSTOM_LIB<home> = "$home/.perl6/$ver";  # prime it
                    }
                }
            }
            if CompUnitRepo::Local::File.new("$prefix/lib") -> $cur {
                @INC.push: $cur;
            }
            if CompUnitRepo::Local::File.new("$prefix/vendor/lib") -> $cur {
                @INC.push: $cur;
            }
            if CompUnitRepo::Local::File.new("$prefix/site/lib") -> $cur {
                @INC.push: $cur;
            }
            if CompUnitRepo::Local::Installation.new($prefix) -> $cur {
                @INC.push: %CUSTOM_LIB<perl> = $cur;
            }
            else {
                %CUSTOM_LIB<perl> = $prefix;  # prime it
            }
            if CompUnitRepo::Local::Installation.new("$prefix/vendor") -> $cur {
                @INC.push: %CUSTOM_LIB<vendor> = $cur;
            }
            else {
                %CUSTOM_LIB<vendor> = "$prefix/vendor";  # prime it
            }
            if CompUnitRepo::Local::Installation.new("$prefix/site") -> $cur {
                @INC.push: %CUSTOM_LIB<site> = $cur;
            }
            else {
                %CUSTOM_LIB<site> = "$prefix/site";  # prime it
            }
        }
    }

    PROCESS::<@INC>        := @INC;
    PROCESS::<%CUSTOM_LIB> := %CUSTOM_LIB;

    nqp::bindhllsym('perl6', 'ModuleLoader', CompUnitRepo);
}

# vim: ft=perl6 expandtab sw=4
