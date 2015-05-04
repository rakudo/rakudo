{
    my @INC;
    my %CUSTOM_LIB;
    my %ENV := %*ENV; # only look up environment once

    # starting up for creating precomp
    if %ENV<RAKUDO_PRECOMP_WITH> -> \specs {

RAKUDO_MODULE_DEBUG("Init @INC with {specs}")
  if $?RAKUDO_MODULE_DEBUG;

        @INC = specs.split(','); # assume well formed strings
        $*VM;     # we need $*VM to be populated for valid precomps
        $*DISTRO; # we need $*DISTRO to be populated for valid precomps
    }

    # normal start up
    else {
        my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
        if nqp::defined($I) {
            if nqp::islist($I) {
                my Mu $iter := nqp::iterator($I);
                while $iter {
                    @INC.push: PARSE-INCLUDE-SPECS(nqp::shift($iter));
                }
            }
            else {
                @INC.push: PARSE-INCLUDE-SPECS($I);
            }
        }

        if %ENV<RAKUDOLIB> -> $rakudolib {
            @INC.push: PARSE-INCLUDE-SPECS($rakudolib);
        }
        if %ENV<PERL6LIB> -> $perl6lib {
            @INC.push: PARSE-INCLUDE-SPECS($perl6lib);
        }

#?if jvm
        for nqp::jvmclasspaths() -> $path {
            @INC.push: PARSE-INCLUDE-SPECS($path);
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
                                @INC.push: PARSE-INCLUDE-SPECS($path);
                                %CUSTOM_LIB{$prop.key} = $path;
                            }
                        }
                        else {
                            for $prop.flat -> $path {
                                @INC.push: PARSE-INCLUDE-SPECS($path);
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
                    my $path := "$home/.perl6/$ver";
                    @INC.push: "file#$path/lib", "inst#$path";
                    %CUSTOM_LIB<home> = $path;
                }
            }
            @INC.push:
              "file#$prefix/lib",
              "file#$prefix/vendor/lib",
              "file#$prefix/site/lib",
              "inst#$prefix",
              "inst#$prefix/vendor",
              "inst#$prefix/site";

            %CUSTOM_LIB<perl>   =  $prefix;
            %CUSTOM_LIB<vendor> = "$prefix/vendor";
            %CUSTOM_LIB<site>   = "$prefix/site";
        }
    }

    PROCESS::<@INC>        := @INC;
    PROCESS::<%CUSTOM_LIB> := %CUSTOM_LIB;

    nqp::bindhllsym('perl6', 'ModuleLoader', CompUnitRepo);
}

# vim: ft=perl6 expandtab sw=4
