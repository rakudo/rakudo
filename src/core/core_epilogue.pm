# Re-parent meta-objects so they appear to be under Any.
BEGIN {
    Perl6::Metamodel::ClassHOW.HOW.reparent(Perl6::Metamodel::ClassHOW, Any);
    Perl6::Metamodel::ConcreteRoleHOW.HOW.reparent(Perl6::Metamodel::ConcreteRoleHOW, Any);
    Perl6::Metamodel::CurriedRoleHOW.HOW.reparent(Perl6::Metamodel::CurriedRoleHOW, Any);
    Perl6::Metamodel::EnumHOW.HOW.reparent(Perl6::Metamodel::EnumHOW, Any);
    Perl6::Metamodel::GenericHOW.HOW.reparent(Perl6::Metamodel::GenericHOW, Any);
    Perl6::Metamodel::ModuleHOW.HOW.reparent(Perl6::Metamodel::ModuleHOW, Any);
    Perl6::Metamodel::NativeHOW.HOW.reparent(Perl6::Metamodel::NativeHOW, Any);
    Perl6::Metamodel::PackageHOW.HOW.reparent(Perl6::Metamodel::PackageHOW, Any);
    Perl6::Metamodel::ParametricRoleGroupHOW.HOW.reparent(Perl6::Metamodel::ParametricRoleGroupHOW, Any);
    Perl6::Metamodel::ParametricRoleHOW.HOW.reparent(Perl6::Metamodel::ParametricRoleHOW, Any);
    Perl6::Metamodel::SubsetHOW.HOW.reparent(Perl6::Metamodel::SubsetHOW, Any);
    Perl6::Metamodel::GrammarHOW.HOW.compose(Perl6::Metamodel::GrammarHOW);
}

my $prefix = ($*VM<properties><perl6.prefix> // $*VM<config><prefix>);
if "$prefix/share/libraries.cfg".IO.e {
    my @lines = slurp("$prefix/share/libraries.cfg").lines;
    my %repos;
    for %*COMPILING<%?OPTIONS><I>, @lines -> $repo {
        next if !$repo || $repo ~~ /^ '#'/;
        my %options;
        if $repo ~~ / $<class>=[ <.ident>+ % '::' ] [ ':' $<n>=\w+ <[<([]> $<v>=<[\w-]>+ <[>)\]]> { %options{$<n>} = $<v> } ]* '=' $<path>=.+ / {
            my $name = %options<name> // '';
            my $prio = %options<prio> // 0;
            %repos{~$<class>}{$name}{$prio} //= [];
            my $env_sep = $*OS eq 'MSWin32' ?? ';' !! ':';
            my @path = $<path>.split($env_sep);
            for @path {
                %repos{~$<class>}{$name}{$prio}.push(.IO.path.is-relative ?? "$prefix/share/$_" !! .Str);
            }
        }
    }
    my @custom_libs = %*CUSTOM_LIB.values;
    for %repos.kv -> $classname, $libnames {
        for $libnames.kv -> $name, $prios {
            for $prios.kv -> $prio, $args {
                if $classname eq 'CompUnitRepo::Local::File' {
                    CompUnitRepo.add_repo( CompUnitRepo::Local::File.new(|@$args), :$name, :$prio );
                }
                elsif $classname eq 'CompUnitRepo::Local::Installation' {
                    CompUnitRepo.add_repo( CompUnitRepo::Local::Installation.new(|@$args), :$name, :$prio );
                }
                else {
                    my $name = 'lib/' ~ $classname.split('::').join('/') ~ '.pm';
                    for @custom_libs -> $path {
                        if "$path/$name".IO.e {
                            require "$path/$name";
                            CompUnitRepo.add_repo( ::($classname).new(|@$args), :$name, :$prio );
                        }
                    }
                }
            }
        }
    }
}

nqp::bindhllsym('perl6', 'ModuleLoader', CompUnitRepo);

{YOU_ARE_HERE}
