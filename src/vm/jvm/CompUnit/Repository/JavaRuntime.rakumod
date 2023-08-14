class CompUnit::Repository::JavaRuntime does CompUnit::Repository {
    has $!interop;

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
    )
        returns CompUnit:D
    {
        if $spec.from eq 'JavaRuntime' {
            $!interop = nqp::jvmrakudointerop() unless nqp::isconcrete($!interop);

            my $jtype = $!interop.typeForName($spec.short-name.subst(/'::'/, '.', :g));

            # register the class by its name (cf. Inline::Perl5, nine++)
            my @parts = $spec.short-name.split('::');
            my $inner = @parts.pop;
            my $ns := ::GLOBAL.WHO;
            for @parts {
                $ns{$_} := Metamodel::PackageHOW.new_type(name => $_) unless $ns{$_}:exists;
                $ns := $ns{$_}.WHO;
            }
            my @existing = $ns{$inner}.WHO.pairs;
            $ns{$inner} := $jtype;
            # $jtype.WHO{$_.key} := $_.value for @existing;

            nqp::setwho(::($spec.short-name), Stash.new());
            ::($spec.short-name).WHO<EXPORT> := Metamodel::PackageHOW.new();
            ::($spec.short-name).WHO<&EXPORT> := sub EXPORT(*@args) {
                Map.new($inner => nqp::decont($jtype));
            }

            return CompUnit.new(
                :short-name($spec.short-name),
                :handle(CompUnit::Handle.from-unit(::($spec.short-name).WHO)),
                :repo(self),
                :repo-id($spec.short-name),
                :from($spec.from),
            );
        }

        return self.next-repo.need($spec, $precomp) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method loaded() {
        []
    }

    method id() {
        'JavaRuntime'
    }

    method path-spec() {
        'javart#'
    }
}

# vim: expandtab sw=4
