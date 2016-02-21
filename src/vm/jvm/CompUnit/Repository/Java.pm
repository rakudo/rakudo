class CompUnit::Repository::Java does CompUnit::Repository {
    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
    )
        returns CompUnit:D
    {
        if $spec.from eq 'Java' {
            my $java := nqp::gethllsym('perl6', 'JavaModuleLoader');
            say "got hllsym JavaModuleLoader..." if %*ENV<RAKUDO_MODULE_DEBUG>;

            return CompUnit.new(
                :short-name($spec.short-name),
                :handle(CompUnit::Handle.new($java.load_module($spec.short-name, {:from<Java>}))),
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
        'Java'
    }

    method path-spec() {
        'java#'
    }
}

# vim: ft=perl6 expandtab sw=4
