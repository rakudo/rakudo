class CompUnit::Repository::Unknown does CompUnit::Repository {
    has $.path-spec;
    has $.short-name;

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp?,
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new(
            self.repo-chain.map(*.precomp-store).grep(*.defined)
        ),
        --> CompUnit:D)
    {
        return $precomp
            ?? self.next-repo.need($spec, $precomp, :@precomp-stores)
            !! self.next-repo.need($spec, :@precomp-stores)
            if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method loaded() {
        []
    }

    method id() {
        $.path-spec
    }

    method Str() { self.^name ~ " $.short-name $.path-spec" }
}

# vim: expandtab shiftwidth=4
