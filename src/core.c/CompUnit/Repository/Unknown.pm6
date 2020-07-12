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
        self.next-repo
          ?? $precomp
            ?? self.next-repo.need($spec, $precomp, :@precomp-stores)
            !! self.next-repo.need($spec, :@precomp-stores)
          !! Nil
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
