role CompUnit::Repository {
    # Resolves a dependency specification to a concrete dependency. If the
    # dependency was not already loaded, loads it. Returns a CompUnit
    # object that represents the selected dependency. If there is no
    # matching dependency, throws X::CompUnit::UnsatisfiedDependency.
    method need(CompUnit::DependencySpecification $spec,
                # If we're first in the chain, our precomp repo is the chosen one.
                CompUnit::PrecompilationRepository $precomp = self.precomp-repository())
        returns CompUnit:D
        { ... }

    # Returns the CompUnit objects describing all of the compilation
    # units that have been loaded by this repository in the current
    # process.
    method loaded()
        returns Iterable
        { ... }

    method precomp-repository()
        returns CompUnit::PrecompilationRepository
        { CompUnit::PrecompilationRepository::None }
}

# vim: ft=perl6 expandtab sw=4
