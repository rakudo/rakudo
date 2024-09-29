role CompUnit::Repository {
    has CompUnit::Repository $.next-repo is rw;

    # Resolves a dependency specification to a concrete dependency. If the
    # dependency was not already loaded, loads it. Returns a CompUnit
    # object that represents the selected dependency. If there is no
    # matching dependency, throws X::CompUnit::UnsatisfiedDependency.
    method need(CompUnit::DependencySpecification $spec,
                # If we're first in the chain, our precomp repo is the chosen one.
                CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
                CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new($precomp.store)
                --> CompUnit:D)
        { ... }

    # Resolves a dependency specification to a concrete dependency.
    # Returns a CompUnit object that represents the selected dependency.
    # If there is no matching dependency, Nil is returned.
    method resolve(CompUnit::DependencySpecification $spec --> CompUnit:D)
    {
        self.next-repo
          ?? self.next-repo.resolve($spec)
          !! Nil
    }

    # Just load the file and return a CompUnit object representing it.
    method load(IO::Path:D $file --> CompUnit:D)
    {
        self.next-repo
          ?? self.next-repo.load($file)
          !! X::NotFoundInRepository.new(:$file).throw;
    }

    # Returns the CompUnit objects describing all of the compilation
    # units that have been loaded by this repository in the current
    # process.
    method loaded(--> Iterable:D)
        { ... }

    # Returns a unique ID of this repository
    method id(--> Str:D)
        { ... }

    method precomp-store(--> CompUnit::PrecompilationStore)
        { CompUnit::PrecompilationStore }

    method precomp-repository(--> CompUnit::PrecompilationRepository)
        { CompUnit::PrecompilationRepository::None }

    method repo-chain() {
        my $buffer := nqp::create(IterationBuffer);
        nqp::push($buffer,my $repo := self);
        nqp::while(
          ($repo := $repo.next-repo).defined,
          nqp::push($buffer,$repo)
        );
        $buffer.List
    }
}

# vim: expandtab shiftwidth=4
