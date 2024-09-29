class CompUnit::Repository::NQP does CompUnit::Repository {
    my constant %opts = :from<NQP>,;  # comma needed to make a Map

    method need(CompUnit::Repository::NQP:D:
      CompUnit::DependencySpecification $spec,
      CompUnit::PrecompilationRepository $precomp?,
    --> CompUnit:D) {
        if $spec.from eq 'NQP' {
            my $key := $spec.short-name;
            CompUnit.new:
              :short-name($key),
              :handle(CompUnit::Handle.new(
                 nqp::gethllsym('Raku','ModuleLoader').load_module($key, %opts)
              )),
              :repo(self),
              :repo-id($key),
              :from<NQP>;
        }
        elsif self.next-repo -> $repo {
            $repo.need($spec, $precomp // self.precomp-repository)
        }
        else {
            X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
        }
    }

    method loaded(CompUnit::Repository::NQP:D: --> Empty) { }

    method id(--> Str:D) { 'NQP' }

    method path-spec(--> Str:D) { 'nqp#' }

    multi method gist(CompUnit::Repository::NQP:D: --> Str:D) {
        self.path-spec
    }
}

# vim: expandtab shiftwidth=4
