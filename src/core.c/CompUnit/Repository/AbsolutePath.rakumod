class CompUnit::Repository::AbsolutePath does CompUnit::Repository {
    has $!lock;
    has $!loaded;

    method TWEAK(--> Nil) {
        $!loaded := nqp::hash;
        $!lock   := Lock.new;
    }

    method need(CompUnit::Repository::AbsolutePath:D:
      CompUnit::DependencySpecification $spec,
      CompUnit::PrecompilationRepository $precomp = self.precomp-repository()
    --> CompUnit:D) {
        (my $repo := self.next-repo)
          ?? $repo.need($spec, $precomp)
          !! X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw
    }

    method load(CompUnit::Repository::AbsolutePath:D:
      IO::Path:D $file
    --> CompUnit:D) {
        if $file.is-absolute && $file.f {

            # we have a $file when we hit: require "PATH"
            # or use/require Foo:file<PATH>;
            my $key := $file.Str;
            $!lock.protect: {
                nqp::ifnull(
                  nqp::atkey($!loaded,$key),
                  nqp::stmts(
                    (my $precompiled :=
                      $key.ends-with(Rakudo::Internals.PRECOMP-EXT)),
                    nqp::bindkey($!loaded,$key,CompUnit.new(
                      :handle($precompiled
                        ?? CompUnit::Loader.load-precompilation-file($file)
                        !! CompUnit::Loader.load-source-file($file)
                      ),
                      :short-name($key),
                      :repo(self),
                      :repo-id($key),
                      :$precompiled
                    ))
                  )
                )
            }
        }
        elsif self.next-repo -> $repo {
            $repo.load($file)
        }
        else {
            X::NotFoundInRepository.new(:$file).throw;
        }
    }

    method loaded(CompUnit::Repository::AbsolutePath:D: --> Iterable:D) {
        my $loaded := $!lock.protect: { nqp::clone($!loaded) }
        nqp::p6bindattrinvres(
          nqp::create(Map),Map,'$!storage',$loaded
        ).values
    }

    method id(--> Str:D) { 'ap' }

    method path-spec(--> Str:D) { 'ap#' }

    multi method gist(CompUnit::Repository::AbsolutePath:D:--> Str:D) {
        self.path-spec
    }
}

# vim: expandtab shiftwidth=4
