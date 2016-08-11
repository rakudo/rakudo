class CompUnit::Repository::AbsolutePath does CompUnit::Repository {
    has %!loaded;

    method need(CompUnit::DependencySpecification $spec,
                CompUnit::PrecompilationRepository $precomp = self.precomp-repository())
        returns CompUnit:D
    {
        return self.next-repo.need($spec, $precomp) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method load(IO::Path:D $file) returns CompUnit:D {
        if $file.is-absolute {

            # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
            my $precompiled =
              $file.Str.ends-with(Rakudo::Internals.PRECOMP-EXT);

            if $file.f {
                return %!loaded{$file} = CompUnit.new(
                    :handle(
                        $precompiled
                            ?? CompUnit::Loader.load-precompilation-file($file)
                            !! CompUnit::Loader.load-source-file($file)
                    ),
                    :short-name($file.Str),
                    :repo(self),
                    :repo-id($file.Str),
                    :$precompiled,
                );
            }
        }

        return self.next-repo.load($file) if self.next-repo;
        die("Could not find $file in:\n" ~ $*REPO.repo-chain.quickmap(*.Str).join("\n").indent(4));
    }

    method loaded() returns Iterable {
        return %!loaded.values;
    }

    method id() {
        'ap'
    }

    method path-spec() {
        'ap#'
    }
}

# vim: ft=perl6 expandtab sw=4
