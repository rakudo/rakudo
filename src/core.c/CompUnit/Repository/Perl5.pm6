class CompUnit::Repository::Perl5 does CompUnit::Repository {
    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
        --> CompUnit:D)
    {
        if $spec.from eq 'Perl5' {
            my $compunit = $*REPO.need(CompUnit::DependencySpecification.new(:short-name<Inline::Perl5>));
            my $perl5 := $compunit.handle.globalish-package<Inline>.WHO<Perl5>.default_perl5;

            if $*RAKUDO_MODULE_DEBUG -> $RMD {
                $RMD("Loading {$spec.short-name} via Inline::Perl5");
            }
            my $handle := $perl5.require(
                $spec.short-name,
                $spec.version-matcher !== True ?? $spec.version-matcher.Num !! Num,
                :handle
            );
            return CompUnit.new(
                :short-name($spec.short-name),
                :$handle,
                :repo(self),
                :repo-id($spec.short-name),
                :from($spec.from),
            );

            CATCH {
                when X::CompUnit::UnsatisfiedDependency {
                    X::NYI::Available.new(:available('Inline::Perl5'), :feature('Perl 5')).throw;
                }
            }
        }

        return self.next-repo.need($spec, $precomp) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method loaded() {
        []
    }

    method id() {
        'Perl5'
    }

    method path-spec() {
        'perl5#'
    }

    multi method gist(CompUnit::Repository::Perl5:D:) {
        self.path-spec
    }
}

# vim: expandtab shiftwidth=4
