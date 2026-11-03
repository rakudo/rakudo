class CompUnit::Repository::Perl5 does CompUnit::Repository {
    my constant $perl5-dependency =
      CompUnit::DependencySpecification.new(:short-name<Inline::Perl5>);

    method need(CompUnit::Repository::Perl5:D:
      CompUnit::DependencySpecification $spec,
      CompUnit::PrecompilationRepository $precomp?,
    --> CompUnit:D) {
        if $spec.from eq 'Perl5' {
            CATCH {
                when X::CompUnit::UnsatisfiedDependency {
                    X::NYI::Available.new(
                      :available('Inline::Perl5'),
                      :feature('Perl 5')
                    ).throw;
                }
            }

            my $compunit := $*REPO.need($perl5-dependency);
            my $perl5    := $compunit.handle.globalish-package<Inline>.WHO<Perl5>.default_perl5;

            if $*RAKUDO_MODULE_DEBUG -> $RMD {
                $RMD("Loading $spec.short-name() via Inline::Perl5");
            }
            my $short-name := $spec.short-name;
            my $handle := $perl5.require(
              $short-name,
              $spec.version-matcher =:= True
                ?? Num
                !! $spec.version-matcher.Num,
              :handle
            );
            CompUnit.new:
              :$short-name,
              :$handle,
              :repo(self),
              :repo-id($short-name),
              :from<Perl5>;
        }
        elsif self.next-repo -> $repo {
            $repo.need($spec, $precomp // self.precomp-repository)
        }
        else {
            X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
        }
    }

    method loaded(--> Empty) { }

    method id(--> Str:D) { 'Perl5' }

    method path-spec(--> Str:D) { 'perl5#' }

    multi method gist(CompUnit::Repository::Perl5:D:) {
        self.path-spec
    }
}

# vim: expandtab shiftwidth=4
