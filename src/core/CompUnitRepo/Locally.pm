role CompUnitRepo::Locally {
    has Lock     $!lock;
    has IO::Path $.IO;
    has Str      $.WHICH;

    my %instances;

    method new(CompUnitRepo::Locally: $dir) {
        my $abspath := $*SPEC.rel2abs($dir);
        my $IO      := IO::Path.new-from-absolute-path($abspath);

        %instances{$abspath} //=
          self.bless(:$IO,:lock(Lock.new),:WHICH(self.^name ~ '|' ~ $abspath));
    }

    multi method Str(CompUnitRepo::Locally:D:) { $!IO.abspath }
    multi method gist(CompUnitRepo::Locally:D:) {
        self.path-spec
    }
    multi method perl(CompUnitRepo::Locally:D:) {
        $?CLASS.^name ~ ".new('$!IO.abspath()')";
    }

    multi method WHICH(CompUnitRepo::Locally:D:) { $!WHICH }

    method path-spec(CompUnitRepo::Locally:D:) {
        self.short-id ~ '#' ~ $!IO.abspath;
    }

    # stubs
    method files(CompUnitRepo::Locally:D: $file, :$name, :$auth, :$ver)   {...}
    method candidates(CompUnitRepo::Locally:D: $name,:$file,:$auth,:$ver) {...}
    method short-id(CompUnitRepo::Locally:D:)                             {...}

    method need(CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository())
        returns CompUnit:D
    {
        self.candidates($spec.short-name, :auth($spec.auth-matcher), :ver($spec.version-matcher))[0];
    }

    method loaded() returns Iterable {
        return ();
    }
}
