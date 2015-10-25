role CompUnitRepo::Locally {
    has Lock     $!lock;
    has IO::Path $.IO;
    has Str      $.WHICH;

    my %instances;

    method new(CompUnitRepo::Locally: $dir, CompUnit::Repository :$next-repo) {
        my $abspath := $*SPEC.rel2abs($dir);
        my $IO      := IO::Path.new-from-absolute-path($abspath);

        %instances{$abspath} //=
          self.bless(:$IO, :lock(Lock.new), :WHICH(self.^name ~ '|' ~ $abspath), :$next-repo);
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

    method need(
        CompUnit::DependencySpecification $spec,
        \GLOBALish is raw = Any,
        CompUnit::PrecompilationRepository :$precomp = self.precomp-repository(),
        :$line
    )
        returns CompUnit:D
    {
        my @candidates = self.candidates($spec.short-name, :auth($spec.auth-matcher), :ver($spec.version-matcher));
        if @candidates {
            @candidates[0].load(GLOBALish, :$line);
            return @candidates[0];
        }
        return self.next-repo.need($spec, GLOBALish, :$precomp, :$line) if self.next-repo;
        nqp::die("Could not find $spec in $.Str");
    }

    method loaded() returns Iterable {
        return ();
    }
}

# vim: ft=perl6 expandtab sw=4
