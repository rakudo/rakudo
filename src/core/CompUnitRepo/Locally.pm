role CompUnitRepo::Locally {
    has Lock     $!lock;
    has IO::Path $.path;
    has Str      $!WHICH;

    my %instances;

    method new(CompUnitRepo::Locally: $dir) {
        my $path := IO::Path.new-from-absolute-path($*SPEC.rel2abs($dir));
        return Nil unless $path.d and $path.r;
        %instances{$path.abspath} //= self.bless(:$path, :lock(Lock.new));
    }

    multi method WHICH (CompUnitRepo::Locally:D:) {
        $!WHICH //= self.^name ~ '|' ~ $!path.abspath;
    }
    method Str(CompUnitRepo::Locally:D:) { $!path.abspath }
    method gist(CompUnitRepo::Locally:D:) {
      "{self.short-id}:{$!path.abspath}"
    }
    method perl(CompUnitRepo::Locally:D:) {
      "CompUnitRepo.new('{self.short-id}:{$!path.abspath}')"
    }

    # stubs
    method install(CompUnitRepo::Locally:D: $source, $from? )             {...}
    method files(CompUnitRepo::Locally:D: $file, :$name, :$auth, :$ver)   {...}
    method candidates(CompUnitRepo::Locally:D: $name,:$file,:$auth,:$ver) {...}
    method short-id(CompUnitRepo::Locally:D:)                             {...}
}
