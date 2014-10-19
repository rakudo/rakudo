role CompUnitRepo::Locally {
    has Lock     $!lock;
    has IO::Path $.path;
    has Str      $!WHICH;

    my %instances;

    method new($dir) {
        my $path := IO::Path.new-from-absolute-path($*SPEC.rel2abs($dir));
        return Nil unless $path.d and $path.r;
        %instances{$path.abspath} //= self.bless(:$path, :lock(Lock.new));
    }

    multi method WHICH (CompUnitRepo::Locally:D:) {
        $!WHICH //= self.^name ~ '|' ~ $!path.abspath;
    }
    method Str   { self.DEFINITE ?? $!path.abspath !! Nil }
    method gist  { self.DEFINITE
      ?? "{self.short-id}:{$!path.abspath}"
      !! self.^name;
    }
    method perl  { self.DEFINITE
      ?? "CompUnitRepo.new('{self.short-id}:{$!path.abspath}')"
      !! self.^name;
    }

    # stubs
    method install    ($source, $from?             ) { ... }
    method files      ($file, :$name, :$auth, :$ver) { ... }
    method candidates ($name, :$file, :$auth, :$ver) { ... }
    method short-id   (                            ) { ... }
}
