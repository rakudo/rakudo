role CompUnitRepo::Locally {
    has Lock     $!lock;
    has IO::Path $.path;
    has Str      $!WHICH;

    my %instances;

    method new( $path ) {
        return Nil unless $path.IO.e;
        %instances{$path} //= self.bless(:path($path.path), :lock(Lock.new));
    }

    multi method WHICH (CompUnitRepo::Locally:D:) {
        $!WHICH = self.^name ~ '|' ~ $!path;
    }
    method Str   { self.DEFINITE ?? $!path.Str !! Nil }
    method gist  { self.DEFINITE
      ?? "{self.short-id}:{$!path.Str}"
      !! self.^name;
    }
    method perl  { self.DEFINITE
      ?? "CompUnitRepo.new('{self.short-id}:{$!path.Str}')"
      !! self.^name;
    }

    # stubs
    method install    ($source, $from?             ) { ... }
    method files      ($file, :$name, :$auth, :$ver) { ... }
    method candidates ($name, :$file, :$auth, :$ver) { ... }
    method short-id   (                            ) { ... }
}
