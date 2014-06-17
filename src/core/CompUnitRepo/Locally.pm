role CompUnitRepo::Locally {
    has Lock $!lock;
    has IO::Path $.path;
    has Str $.WHICH;

    my %instances;

    method new( $path is copy ) {
        $path = IO::Spec.rel2abs($path);
        return Nil unless $path.IO.e;
        %instances{$path} //= self.bless(:$path)
    }

    method BUILD(:$path) {
        $!lock  = Lock.new;
        $!WHICH = self.^name ~ '|' ~ $path;
        $!path  = $path.path;
        self
    }

    method Str  { self.DEFINITE ?? $!path.Str !! Nil }
    method gist { self.DEFINITE
      ?? "{self.short-id}:{$!path.Str}"
      !! self.^name;
    }
    method perl { self.DEFINITE
      ?? "CompUnitRepo.new('{self.short-id}:{$!path.Str}')"
      !! self.^name;
    }

    # stubs
    method install($source, $from?) { ... }
    method files($file, :$name, :$auth, :$ver) { ... }
    method candidates($name, :$file, :$auth, :$ver) { ... }
    method short-id() { ... }
}
