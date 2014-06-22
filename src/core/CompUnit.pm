class CompUnit {
    has Lock $!lock;
    has IO::Path $.path;
    has Str $!WHICH;

    my %instances;

    method new( $path is copy ) {
        $path = IO::Spec.rel2abs($path);
        return Nil unless $path.IO.e;
        %instances{$path} //= self.bless(:$path)
    }

    method BUILD(:$path) {
        $!lock  = Lock.new;
        $!WHICH = "{self.^name}|$path";
        $!path  = $path.path;
        self
    }

    method WHICH { self.DEFINITE ?? $!WHICH !! self.^name }
    method Str   { self.DEFINITE ?? $!path.Str !! Nil }
    method gist  { self.DEFINITE
      ?? "{self.name}:{$!path.Str}"
      !! self.^name;
    }
    method perl  { self.DEFINITE
      ?? "CompUnit.new('{$!path.Str}')"
      !! self.^name;
    }
}
