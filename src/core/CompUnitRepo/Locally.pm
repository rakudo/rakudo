role CompUnitRepo::Locally {
    has IO::Path $!path;
    has Str $.WHICH;

    my %instances;

    method new( $path is copy ) {
        $path = IO::Spec.rel2abs($path);
        return Nil unless $path.IO.e;
        %instances{$path} //= self.bless(:$path)
    }

    method BUILD(:$path) {
        $!WHICH = self.^name ~ '|' ~ $path;
        $!path = $path.path;
        self
    }

    method Str { $!path.Str }
    method gist { self.^name ~ '(' ~ $!path.Str ~ ')' }
    method perl { self.^name ~ ".new('" ~ $!path.Str ~ "')" }

    # stubs
    method install($source, $from?) { ... }
    method files($file, :$name, :$auth, :$ver) { ... }
    method candidates($name, :$file, :$auth, :$ver) { ... }
}
