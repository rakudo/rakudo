class CompUnitRepo::Local::File {
    has IO::Path $!path;
    has Str $.WHICH;

    my %instances;

    method new( $path ) {
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

    method install($source, $from?) {
        ...
    }

    method files($file, :$name, :$auth, :$ver) {
    }

    method candidates($name, :$file, :$auth, :$ver) {
        my @candi;
        my Mu $c := nqp::gethllsym('perl6', 'ModuleLoader').p6ml.locate_candidates(
            $name, nqp::p6listitems(nqp::decont([ $!path ])), :$file);
        if $c[0] {
            my $candi;
            $candi<ver> = Version.new('0');
            $candi<provides>{$name}<pm><file> = $c[0]<pm>;
            $candi<provides>{$name}{$*VM.precomp-ext}<file> = $c[0]<load>;
            @candi.push: $candi;
        }
        @candi
    }
}
