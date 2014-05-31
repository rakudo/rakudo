class CompUnitRepo::Local::File {
    has @!paths;
    method new( *@location ) {
        self.bless(:@location)
    }

    method BUILD(:@location) {
        @!paths = @location>>.map(*.path);
        self
    }

    method Str { @!paths.Str }
    method gist { "CompUnitRepo::Local::File(" ~ @!paths.Str ~ ')' }

    method install($source, $from?) {
        ...
    }

    method files($file, :$name, :$auth, :$ver) {
    }

    method candidates($name, :$file, :$auth, :$ver) {
        my @candi;
        my Mu $c := nqp::gethllsym('perl6', 'ModuleLoader').p6ml.locate_candidates(
            $name, nqp::p6listitems(nqp::decont([ @!paths ])), :$file);
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
