class CompUnitRepo::Local::File {
    has @!paths;
    method new( *@location ) {
        self.bless(:@location)
    }

    method BUILD(:@location) {
        @!paths = @location;
        self
    }

    method install($source, $from?) {
        ...
    }

    method files($file, :$name, :$auth, :$ver) {
    }

    method candidates($name, :$file, :$auth, :$ver) {
        my @candi;
        my Mu $c := nqp::gethllsym('perl6', 'ModuleLoader').p6ml.locate_candidates(
            $name, nqp::p6listitems(nqp::decont([ @!paths, @*INC ])), :$file);
        if $c[0] {
            my $candi;
            $candi<ver> = Version.new('0');
            $candi<provides>{$name}<pm><file> = $c[0]<pm>;
#?if parrot
            $candi<provides>{$name}<pir><file> = $c[0]<load>;
#?endif
#?if jvm
            $candi<provides>{$name}<jar><file> = $c[0]<load>;
#?endif
#?if moar
            $candi<provides>{$name}<moarvm><file> = $c[0]<load>;
#?endif
            @candi.push: $candi;
        }
        @candi
    }
}
