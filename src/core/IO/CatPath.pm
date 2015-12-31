class IO::CatPath {
    has @.todo;
    
    method BUILD(@!todo) { self }
    multi method new(IO::CatPath: +@todo) { nqp::create(self).BUILD(@todo) }

    method open(IO::CatPath:D: *%named) {
        IO::CatHandle.new(self).open(|%named)
    }

    method slurp(IO::CatPath:D: *%named) {
        my $handle  = self.open(|%named);
        my $slurped = $handle.slurp-rest(|%named);
        $handle.close;
        $slurped
    }

    method lines(IO::CatPath:D: *%named) {
        self.open(|%named).lines(:close)
    }
    method words(IO::CatPath:D: *%named) {
        self.open(|%named).words(:close)
    }
    method comb(IO::CatPath:D: $comber = "", *%named) {
        self.open(|%named).comb($comber, :close)
    }
    method split(IO::CatPath:D: $splitter = "", *%named) {
        self.open(|%named).split($splitter, :close)
    }

    multi method Str(IO::CatPath:D:) {
        @!todo>>.Str.join(" ")
    }
    multi method gist(IO::CatPath:D:) {
        "({self.Str}).IO"
    }
    multi method perl(IO::CatPath:D:) {
        @!todo.perl ~ '.IO'
    }
}

# vim: ft=perl6 expandtab sw=4
