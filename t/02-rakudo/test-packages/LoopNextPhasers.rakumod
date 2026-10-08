unit module LoopNextPhasers;

our sub while-next() {
    my $i = 0;
    my @log;
    my @values = do while $i++ < 2 { NEXT { @log.push('N') }; $i };
    (@values.List, @log.List)
}

our sub while-two-nexts() {
    my $i = 0;
    my @log;
    my @values = do while $i++ < 2 { NEXT { @log.push('N') }; NEXT { @log.push('M') }; $i };
    (@values.List, @log.List)
}

our sub while-true-next() {
    my $i = 0;
    my @log;
    my @values = do while True { last if $i++ == 2; NEXT { @log.push('N') }; $i };
    (@values.List, @log.List)
}

our sub loop-next() {
    my $i = 0;
    my @log;
    my @values = do loop { last if $i++ == 2; NEXT { @log.push('N') }; $i };
    (@values.List, @log.List)
}

our sub while-undo-next() {
    my $i = 0;
    my @log;
    while $i++ < 2 { UNDO { }; NEXT { @log.push('N') }; @log.push($i) }
    @log.List
}
