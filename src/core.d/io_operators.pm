multi sub slurp(IO::CatHandle:D $io = $*ARGFILES, :$bin, :$enc = 'utf8', |c) {
    my $result := $io.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}

# vim: ft=perl6 expandtab sw=4
