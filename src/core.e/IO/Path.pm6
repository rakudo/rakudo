augment class IO::Path {
    method stem(IO::Path:D: --> Str:D) {
        my str $basename = self.basename;
        my int $index = nqp::index($basename,'.');
        $index == -1 ?? $basename !! nqp::substr($basename,0,$index)
    }
    method suffix(IO::Path:D: --> Str:D) {
        my str $basename = self.basename;
        my int $index = nqp::index($basename,'.');
        $index == -1 ?? '' !! nqp::substr($basename,$index + 1)
    }
}

# vim: expandtab shiftwidth=4
