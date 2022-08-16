augment class IO::Path {
    method stem(IO::Path:D: $parts = * --> Str:D) {
        my str $basename = self.basename;
        (my @indices := indices($basename, '.'))
          ?? nqp::substr(
               $basename,
               0,
               nqp::istype($parts,Whatever) || $parts > @indices
                ?? @indices[0]
                !! @indices[@indices - $parts]
             )
          !! $basename
    }
}

# vim: expandtab shiftwidth=4
