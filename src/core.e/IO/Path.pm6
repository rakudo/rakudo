augment class IO::Path {
    method under-version-control(IO::Path:D: --> Bool:D) {
        my $io = self.resolve;
        (my $parent := $io.parent.resolve) eqv $io
          ?? (return False)
          !! ($io = $parent)
          until $io.add(".git").d;
        True
    }
}

# vim: expandtab shiftwidth=4
