my class IO::Spec::QNX is IO::Spec::Unix {

    method canonpath ($patharg, :$parent) {
        my $path = $patharg.Str;
        # Handle POSIX-style node names beginning with double slash (qnx, nto)
        # (POSIX says: "a pathname that begins with two successive slashes
        # may be interpreted in an implementation-defined manner, although
        # more than two leading slashes shall be treated as a single slash.")
        my $node = '';
        if $path ~~ s {^ ( '//' <-[ / ]>+ ) '/'? $} = ''
        or $path ~~ s {^ ( '//' <-[ / ]>+ ) '/' }   = '/'
            { $node = ~ $0; }

        $path = IO::Spec::Unix.canonpath($path, :$parent);

        $node ~ $path;
    }
}

# vim: expandtab shiftwidth=4
