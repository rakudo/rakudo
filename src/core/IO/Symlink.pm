# A class for symlinks that we know exist
my class IO::Symlink is Cool does IO::Pathy {

    method l(IO::Symlink:D:) { True }
    # other filetests to be using lstat rather than stat
}

# vim: ft=perl6 expandtab sw=4
