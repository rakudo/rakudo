#!/usr/bin/env raku

unit sub MAIN(Str $folder, *@files where @files.so, Bool :$v);

for @files {
    given ( $folder.IO.add($_) ) {
        say 'rm -f ' ~ .Str if $v;
        .unlink if .e;
    }
}

# vim: expandtab sw=4
