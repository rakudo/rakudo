class Distribution::Path does Distribution::Locally {
    has $!meta;
    has $!meta-file;
    submethod BUILD(:$!meta, :$!prefix, :$!meta-file --> Nil) { }
    method new(IO::Path $prefix, IO::Path :$meta-file = IO::Path) {
        my $meta-path = $meta-file // $prefix.add('META6.json');
        die "No meta file located at {$meta-path.path}" unless $meta-path.e;
        my $meta = Rakudo::Internals::JSON.from-json($meta-path.slurp);

        # generate `files` (special directories) directly from the file system
        my %bins = Rakudo::Internals.DIR-RECURSE($prefix.add('bin').absolute).map(*.IO).map: -> $real-path {
            my $name-path = $real-path.is-relative
                ?? $real-path
                !! $real-path.relative($prefix);
            $name-path.subst(:g, '\\', '/') => $name-path.subst(:g, '\\', '/')
        }

        my $resources-dir = $prefix.add('resources');
        my %resources = $meta<resources>.grep(*.?chars).map(*.IO).map: -> $path {
            my $real-path = $path ~~ m/^libraries\/(.*)/
                ?? $resources-dir.add('libraries').add( $*VM.platform-library-name($0.Str.IO) )
                !! $resources-dir.add($path);
            my $name-path = $path.is-relative
                ?? "resources/{$path}"
                !! "resources/{$path.relative($prefix)}";
            $name-path.subst(:g, '\\', '/') => $real-path.relative($prefix).subst(:g, '\\', '/')
        }

        $meta<files> = Hash.new(%bins, %resources);

        self.bless(:$meta, :$prefix, :$meta-file);
    }
    method meta { $!meta }
    method raku {
       self.^name ~ ".new({$!prefix.raku}, meta-file => {$!meta-file.raku})";
    }
}

# vim: expandtab shiftwidth=4
