class CompUnit::Repository::FileSystem does CompUnit::Repository::Locally does CompUnit::Repository {
    has %!loaded;
    has $!precomp;
    has $!id;

    my @extensions = <pm6 pm>;

    # global cache of files seen
    my %seen;

    method !matching-file(CompUnit::DependencySpecification $spec) {
        if $spec.from eq 'Perl6' {
            my $name = $spec.short-name;
            return %!loaded{$name} if %!loaded{$name}:exists;

            my $base := $!prefix.child($name.subst(:g, "::", $*SPEC.dir-sep) ~ '.').Str;
            return $base if %seen{$base}:exists;
            my $found;

            # find source file
            # pick a META6.json if it is there
            if (my $meta = $!prefix.child('META6.json')) && $meta.f {
                try {
                    my $json = Rakudo::Internals::JSON.from-json: $meta.slurp;
                    if $json<provides>{$name} -> $file {
                        my $path = $file.IO.is-absolute ?? $file.IO !! $!prefix.child($file);
                        $found = $path if $path.f;
                    }

                    CATCH {
                        when JSONException {
                            fail "Invalid JSON found in META6.json";
                        }
                    }
                }
            }

            unless ?$found {
                # deduce path to compilation unit from package name
                for @extensions -> $extension {
                    my $path = $base ~ $extension;
                    $found = $path.IO if IO::Path.new-from-absolute-path($path).f;
                }
            }

            return $base, $found if $found;
        }
        False
    }

    method !comp-unit-id($name) {
        nqp::sha1($name);
    }

    method id() {
        $!id //= !self.prefix.e
            ?? nqp::sha1('')
            !! do {
                my $content-id = nqp::sha1(
                    [~]
                    map    {
                        if $_.IO.open -> $handle {
                            nqp::sha1($handle.slurp-rest(:enc<latin1>,:close));
                        }
                        else {
                            ''
                        }
                    },
                    grep   {
                        try Rakudo::Internals.FILETEST-F($_)
                        and Rakudo::Internals.MAKE-EXT($_) eq @extensions.any
                    },
                    Rakudo::Internals.DIR-RECURSE(self.prefix.absolute)
                );
                nqp::sha1($content-id ~ self.next-repo.id) if self.next-repo;
            }
    }

    method resolve(CompUnit::DependencySpecification $spec) returns CompUnit {
        my ($base, $file) = self!matching-file($spec);

        return CompUnit.new(
            :short-name($spec.short-name),
            :repo-id(self!comp-unit-id($spec.short-name)),
            :repo(self)
        ) if $base;
        return self.next-repo.resolve($spec) if self.next-repo;
        Nil
    }

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new(self.repo-chain.map(*.precomp-store).grep(*.defined)),
    )
        returns CompUnit:D
    {
        my ($base, $file) = self!matching-file($spec);
        if $base {
            my $name = $spec.short-name;
            return %!loaded{$name} if %!loaded{$name}:exists;
            return %seen{$base}    if %seen{$base}:exists;

            my $id = self!comp-unit-id($name);
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :dist-id(''));
            my $handle = $precomp.try-load(
                CompUnit::PrecompilationDependency::File.new(
                    :$id,
                    :src($file.Str),
                    :$spec,
                ),
                :@precomp-stores,
            );
            my $precompiled = defined $handle;
            $handle //= CompUnit::Loader.load-source-file($file); # precomp failed

            return %!loaded{$name} = %seen{$base} = CompUnit.new(
                :short-name($name),
                :$handle,
                :repo(self),
                :repo-id($id),
                :$precompiled,
            );
        }

        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method load(IO::Path:D $file) returns CompUnit:D {
        unless $file.is-absolute {

            # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
            my $precompiled =
              $file.Str.ends-with(Rakudo::Internals.PRECOMP-EXT);
            my $path = $!prefix.child($file);

            if $path.f {
                return %!loaded{$file.Str} //= %seen{$path.Str} = CompUnit.new(
                    :handle(
                        $precompiled
                            ?? CompUnit::Loader.load-precompilation-file($path)
                            !! CompUnit::Loader.load-source-file($path)
                    ),
                    :short-name($file.Str),
                    :repo(self),
                    :repo-id($file.Str),
                    :$precompiled,
                );
            }
        }

        return self.next-repo.load($file) if self.next-repo;
        nqp::die("Could not find $file in:\n" ~ $*REPO.repo-chain.map(*.Str).join("\n").indent(4));
    }

    method short-id() { 'file' }

    method loaded() returns Iterable {
        return %!loaded.values;
    }

    method files($file, :$name, :$auth, :$ver) {
        my $base := $file.IO;
        $base.f
         ?? { files => { $file => $base.path }, ver => Version.new('0') }
         !! ();
    }

    method resource($dist-id, $key) {
        # We now save the 'resources/' part of a resource's path in files, i.e:
        # "files" : [ "resources/libraries/xxx" => "resources/libraries/xxx.so" ]
        # but we also want to root any path request to the CUR's resources directory
        $.prefix.parent.child('resources').child($key.subst(/^resources\//, ""));
    }

    method precomp-store() returns CompUnit::PrecompilationStore {
        CompUnit::PrecompilationStore::File.new(
            :prefix(self.prefix.child('.precomp')),
        )
    }

    method precomp-repository() returns CompUnit::PrecompilationRepository {
        $!precomp := CompUnit::PrecompilationRepository::Default.new(
            :store(self.precomp-store),
        ) unless $!precomp;
        $!precomp
    }
}

# vim: ft=perl6 expandtab sw=4
