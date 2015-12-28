class CompUnit::Repository::FileSystem does CompUnit::Repository::Locally does CompUnit::Repository {
    has %!loaded;
    has $!precomp;

    my %extensions =
      Perl6 => <pm6 pm>,
      Perl5 => <pm5 pm>,
      NQP   => <nqp>,
      JVM   => ();

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
                    my $json = from-json $meta.slurp;
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
            # deduce path to compilation unit from package name
            elsif %extensions<Perl6> -> @extensions {
                for @extensions -> $extension {
                    my $path = $base ~ $extension;
                    $found = $path.IO if IO::Path.new-from-absolute-path($path).f;
                }
            }

            return $base, $found if $found;
        }
        False
    }

    method resolve(CompUnit::DependencySpecification $spec) returns CompUnit {
        my ($base, $file) = self!matching-file($spec);
        return CompUnit.new(
            :short-name($spec.short-name),
            :repo-id($file.Str),
            :repo(self)
        ) if $base;
        return self.next-repo.resolve($spec) if self.next-repo;
        Nil
    }

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
    )
        returns CompUnit:D
    {
        my ($base, $file) = self!matching-file($spec);
        if $base {
            my $name = $spec.short-name;
            return %!loaded{$name} if %!loaded{$name}:exists;
            return %seen{$base}    if %seen{$base}:exists;

            my $id = nqp::sha1($name ~ $*REPO.id);
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :dist-id(''));
            my $handle = (
                $precomp.may-precomp and (
                    $precomp.load($id, :since($file.modified)) # already precompiled?
                    or $precomp.precompile($file, $id) and $precomp.load($id) # if not do it now
                )
            );
            my $precompiled = ?$handle;

            if $*W and $*W.is_precompilation_mode {
                if $precompiled {
                    say "$id $file";
                }
                else {
                    nqp::exit(0);
                }
            }
            $handle ||= CompUnit::Loader.load-source-file($file); # precomp failed

            return %!loaded{$name} = %seen{$base} = CompUnit.new(
                :short-name($name), :$handle, :repo(self), :repo-id($id), :$precompiled
            );
        }

        return self.next-repo.need($spec, $precomp) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method load(IO::Path:D $file) returns CompUnit:D {
        unless $file.is-absolute {

            # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
            my $precompiled =
              $file.Str.ends-with(Rakudo::Internals.PRECOMP-EXT);
            my $path = $!prefix.child($file);

            if $path.f {
                return %!loaded{$file} = %seen{$path} = CompUnit.new(
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
        $.prefix.parent.child('resources').child($key);
    }

    method precomp-repository() returns CompUnit::PrecompilationRepository {
        $!precomp := CompUnit::PrecompilationRepository::Default.new(
            :store(
                CompUnit::PrecompilationStore::File.new(
                    :prefix(self.prefix.child('.precomp')),
                )
            ),
        ) unless $!precomp;
        $!precomp
    }
}

# vim: ft=perl6 expandtab sw=4
