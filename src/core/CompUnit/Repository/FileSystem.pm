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

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
    )
        returns CompUnit:D
    {
        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably
        my $name               = $spec.short-name;
        return %!loaded{$name} if %!loaded{$name}:exists;

        my $base := $!prefix.child($name.subst(:g, "::", $*SPEC.dir-sep) ~ '.').Str;
        my $found;

        # find source file
        # pick a META6.json if it is there
        if (my $meta = $!prefix.child('META6.json')) && $meta.f {
            my $json = from-json $meta.slurp;
            if $json<provides>{$name} -> $file {
                my $path = $file.IO.is-absolute ?? $file.IO !! $!prefix.child($file);
                $found = $path if IO::Path.new-from-absolute-path($path).f;
            }
        }
        # deduce path to compilation unit from package name
        else {
            if %seen{$base} -> $compunit {
                return $compunit;
            }

            # have extensions to check
            elsif %extensions<Perl6> -> @extensions {
                for @extensions -> $extension {
                    my $path = $base ~ $extension;
                    $found = $path.IO if IO::Path.new-from-absolute-path($path).f;
                }
            }
        }

        if $found {
            my $id = nqp::sha1($name ~ $*REPO.id);
            say $id if $*W and $*W.is_precompilation_mode;
            my $handle = (
                $precomp.may-precomp and (
                    $precomp.load($id, :since($found.modified)) # already precompiled?
                    or $precomp.precompile($found, $id) and $precomp.load($id) # if not do it now
                )
                or CompUnit::Loader.load-source-file($found) # precomp failed
            );

            return %!loaded{$name} = %seen{$base} = CompUnit.new(
                :name($name), :$handle, :repo(self), :repo-id($id)
            );
        }

        return self.next-repo.need($spec, $precomp) if self.next-repo;
        nqp::die("Could not find $spec in:\n" ~ $*REPO.repo-chain.map(*.Str).join("\n").indent(4));
    }

    method load(IO::Path:D $file) returns CompUnit:D {
        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably

        # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
        my $has_precomp = $file.Str.ends-with($precomp-ext);
        my $path = $file.is-absolute
                ?? $file
                !! $!prefix.child($file);

        if $path.f {
            return %!loaded{$file} = %seen{$path} = CompUnit.new(
                :handle(CompUnit::Loader.load-source-file($path)),
                :name($file.Str),
                :repo(self),
                :repo-id($file.Str),
            );
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
