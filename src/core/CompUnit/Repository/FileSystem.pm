class CompUnit::Repository::FileSystem does CompUnit::Repository::Locally does CompUnit::Repository {
    has %!loaded;
    has $!precomp;
    has $!id;
    has %!meta;
    has $!precomp-stores;
    has $!precomp-store;

    my @extensions = <pm6 pm>;
    my $extensions := nqp::hash('pm6',1,'pm',1);

    # global cache of files seen
    my %seen;

    method !matching-file(CompUnit::DependencySpecification $spec) {
        if $spec.from eq 'Perl6' {
            my $name = $spec.short-name;
            return %!loaded{$name} if %!loaded{$name}:exists;

            my $base := $!prefix.add($name.subst(:g, "::", $*SPEC.dir-sep) ~ '.').Str;
            return $base if %seen{$base}:exists;
            my $found;

            # find source file
            # pick a META6.json if it is there
            if not %!meta and (my $meta = $!prefix.add('META6.json')) and $meta.f {
                try {
                    %!meta = Rakudo::Internals::JSON.from-json: $meta.slurp;
                    CATCH {
                        when JSONException {
                            fail "Invalid JSON found in META6.json";
                        }
                    }
                }
            }
            if %!meta {
                if %!meta<provides>{$name} -> $file {
                    my $path = $file.IO.is-absolute ?? $file.IO !! $!prefix.add($file);
                    $found = $path if $path.f;
                }
            }

            unless ?$found {
                # deduce path to compilation unit from package name
                for @extensions -> $extension {
                    my $path = ($base ~ $extension).IO;
                    $found = $path if $path.f;
                }
            }

            return $base, $found if $found;
        }
        False
    }

    method !comp-unit-id($name) {
        CompUnit::PrecompilationId.new(nqp::sha1($name));
    }

    method id() {
        my $parts := nqp::list_s;
        my $prefix = self.prefix;
        my $dir  := { .match(/ ^ <.ident> [ <[ ' - ]> <.ident> ]* $ /) }; # ' hl
        my $file := -> str $file {
            nqp::eqat($file,'.pm',nqp::sub_i(nqp::chars($file),3))
            || nqp::eqat($file,'.pm6',nqp::sub_i(nqp::chars($file),4))
        };
        nqp::if(
          $!id,
          $!id,
          ($!id = nqp::if(
            $prefix.e,
            nqp::stmts(
              (my $iter := Rakudo::Internals.DIR-RECURSE(
                $prefix.absolute,:$dir,:$file).iterator),
              nqp::until(
                nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::filereadable($pulled),
                  nqp::push_s($parts,nqp::sha1(slurp($pulled, :enc<iso-8859-1>))),
                )
              ),
              nqp::if(
                (my $next := self.next-repo),
                nqp::push_s($parts,$next.id),
              ),
              nqp::sha1(nqp::join('',$parts))
            ),
            nqp::sha1('')
          ))
        )
    }

    method resolve(CompUnit::DependencySpecification $spec --> CompUnit:D) {
        my ($base, $file) = self!matching-file($spec);

        return CompUnit.new(
            :short-name($spec.short-name),
            :repo-id(self!comp-unit-id($spec.short-name).Str),
            :repo(self)
        ) if $base;
        return self.next-repo.resolve($spec) if self.next-repo;
        Nil
    }

    method !precomp-stores() {
        $!precomp-stores //= Array[CompUnit::PrecompilationStore].new(
            self.repo-chain.map(*.precomp-store).grep(*.defined)
        )
    }

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
        CompUnit::PrecompilationStore :@precomp-stores = self!precomp-stores(),

        --> CompUnit:D)
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
                :repo-id($id.Str),
                :$precompiled,
            );
        }

        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method load(IO::Path:D $file --> CompUnit:D) {
        unless $file.is-absolute {

            # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
            my $precompiled =
              $file.Str.ends-with(Rakudo::Internals.PRECOMP-EXT);
            my $path = $!prefix.add($file);

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

    method loaded(--> Iterable:D) {
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

        # When $.prefix points at a directory containing a meta file (eg. -I.)
        return $.prefix.add( %!meta<files>.first(*<<$key>>).values[0] )
            if %!meta<files>.first(*<<$key>>);
        return $.prefix.add( $key )
            if %!meta<resources>.first({ $_ eq $key.subst(/^resources\//, "") });

        # When $.prefix is presumably the 'lib' folder (eg. -Ilib)
        return $.prefix.parent.add($key);
    }

    method precomp-store(--> CompUnit::PrecompilationStore:D) {
        $!precomp-store //= CompUnit::PrecompilationStore::File.new(
            :prefix(self.prefix.add('.precomp')),
        )
    }

    method precomp-repository(--> CompUnit::PrecompilationRepository:D) {
        $!precomp := CompUnit::PrecompilationRepository::Default.new(
            :store(self.precomp-store),
        ) unless $!precomp;
        $!precomp
    }
}

# vim: ft=perl6 expandtab sw=4
