class CompUnit::Repository::FileSystem does CompUnit::Repository::Locally does CompUnit::Repository {
    has %!loaded; # cache compunit lookup for self.need(...)
    has %!seen;   # cache distribution lookup for self!matching-dist(...)
    has $!precomp;
    has $!id;
    has $!precomp-stores;
    has $!precomp-store;

    my @extensions = <pm6 pm>;

    method !comp-unit-id($name) {
        CompUnit::PrecompilationId.new(nqp::sha1($name));
    }

    method !precomp-stores() {
        $!precomp-stores //= Array[CompUnit::PrecompilationStore].new(
            self.repo-chain.map(*.precomp-store).grep(*.defined)
        )
    }

    method !matching-dist(CompUnit::DependencySpecification $spec) {
        return %!seen{~$spec} if %!seen{~$spec}:exists;

        with self.candidates($spec).head {
            return %!seen{~$spec} //= $_;
        }

        Nil
    }

    method id() {
        $!id //= do {
            my @parts =
                grep { .defined }, (.id with self.next-repo), slip   # slip next repo id into hash parts to be hashed together
                map  { nqp::sha1(slurp($_, :enc<iso-8859-1>)) },     # D8FEDAD3A05A68501ED829E21E5C8C80850910AB, 0BDE185BBAE51CE25E18A90B551A60AF27A9239C
                map  { self!dist-prefix.child($_) },                 # /home/lib/Foo/Bar.pm6, /home/lib/Foo/Baz.pm6
                self!distribution.meta<provides>.values.unique.sort; # lib/Foo/Bar.pm6, lib/Foo/Baz.pm6
            nqp::sha1(@parts.join(''));
        }
    }

    method resolve(CompUnit::DependencySpecification $spec --> CompUnit:D) {
        with self!matching-dist($spec) {
            return CompUnit.new(
                :short-name($spec.short-name),
                :repo-id(self!comp-unit-id($spec.short-name).Str),
                :repo(self),
                :distribution($_),
            );
        }
        return self.next-repo.resolve($spec) if self.next-repo;
        Nil
    }

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
        CompUnit::PrecompilationStore :@precomp-stores = self!precomp-stores(),

        --> CompUnit:D)
    {
        return %!loaded{~$spec} if %!loaded{~$spec}:exists;

        with self!matching-dist($spec) {
            my $name = $spec.short-name;
            my $file = self!dist-prefix.child($_.meta<provides>{$name});
            my $id   = self!comp-unit-id($name);
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
            $handle //= CompUnit::Loader.load-source-file($file);

            return %!loaded{~$spec} = CompUnit.new(
                :short-name($name),
                :$handle,
                :repo(self),
                :repo-id($id.Str),
                :$precompiled,
                :distribution($_),
            );
        }

        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method load(IO::Path:D $file --> CompUnit:D) {
        unless $file.is-absolute {

            # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
            my $precompiled = $file.Str.ends-with(Rakudo::Internals.PRECOMP-EXT);
            my $path = $!prefix.add($file);

            if $path.f {
                return %!loaded{$file.Str} //= CompUnit.new(
                    :handle(
                        $precompiled
                            ?? CompUnit::Loader.load-precompilation-file($path)
                            !! CompUnit::Loader.load-source-file($path)
                    ),
                    :short-name($file.Str),
                    :repo(self),
                    :repo-id($file.Str),
                    :$precompiled,
                    :distribution(self!distribution),
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

    method !dist-prefix {
        $!prefix.child('META6.json').e ?? $!prefix !! $!prefix.parent
    }

    proto method candidates(|) {*}
    multi method candidates(Str:D $name, :$auth, :$ver, :$api) {
        return samewith(CompUnit::DependencySpecification.new(
            short-name      => $name,
            auth-matcher    => $auth // True,
            version-matcher => $ver  // True,
            api-matcher     => $api  // True,
        ));
    }
    multi method candidates(CompUnit::DependencySpecification $spec) {
        return Empty unless $spec.from eq 'Perl6';

        my $distribution := self!distribution;

        return Empty unless ($distribution.meta<provides> && $distribution.meta<provides>{$spec.short-name})
                        or  ($distribution.meta<files>    && $distribution.meta<files>{$spec.short-name});

        my $version-matcher = ($spec.version-matcher ~~ Bool)
            ?? $spec.version-matcher
            !! Version.new($spec.version-matcher);
        my $api-matcher = ($spec.api-matcher ~~ Bool)
            ?? $spec.api-matcher
            !! Version.new($spec.api-matcher);

        return Empty unless ($distribution.meta<auth> // '') ~~ $spec.auth-matcher
            and Version.new($distribution.meta<ver> // 0) ~~ $version-matcher
            and Version.new($distribution.meta<api> // 0) ~~ $api-matcher;

        return ($distribution,);
    }

    proto method files(|) {*}
    multi method files($file, Str:D :$name!, :$auth, :$ver, :$api) {
        my $spec = CompUnit::DependencySpecification.new(
            short-name      => $name,
            auth-matcher    => $auth // True,
            version-matcher => $ver  // True,
            api-matcher     => $api  // True,
        );

        with self.candidates($spec) {
            my $matches := $_.grep: { .meta<files>{$file}:exists }

            my $absolutified-metas := $matches.map: {
                my $meta      = $_.meta;
                $meta<source> = self!dist-prefix.add($meta<files>{$file});
                $meta;
            }

            return $absolutified-metas.grep(*.<source>.e);
        }
    }
    multi method files($file, :$auth, :$ver, :$api) {
        my $spec = CompUnit::DependencySpecification.new(
            short-name      => $file,
            auth-matcher    => $auth // True,
            version-matcher => $ver  // True,
            api-matcher     => $api  // True,
        );

        with self.candidates($spec) {
            my $absolutified-metas := $_.map: {
                my $meta      = $_.meta;
                $meta<source> = self!dist-prefix.add($meta<files>{$file});
                $meta;
            }

            return $absolutified-metas.grep(*.<source>.e);
        }
    }

    method !distribution {
        my $distribution := $!prefix.add('META6.json').f
            ?? Distribution::Path.new($!prefix)
            !! Distribution::Hash.new({
                    ver   => '*',
                    api   => '*',
                    auth  => '',
                    files => %((
                        (Rakudo::Internals.DIR-RECURSE(self!dist-prefix.child('bin').absolute).map({ .IO.relative(self!dist-prefix) }).map({
                            $_ => $_
                        }).hash if self!dist-prefix.child('bin').d).Slip,
                        (Rakudo::Internals.DIR-RECURSE(self!dist-prefix.child('resources').absolute).map({ .IO.relative(self!dist-prefix) }).map({
                            $_ ~~ m/^resources\/libraries\/(.*)/
                                ?? ('resources/libraries/' ~ ($0.IO.dirname eq '.'??''!!$0.IO.dirname~"/") ~ $0.IO.basename.subst(/^lib/, '').subst(/\..*/, '') => $_)
                                !! ($_ => $_)
                        }).hash).Slip,
                    ).Slip),
                    provides => Rakudo::Internals.DIR-RECURSE($!prefix.absolute).map({ .IO.relative(self!dist-prefix) }).map({
                        $_.subst(:g, /\/ | \\/, "::").subst(:g, /\:\:+/, '::').subst(/^.*?'::'/, '').subst(/\..*/, '') => $_
                    }).hash,
                }, :prefix(self!dist-prefix));

        $distribution.meta<name> //= $distribution.meta<provides>.hash.keys.sort(*.chars).head // ~$!prefix; # must guess name when using -Ilib / use lib 'lib'
        $distribution.meta<ver>  = Version.new($distribution.meta<ver version>.first(*.defined) // 0);
        $distribution.meta<resources> //= $distribution.meta<files>.keys.grep(*.starts-with('resources/')).map(*.substr(10)).List;

        return $distribution;
    }

    method resource($dist-id, $key) {
        if self!distribution -> $dist {
            if $dist.meta<files>.hash.{$key} -> IO() $path {
                return $path.is-relative ?? $dist.prefix.add( $path ) !! $path;
            }
        }
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
