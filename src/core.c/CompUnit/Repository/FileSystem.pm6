class CompUnit::Repository::FileSystem
  does CompUnit::Repository::Locally
  does CompUnit::Repository
{
    has %!loaded; # cache compunit lookup for self.need(...)
    has %!seen;   # cache distribution lookup for self!matching-dist(...)
    has $!precomp;
    has $!id;
    has $!precomp-stores;
    has $!precomp-store;
    has $!distribution;
    has $!files-prefix;

    my constant @extensions = <.rakumod .pm6 .pm>;

    method !matching-dist(CompUnit::DependencySpecification $spec) {
        return $_ with %!seen{~$spec};

        with self.candidates($spec).head {
            return %!seen{~$spec} //= $_;
        }

        Nil
    }

    method !comp-unit-id($name) {
        CompUnit::PrecompilationId.new-from-string($name);
    }

    method !precomp-stores() {
        $!precomp-stores //= Array[CompUnit::PrecompilationStore].new(
            gather {
                my $repo = $*REPO;
                while $repo {
                    my \store = $repo.precomp-store;
                    take store if store.defined;
                    $repo = $repo.next-repo;
                }
            }
        )
    }

    method id() {
        $!id //= do with self!distribution -> $distribution {
            my $parts :=
                grep { .defined }, (.id with self.next-repo), slip # slip next repo id into hash parts to be hashed together
                map  { nqp::sha1($_) },
                map  { $distribution.content($_).open(:enc<iso-8859-1>).slurp(:close) },
                $distribution.meta<provides>.values.unique.sort;
            nqp::sha1($parts.join(''));
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
        return $_ with %!loaded{~$spec};

        with self!matching-dist($spec) {
            my $name = $spec.short-name;
            my $id   = self!comp-unit-id($name);
            my $*DISTRIBUTION  = $_;
            my $*RESOURCES     = Distribution::Resources.new(:repo(self), :dist-id(''));
            my $source-handle  = $_.content($_.meta<provides>{$name});
            my $precomp-handle = $precomp.try-load(
                CompUnit::PrecompilationDependency::File.new(
                    :$id,
                    :src($source-handle.path.absolute),
                    :$spec,
                ),
                :@precomp-stores,
            );

            return %!loaded{~$spec} = CompUnit.new(
                :short-name($name),
                :handle($precomp-handle // CompUnit::Loader.load-source($source-handle.open(:bin).slurp(:close))),
                :repo(self),
                :repo-id($id.Str),
                :precompiled($precomp-handle.defined),
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

    # This allows -Ilib to find resources/ ( and by extension bin/ ) for %?RESOURCES.
    # Note this only works in the well formed case, i.e. given Foo::Bar and no META6.json --
    # use lib 'packages'; use 'Foo::Bar'; # well formed -- %?RESOURCES uses packages/../resources
    # use lib 'packages/Foo'; use 'Bar';  # not well formed --  %?RESOURCES is ambigious now...
    #                                                           packages/../resources?
    #                                                           packages/resources?
    method !files-prefix {
        $!files-prefix //= $!prefix.child('META6.json').e ?? $!prefix !! $!prefix.parent
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

        my $distribution = self!distribution;

        unless ($distribution.meta<provides> && $distribution.meta<provides>{$spec.short-name})
            or ($distribution.meta<files>    && $distribution.meta<files>{$spec.short-name})
        {
            # Only break the cache if there is no inclusion authority (i.e. META6.json)
            return Empty if $!prefix.child('META6.json').e;

            # Break the !distribution cache if we failed to find a match using the cached distribution
            # but still found an existing file that matches the $spec.short-name
            return Empty unless @extensions.map({ $!prefix.add($spec.short-name.subst(:g, "::", $*SPEC.dir-sep) ~ $_) }).first(*.f);
            $!distribution := Nil;
            $distribution = self!distribution;
        }

        my $version-matcher = ($spec.version-matcher ~~ Bool)
            ?? $spec.version-matcher
            !! Version.new($spec.version-matcher);
        my $api-matcher = ($spec.api-matcher ~~ Bool)
            ?? $spec.api-matcher
            !! Version.new($spec.api-matcher);

        return Empty unless (($distribution.meta<auth> // '') eq '' || ($distribution.meta<auth> // '') ~~ $spec.auth-matcher)
            and (($distribution.meta<ver> // '*') eq '*' || Version.new($distribution.meta<ver> // 0) ~~ $version-matcher)
            and (($distribution.meta<api> // '*') eq '*' || Version.new($distribution.meta<api> // 0) ~~ $api-matcher);

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
                $meta<source> = $!prefix.add($meta<files>{$file});
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
                $meta<source> = self!files-prefix.add($meta<files>{$file});
                $meta;
            }

            return $absolutified-metas.grep(*.<source>.e);
        }
    }

    method !distribution {
        if nqp::isconcrete($!distribution) {
            $!distribution
        }

        # need to create a new distribution
        else {
            # Path contains a META6.json file, so only use paths/modules
            # explicitly declared therein ( -I ./ )
            my $dist := $!prefix.add('META6.json').f
              ?? Distribution::Path.new($!prefix)
              !! self!dist-from-ls;

            $!distribution := .clone(:dist-id(.Str))
              with CompUnit::Repository::Distribution.new($dist, :repo(self));
        }
    }

    # Path does not contain a META6.json file so grep for files to be used
    # to map to arbitrary module names later ( -I ./lib ).  This is considered
    # a developmental mode of library inclusion -- technically a Distribution,
    # but probably a poorly formed one.
    method !dist-from-ls {
        my $prefix := self!files-prefix;
        my &ls := {
            Rakudo::Internals.DIR-RECURSE($_).map:
              *.IO.relative($prefix).subst(:g, '\\', '/')
        }

        # files is a non-spec internal field used by
        # CompUnit::Repository::Installation included to make cross CUR
        # install easier
        my %files;

        # all the files in bin
        %files{$_} = $_ for ls($prefix.child('bin').absolute);

        # all the files in resources
        %files{ m/^resources\/libraries\/(.*)/
          ?? 'resources/libraries/'
               ~ ($0.IO.dirname eq '.' ?? '' !! $0.IO.dirname ~ "/")
               ~ $0.IO.basename.subst(/^lib/, '').subst(/\..*/, '')
          !! $_
        } = $_ for ls($prefix.child('resources').absolute);

        # already grepped resources/ for %files, so reuse that information
        my @resources := %files.keys
          .grep(*.starts-with('resources/'))
          .map(*.substr(10))
          .List.eager;

        # Set up hash of hashes of files found that could be modules.
        # Then select the most prominent one from there when done.
        my %provides;
        %provides{
          .subst(:g, /\//, "::")
          .subst(:g, /\:\:+/, '::')
          .subst(/^.*?'::'/, '')
          .subst(/\..*/, '')
        }{ .IO.extension } = $_
          for ls($!prefix.absolute).grep(*.ends-with(any(@extensions)));

        # make sure .rakumod has precedence over .pm6 and .pm
        $_ = .<rakumod> // .<pm6> // .<pm> for %provides.values;

        Distribution::Hash.new(:$prefix, %(
          name      => ~$!prefix,  # must make up a name when using -Ilib
          ver       => '*',
          api       => '*',
          auth      => '',
          files     => %files,
          resources => @resources,
          provides  => %provides,
        ))
    }

    method resource($dist-id, $key) {
        if self!distribution -> $dist {
            if $dist.meta<files>.hash.{$key} -> IO() $path {
                return $path.is-relative ?? $dist.prefix.add( $path ) !! $path;
            }
        }
    }

    method distribution(Str $id? --> Distribution) {
        # CURFS is a single-distribution repository so there is no need for $id
        # ( similar to $dist-id of method resource )
        return self!distribution;
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

# vim: expandtab shiftwidth=4
