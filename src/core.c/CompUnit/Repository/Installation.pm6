class CompUnit::Repository::Installation does CompUnit::Repository::Locally does CompUnit::Repository::Installable {
    has $!lock = Lock.new;
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('default', 'SysConfig').rakudo-build-config(), 'version'));
    has %!loaded; # cache compunit lookup for self.need(...)
    has %!seen;   # cache distribution lookup for self!matching-dist(...)
    has $!precomp;
    has $!id;
    has Int $!version;
    has %!dist-metas;
    has $!precomp-stores;
    has $!precomp-store;

    my $verbose = nqp::getenvhash<RAKUDO_LOG_PRECOMP>;

    my class InstalledDistribution is Distribution::Hash {
        method content($address) {
            my $entry = $.meta<provides>.values.first: { $_{$address}:exists };
            my $file = $entry
                ?? $.prefix.add('sources').add($entry{$address}<file>)
                !! $.prefix.add('resources').add($.meta<files>{$address});

            $file.open(:r)
        }
    }

    method writeable-path {
        $.prefix.w ?? $.prefix !! IO::Path;
    }

    method !writeable-path {
        self.can-install ?? $.prefix !! IO::Path;
    }

    method can-install() {
        $.prefix.w || ?(!$.prefix.e && try { $.prefix.mkdir } && $.prefix.e);
    }

    my $windows_wrapper = '@rem = \'--*-Perl-*--
@echo off
if "%OS%" == "Windows_NT" goto WinNT
#raku# "%~dpn0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofraku
:WinNT
#raku# "%~dpn0" %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofraku
if %errorlevel% == 9009 echo You do not have Rakudo in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofraku
@rem \';
__END__
:endofraku
';
    my $raku_wrapper = '#!/usr/bin/env #raku#
sub MAIN(:$name, :$auth, :$ver, *@, *%) {
    CompUnit::RepositoryRegistry.run-script("#name#", :$name, :$auth, :$ver);
}';

    method !sources-dir   { with $.prefix.add('sources')   { once { .mkdir unless .e }; $_ } }
    method !resources-dir { with $.prefix.add('resources') { once { .mkdir unless .e }; $_ } }
    method !dist-dir      { with $.prefix.add('dist')      { once { .mkdir unless .e }; $_ } }
    method !bin-dir       { with $.prefix.add('bin')       { once { .mkdir unless .e }; $_ } }
    method !short-dir     { with $.prefix.add('short')     { once { .mkdir unless .e }; $_ } }

    method !add-short-name($name, $dist, $source?, $checksum?) {
        my $id = nqp::sha1($name);
        my $lookup = self!short-dir.add($id) andthen { .mkdir unless .e }
        $lookup.add($dist.id).spurt(
                "{$dist.meta<ver>  // ''}\n"
            ~   "{$dist.meta<auth> // ''}\n"
            ~   "{$dist.meta<api>  // ''}\n"
            ~   "{$source // ''}\n"
            ~   "{$checksum // ''}\n"
        );
    }

    method !remove-dist-from-short-name-lookup-files($dist --> Nil) {
        my $short-dir = self!short-dir;
        return unless $short-dir.e;

        my $id = $dist.id;

        for $short-dir.dir -> $dir {
            $dir.add($id).unlink;
            $dir.rmdir unless $dir.dir.elems;
        }
    }

    method !file-id(Str $name, Str $dist-id) {
        nqp::sha1($name ~ $dist-id)
    }

    method name(--> Str:D) {
        CompUnit::RepositoryRegistry.name-for-repository(self)
    }

    method !repo-prefix() {
        self.name ?? (self.name ~ '#') !! ''
    }

    method !read-dist($id) {
        my $meta = Rakudo::Internals::JSON.from-json(self!dist-dir.add($id).slurp);
        $meta<ver> = $meta<ver> ?? Version.new( ~$meta<ver> ) !! Version.new('0');
        $meta
    }

    method !repository-version(--> Int:D) {
        $!version //= do {
            my $version-file = $.prefix.add('version');
            $version-file.f ?? $version-file.slurp.Int !! 0
        }
    }

    method upgrade-repository() {
        my $version = self!repository-version;
        my $short-dir = self!short-dir;
        mkdir $short-dir unless $short-dir.e;
        my $precomp-dir = $.prefix.add('precomp');
        mkdir $precomp-dir unless $precomp-dir.e;
        self!sources-dir;
        my $resources-dir = self!resources-dir;
        my $dist-dir = self!dist-dir;
        self!bin-dir;
        if ($version < 1) {
            for $short-dir.dir -> $file {
                my @ids is List = $file.lines.unique;
                $file.unlink;
                $file.mkdir;
                for @ids -> $id {
                    my $meta = self!read-dist($id);
                    $file.add($id).spurt("{$meta<ver> // ''}\n{$meta<auth> // ''}\n{$meta<api> // ''}\n");
                }
            }
        }
        if ($version < 2) {
            for $dist-dir.dir -> $dist-file {
                my %meta = Rakudo::Internals::JSON.from-json($dist-file.slurp);
                my $files = %meta<files> //= [];
                for eager $files.keys -> $file {
                    $files{"resources/$file"} = $files{$file}:delete
                        if $resources-dir.add($files{$file}).e
                        and not $.prefix.add($file).e; # bin/ is already included in the path
                }
                $dist-file.spurt: Rakudo::Internals::JSON.to-json(%meta, :sorted-keys);
            }
        }
        $.prefix.add('version').spurt('2');
        $!version = 2;
    }

    method install(Distribution $distribution, Bool :$force) {
        my $dist  = CompUnit::Repository::Distribution.new($distribution);
        my %files = $dist.meta<files>.grep(*.defined).map: -> $link {
            $link ~~ Str ?? ($link => $link) !! ($link.keys[0] => $link.values[0])
        }

        $!lock.protect( {
        my @*MODULES;
        my $path   = self!writeable-path or die "No writeable path found, $.prefix not writeable";
        my $lock = $.prefix.add('repo.lock').open(:create, :w);
        $lock.lock;

        my $version = self!repository-version;
        self.upgrade-repository unless $version == 2;

        my $dist-id = $dist.id;
        my $dist-dir = self!dist-dir;
        if not $force and $dist-dir.add($dist-id) ~~ :e {
            $lock.unlock;
            fail "$dist already installed";
        }

        my $sources-dir   = self!sources-dir;
        my $resources-dir = self!resources-dir;
        my $bin-dir       = self!bin-dir;
        my $is-win        = Rakudo::Internals.IS-WIN;

        self!add-short-name($dist.meta<name>, $dist); # so scripts can find their dist

        my %links; # map name-path to new content address
        my %provides; # meta data gets added, but the format needs to change to
                      # only extend the structure, not change it

        # the following 3 `for` loops should be a single loop, but has been
        # left this way due to impeding precomp changes

        # lib/ source files
        for $dist.meta<provides>.kv -> $name, $file is copy {
            # $name is "Inline::Perl5" while $file is "lib/Inline/Perl5.pm6"
            my $id          = self!file-id(~$name, $dist-id);
            my $destination = $sources-dir.add($id);
            my $handle      = $dist.content($file);
            my $content     = $handle.open(:bin).slurp(:close);

            self!add-short-name($name, $dist, $id,
              nqp::sha1(nqp::join("\n", nqp::split("\r\n",
                $content.decode('iso-8859-1')))));
            %provides{ $name } = ~$file => {
                :file($id),
                :time(try $file.IO.modified.Num),
                :$!cver
            };
            note("Installing {$name} for {$dist.meta<name>}") if $verbose and $name ne $dist.meta<name>;
            $destination.spurt($content);
        }

        # bin/ scripts
        for %files.kv -> $name-path, $file is copy {
            next unless $name-path.starts-with('bin/');
            my $name        = $name-path.subst(/^bin\//, '');
            my $id          = self!file-id(~$file, $dist-id);
            my $destination = $resources-dir.add($id); # wrappers are put in bin/; originals in resources/
            my $withoutext  = $name-path.subst(/\.[exe|bat]$/, '');
            for '', '-j', '-m', '-js' -> $be {
                $.prefix.add("$withoutext$be").IO.spurt:
                    $raku_wrapper.subst('#name#', $name, :g).subst('#raku#', "rakudo$be");
                if $is-win {
                    $.prefix.add("$withoutext$be.bat").IO.spurt:
                        $windows_wrapper.subst('#raku#', "rakudo$be", :g);
                }
                else {
                    $.prefix.add("$withoutext$be").IO.chmod(0o755);
                }
            }
            self!add-short-name($name-path, $dist, $id);
            %links{$name-path} = $id;
            my $handle  = $dist.content($file);
            my $content = $handle.open.slurp(:bin,:close);
            $destination.spurt($content);
            $handle.close;
        }

        # resources/
        for %files.kv -> $name-path, $file is copy {
            next unless $name-path.starts-with('resources/');
            # $name-path is 'resources/libraries/p5helper' while $file is 'resources/libraries/libp5helper.so'
            my $id             = self!file-id(~$name-path, $dist-id) ~ '.' ~ $file.IO.extension;
            my $destination    = $resources-dir.add($id);
            %links{$name-path} = $id;
            my $handle  = $dist.content($file);
            my $content = $handle.open.slurp(:bin,:close);
            $destination.spurt($content);
            $handle.close;
        }

        my %meta = %($dist.meta);
        %meta<files>    = %links;    # add our new name-path => content-id mapping
        %meta<provides> = %provides; # new meta data added to provides
        %!dist-metas{$dist-id} = %meta;
        $dist-dir.add($dist-id).spurt: Rakudo::Internals::JSON.to-json(%meta, :sorted-keys);

        # reset cached id so it's generated again on next access.
        # identity changes with every installation of a dist.
        $!id = Any;

        {
            my $head = $*REPO;
            PROCESS::<$REPO> := self; # Precomp files should only depend on downstream repos
            my $precomp = $*REPO.precomp-repository;
            my $repo-prefix = self!repo-prefix;
            my $*DISTRIBUTION = CompUnit::Repository::Distribution.new($dist, :repo(self), :$dist-id);
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :$dist-id);
            my %done;

            my $compiler-id = CompUnit::PrecompilationId.new-without-check($*RAKU.compiler.id);
            for %provides.sort {
                my $id = CompUnit::PrecompilationId.new-without-check($_.value.values[0]<file>);
                $precomp.store.delete($compiler-id, $id);
            }

            for %provides.sort {
                my $id = $_.value.values[0]<file>;
                my $source = $sources-dir.add($id);
                my $source-file = $repo-prefix ?? $repo-prefix ~ $source.relative($.prefix) !! $source;

                if %done{$id} {
                    note "(Already did $id)" if $verbose;
                    next;
                }
                note("Precompiling $id ($_.key())") if $verbose;
                $precomp.precompile(
                    $source,
                    CompUnit::PrecompilationId.new-without-check($id),
                    :source-name("$source-file ($_.key())"),
                );
                %done{$id} = 1;
            }
            PROCESS::<$REPO> := $head;
        }

        $lock.unlock;
    } ) }

    method uninstall(Distribution $distribution) {
        my $repo-version = self!repository-version;
        self.upgrade-repository unless $repo-version == 2;

        # xxx: currently needs to be passed in a distribution object that
        # has meta<files> pointing at content-ids, so you cannot yet just
        # pass in the original meta data and have it discovered and deleted
        # (i.e. update resolve to return such a ::Installation::Distribution)
        my $dist  = CompUnit::Repository::Distribution.new($distribution);
        my %provides      = $dist.meta<provides>;
        my %files         = $dist.meta<files>;
        my $sources-dir   = self.prefix.add('sources');
        my $resources-dir = self.prefix.add('resources');
        my $bin-dir       = self.prefix.add('bin');
        my $dist-dir      = self.prefix.add('dist');

        self!remove-dist-from-short-name-lookup-files($dist);
        my sub unlink-if-exists($path) { unlink($path) if $path.IO.e }

        # delete special directory files
        for %files.kv -> $name-path, $file {
            given $name-path {
                when /^bin\/(.*)/ {
                    # wrappers are located in $bin-dir (only delete if no other versions use wrapper)
                    unless self.files($name-path, :name($dist.meta<name>)).elems {
                        unlink-if-exists( $bin-dir.add("$0$_") ) for '', '-m', '-j', '-js', '.bat', '-m.bat', '-j.bat', '-js.bat';
                    }

                    # original bin scripts are in $resources-dir
                    unlink-if-exists( $resources-dir.add($file) )
                }
                when /^resources\// {
                    unlink-if-exists( $resources-dir.add($file) )
                }
            }
        }

        # delete sources
        unlink-if-exists( $sources-dir.add($_) ) for %provides.values.flatmap(*.values.map(*.<file>));

        # delete the meta file
        unlink( $dist-dir.add($dist.id) )
    }

    # Ideally this would return Distributions, but it'd break older bin/ scripts
    proto method files(|) {*}
    multi method files($file, Str:D :$name!, :$auth, :$ver, :$api) {
        # if we have to include :$name then we take the slow path

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
                $meta<source> = self!resources-dir.add($meta<files>{$file});
                $meta;
            }

            return $absolutified-metas.grep(*.<source>.e);
        }
    }
    multi method files($file, :$auth, :$ver, :$api) {
        # avoid parsing json if we don't need to know the short-name

        my $spec = CompUnit::DependencySpecification.new(
            short-name      => $file,
            auth-matcher    => $auth // True,
            version-matcher => $ver  // True,
            api-matcher     => $api  // True,
        );

        with self.candidates($spec) {
            my $absolutified-metas := $_.map: {
                my $meta      = $_.meta;
                $meta<source> = self!resources-dir.add($meta<source> || $meta<files>{$file});
                $meta;
            }

            return $absolutified-metas.grep(*.<source>.e);
        }
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

        # $lookup is a file system resource that acts as a fast meta data lookup for a given module short name.
        my $lookup = self!short-dir.add(nqp::sha1($spec.short-name));
        return Empty unless $lookup.e;

        # Each item contains a subset of meta data - notably items needed `use "Foo:ver<*>"`
        # All items match the given module short name, but may differ in ver, auth, api, etc.
        my $metas := (
                self!repository-version < 1
                ?? $lookup.lines.unique.map({
                        $_ => self!read-dist($_)
                    })
                !! $lookup.dir.map({
                        my ($ver, $auth, $api, $source, $checksum) = $_.slurp.split("\n");
                        $_.basename => {
                            auth     => $auth,
                            api      => Version.new( $api || 0 ), # Create the Version objects once
                            ver      => Version.new( $ver || 0 ), # (used to compare, and then sort)
                            source   => $source || Any,
                            checksum => $checksum || Str,
                        }
                    })
            );

        my $version-matcher = ($spec.version-matcher ~~ Bool)
            ?? $spec.version-matcher # fast path for matching Version.new(*)
            !! Version.new($spec.version-matcher);
        my $api-matcher = ($spec.api-matcher ~~ Bool)
            ?? $spec.api-matcher
            !! Version.new($spec.api-matcher);

        # $metas has already been filtered by name via $lookup, so do remaining filtering on fast lookup fields
        my $matching-metas := $metas.grep: {
            $_.value<auth> ~~ $spec.auth-matcher
            and $_.value<ver> ~~ $version-matcher
            and $_.value<api> ~~ $api-matcher
        }

        # Sort from highest to lowest by version and api
        my $sorted-metas := $matching-metas.sort(*.value<ver>).sort(*.value<api>).reverse;

        # There is nothing left to do with the subset of meta data, so initialize a lazy distribution with it
        my $distributions := $sorted-metas.map(*.kv).map: -> ($dist-id, $meta) { self!lazy-distribution($dist-id, :$meta) }

        # A different policy might wish to implement additional/alternative filtering or sorting at this point,
        # with the caveat that calling a non-lazy field will require parsing json for each matching distribution.
        # my $policy-okd-dists := $distributions.grep({ .meta<license> eq 'Artistic-2.0' }).sort(-*.meta<production>)`

        return $distributions;
    }

    # An equivalent of self.candidates($spec).head that caches the best match
    method !matching-dist(CompUnit::DependencySpecification $spec) {
        return $_ with %!seen{~$spec};

        with self.candidates($spec).head {
            $!lock.protect: { return %!seen{~$spec} //= $_ }
        }

        Nil
    }

    # A distribution that provides a subset of its meta data without parsing the full original
    # original json version, while lazily parsing once fields outside of that subset are used.
    my role LazyMetaReader {
        has $.meta-reader;
        method AT-KEY($key)     { $!meta-reader($key) }
        method EXISTS-KEY($key) { $!meta-reader($key).defined }
    }
    my role MetaAssigner {
        has $.meta-writer;
        method ASSIGN-KEY($key, $value) { $!meta-writer($key, $value) }
    }
    my class LazyDistribution does Distribution::Locally {
        has $.dist-id;
        has $.read-dist;
        has $!installed-dist;
        has $.meta;

        # Parses dist info from json and populates $.meta with any new fields
        method !dist {
            unless $!installed-dist.defined {
                $!installed-dist = InstalledDistribution.new($.read-dist()($!dist-id), :$.prefix);

                # Keep fields of the meta data subset that do not exist in the full meta data
                # (source, default values for versions, etc)
                my %hash = $!installed-dist.meta.hash;
                %hash{$_} //= $!meta{$_} for $!meta.hash.keys;
                $!meta = %hash;
            }
            $!installed-dist;
        }

        method meta(--> Hash:D) {
            my %hash = $!meta.hash;
            unless $!installed-dist.defined {
                # Allow certain meta fields to be read without a full parsing, and fallback
                # to calling self!dist to populate the entire meta data from json.
                %hash does LazyMetaReader({ $!meta.hash{$^a} // self!dist.meta.{$^a} });

                # Allows absolutifying paths in .meta<files source> to keep .files() happy
                %hash does MetaAssigner({ $!meta.ASSIGN-KEY($^a, $^b) });
            }

            return %hash;
        }
        method content($content-id --> IO::Handle:D) { self!dist.content($content-id) }
        method Str { CompUnit::Repository::Distribution.new(self).Str }
        method id { $.dist-id }
    }

    method !lazy-distribution($dist-id, :$meta) {
        LazyDistribution.new(
            :$dist-id,
            :read-dist(-> $_ { self!read-dist($_) }),
            :$meta,
            :$.prefix,
        )
    }

    method resolve(
        CompUnit::DependencySpecification $spec,
        --> CompUnit:D)
    {
        with self!matching-dist($spec) {
            my $meta = .meta;
            return CompUnit.new(
                :handle(CompUnit::Handle),
                :short-name($spec.short-name),
                :version($meta<ver>),
                :auth($meta<auth> // Str),
                :repo(self),
                :repo-id($meta<source>),
                :distribution($_),
            );
        }
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
        with self!matching-dist($spec) {
            my $meta = .meta;
            my $source-file-name = $meta<source>;
            X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw
                unless $source-file-name;
            my $loader = $.prefix.add('sources').add($source-file-name);
            my $id     = $loader.basename;
            return $_ with %!loaded{$id};

            my $*DISTRIBUTION = CompUnit::Repository::Distribution.new($_, :repo(self), :dist-id(.dist-id));
            my $*RESOURCES  = Distribution::Resources.new(:repo(self), :dist-id(.dist-id));
            my $repo-prefix = self!repo-prefix;
            my $handle      = $precomp.try-load(
                CompUnit::PrecompilationDependency::File.new(
                    :id(CompUnit::PrecompilationId.new-without-check($id)),
                    :src($repo-prefix ?? $repo-prefix ~ $loader.relative($.prefix) !! $loader.absolute),
                    :checksum(.meta<checksum> // Str),
                    :$spec,
                ),
                :source($loader),
                :@precomp-stores,
            );

            my $precompiled = defined $handle;
            $handle //= CompUnit::Loader.load-source-file($loader);

            my $compunit = CompUnit.new(
                :$handle,
                :short-name($spec.short-name),
                :version($meta<ver>),
                :auth($meta<auth> // Str),
                :repo(self),
                :repo-id($id),
                :$precompiled,
                :distribution($_),
            );

            $!lock.protect: {
                return %!loaded{$id} //= $compunit;
            }
        }
        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method resource($dist-id, $key) {
        my $meta = %!dist-metas{$dist-id} //= Rakudo::Internals::JSON.from-json(self!dist-dir.add($dist-id).slurp);
        # need to strip the leading resources/ on old repositories
        self!resources-dir.add($meta<files>{$key.substr(self!repository-version < 2 ?? 10 !! 0)})
    }

    method id() {
        return $!id if $!id;
        my $name = self.path-spec;
        $name ~= ',' ~ self.next-repo.id if self.next-repo;
        my $dist-dir = $.prefix.add('dist');
        $!id = nqp::sha1(nqp::sha1($name) ~ ($dist-dir.e ?? $dist-dir.dir !! ''))
    }

    method short-id() { 'inst' }

    method loaded(--> Iterable:D) {
        return %!loaded.values;
    }

    method distribution(Str $id --> Distribution) {
        InstalledDistribution.new(self!read-dist($id), :prefix(self.prefix))
    }

    method installed(--> Iterable:D) {
        my $dist-dir = self.prefix.add('dist');
        $dist-dir.e
            ?? $dist-dir.dir.map({ self.distribution($_.basename) })
            !! Nil
    }

    method precomp-store(--> CompUnit::PrecompilationStore:D) {
        $!precomp-store //= CompUnit::PrecompilationStore::File.new(
            :prefix(self.prefix.add('precomp')),
        )
    }

    method precomp-repository(--> CompUnit::PrecompilationRepository:D) {
        $!precomp := CompUnit::PrecompilationRepository::Default.new(
            :store(self.precomp-store),
        ) unless $!precomp;
        $!precomp
    }

    sub provides-warning($is-win, $name --> Nil) {
        my ($red,$clear) = Rakudo::Internals.error-rcgye;

        note "$red==={$clear}WARNING!$red===$clear
The distribution $name does not seem to have a \"provides\" section in its META6.json file,
and so the packages will not be installed in the correct location.
Please ask the author to add a \"provides\" section, mapping every exposed namespace to a
file location in the distribution.
See http://design.raku.org/S22.html#provides for more information.\n";
    }
}

# vim: expandtab shiftwidth=4
