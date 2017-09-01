class CompUnit::Repository::Installation does CompUnit::Repository::Locally does CompUnit::Repository::Installable {
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));
    has %!loaded;
    has $!precomp;
    has $!id;
    has Int $!version;
    has %!dist-metas;
    has $!precomp-stores;
    has $!precomp-store;

    my $verbose := nqp::getenvhash<RAKUDO_LOG_PRECOMP>;

    submethod BUILD(:$!prefix, :$!lock, :$!WHICH, :$!next-repo --> Nil) { }

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
#perl# "%~dpn0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
#perl# "%~dpn0" %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have Perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem \';
__END__
:endofperl
';
    my $perl_wrapper = '#!/usr/bin/env #perl#
sub MAIN(:$name is copy, :$auth, :$ver, *@, *%) {
    CompUnit::RepositoryRegistry.run-script("#name#", :dist-name<#dist-name#>, :$name, :$auth, :$ver);
}';

    method !sources-dir() {
        my $sources = $.prefix.add('sources');
        $sources.mkdir unless $sources.e;
        $sources
    }

    method !resources-dir() {
        my $resources = $.prefix.add('resources');
        $resources.mkdir unless $resources.e;
        $resources
    }

    method !dist-dir() {
        my $dist = $.prefix.add('dist');
        $dist.mkdir unless $dist.e;
        $dist
    }

    method !bin-dir() {
        my $bin = $.prefix.add('bin');
        $bin.mkdir unless $bin.e;
        $bin
    }

    method !add-short-name($name, $dist, $source?, $checksum?) {
        my $short-dir = $.prefix.add('short');
        my $id = nqp::sha1($name);
        my $lookup = $short-dir.add($id);
        $lookup.mkdir;
        $lookup.add($dist.id).spurt(
                "{$dist.meta<ver>  // ''}\n"
            ~   "{$dist.meta<auth> // ''}\n"
            ~   "{$dist.meta<api>  // ''}\n"
            ~   "{$source // ''}\n"
            ~   "{$checksum // ''}\n"
        );
    }

    method !remove-dist-from-short-name-lookup-files($dist --> Nil) {
        my $short-dir = $.prefix.add('short');
        return unless $short-dir.e;

        my $id = $dist.id;

        for $short-dir.dir -> $dir {
            $dir.add($id).unlink;
            $dir.rmdir unless $dir.dir;
        }
    }

    method !file-id(Str $name, Str $dist-id) {
        my $id = $name ~ $dist-id;
        nqp::sha1($id)
    }

    method name(--> Str:D) {
        CompUnit::RepositoryRegistry.name-for-repository(self)
    }

    method !repo-prefix() {
        my $repo-prefix = self.name // '';
        $repo-prefix ~= '#' if $repo-prefix;
        $repo-prefix
    }

    method !read-dist($id) {
        my $dist = Rakudo::Internals::JSON.from-json($.prefix.add('dist').add($id).slurp);
        $dist<ver> = $dist<ver> ?? Version.new( ~$dist<ver> ) !! Version.new('0');
        $dist
    }

    method !repository-version(--> Int:D) {
        return $!version if defined $!version;
        my $version-file = $.prefix.add('version');
        return $!version = 0 unless $version-file ~~ :f;
        $!version = $version-file.slurp.Int
    }

    method upgrade-repository() {
        my $version = self!repository-version;
        my $short-dir = $.prefix.add('short');
        mkdir $short-dir unless $short-dir.e;
        my $precomp-dir = $.prefix.add('precomp');
        mkdir $precomp-dir unless $precomp-dir.e;
        self!sources-dir;
        my $resources-dir = self!resources-dir;
        my $dist-dir = self!dist-dir;
        self!bin-dir;
        if ($version < 1) {
            for $short-dir.dir -> $file {
                my @ids = $file.lines.unique;
                $file.unlink;
                $file.mkdir;
                for @ids -> $id {
                    my $dist = self!read-dist($id);
                    $file.add($id).spurt("{$dist<ver> // ''}\n{$dist<auth> // ''}\n{$dist<api> // ''}\n");
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
                $dist-file.spurt: Rakudo::Internals::JSON.to-json(%meta);
            }
        }
        $.prefix.add('version').spurt('2');
        $!version = 2;
    }

    proto method install(|) {*}
    multi method install($dist, %sources, %scripts?, %resources?, Bool :$force) {
        # XXX: Deprecation shim
        my %files;
        %files{"bin/$_.key()"} = $_.value for %scripts.pairs;
        %files{"resources/$_.key()"} = $_.value for %resources.pairs;
        my %meta6 = %(
            name     => $dist.?name,
            ver      => $dist.?ver // $dist.?version,
            auth     => $dist.?auth // $dist.?authority,
            provides => %sources,
            files    => %files,
        );

        return samewith(Distribution::Hash.new(%meta6, :prefix($*CWD)), :$force);
    }
    multi method install(Distribution $distribution, Bool :$force) {
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
            my $id          = self!file-id(~$file, $dist-id);
            my $destination = $resources-dir.add($id); # wrappers are put in bin/; originals in resources/
            my $withoutext  = $name-path.subst(/\.[exe|bat]$/, '');
            for '', '-j', '-m' -> $be {
                $.prefix.add("$withoutext$be").IO.spurt:
                    $perl_wrapper.subst('#name#', $name-path.IO.basename, :g).subst('#perl#', "perl6$be").subst('#dist-name#', $dist.meta<name>);
                if $is-win {
                    $.prefix.add("$withoutext$be.bat").IO.spurt:
                        $windows_wrapper.subst('#perl#', "perl6$be", :g);
                }
                else {
                    $.prefix.add("$withoutext$be").IO.chmod(0o755);
                }
            }
            self!add-short-name($name-path, $dist, $id);
            %links{$name-path} = $id;
            my $handle  = $dist.content($file);
            my $content = $handle.open.slurp-rest(:bin,:close);
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
            my $content = $handle.open.slurp-rest(:bin,:close);
            $destination.spurt($content);
            $handle.close;
        }

        my %meta = %($dist.meta);
        %meta<files>    = %links;    # add our new name-path => conent-id mapping
        %meta<provides> = %provides; # new meta data added to provides
        %!dist-metas{$dist-id} = %meta;
        $dist-dir.add($dist-id).spurt: Rakudo::Internals::JSON.to-json(%meta);

        # reset cached id so it's generated again on next access.
        # identity changes with every installation of a dist.
        $!id = Any;

        {
            my $head = $*REPO;
            PROCESS::<$REPO> := self; # Precomp files should only depend on downstream repos
            my $precomp = $*REPO.precomp-repository;
            my $repo-prefix = self!repo-prefix;
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :$dist-id);
            my %done;

            my $compiler-id = CompUnit::PrecompilationId.new($*PERL.compiler.id);
            for %provides.kv -> $source-name, $source-meta {
                my $id = CompUnit::PrecompilationId.new($source-meta.values[0]<file>);
                $precomp.store.delete($compiler-id, $id);
            }

            for %provides.kv -> $source-name, $source-meta {
                my $id = $source-meta.values[0]<file>;
                my $source = $sources-dir.add($id);
                my $source-file = $repo-prefix ?? $repo-prefix ~ $source.relative($.prefix) !! $source;

                if %done{$id} {
                    note "(Already did $id)" if $verbose;
                    next;
                }
                note("Precompiling $id ($source-name)") if $verbose;
                $precomp.precompile(
                    $source,
                    CompUnit::PrecompilationId.new($id),
                    :source-name("$source-file ($source-name)"),
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
                        unlink-if-exists( $bin-dir.add("$0$_") ) for '', '-m', '-j';
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

    method script($file, :$name!, :$auth, :$ver) {
        my $prefix = self.prefix;
        my $lookup = $prefix.add('short').add(nqp::sha1($file));
        return unless $lookup.e;

        # Scripts using this interface could only have been installed long after the introduction of
        # repo version 1, so we don't have to care about very old repos in this method.
        my @dists = $lookup.dir.map({
                my ($ver, $auth, $api, $resource-id) = $_.slurp.split("\n");
                $resource-id ||= self!read-dist($_.basename)<files>{$file};
                (id => $_.basename, ver => Version.new( $ver || 0 ), :$auth, :$api, :$resource-id).hash
            }).grep({
                $_.<auth> ~~ $auth
                and $_.<ver> ~~ $ver
            });
        for @dists.sort(*.<ver>).reverse {
            return self!resources-dir.add($_<resource-id>);
        }
    }

    method files($file, :$name!, :$auth, :$ver) {
        my @candi;
        my $prefix = self.prefix;
        my $lookup = $prefix.add('short').add(nqp::sha1($name));
        if $lookup.e {
            my $repo-version = self!repository-version;
            my @dists = $repo-version < 1
                ?? $lookup.lines.unique.map({
                        self!read-dist($_)
                    })
                !! $lookup.dir.map({
                        my ($ver, $auth, $api) = $_.slurp.split("\n");
                        (id => $_.basename, ver => Version.new( $ver || 0 ), auth => $auth, api => $api).hash
                    });
            for @dists.grep({$_<auth> ~~ $auth and $_<ver> ~~ $ver}) -> $dist is copy {
                $dist = self!read-dist($dist<id>) if $repo-version >= 1;
                with $dist<files>{$file} {
                    my $candi = %$dist;
                    $candi<files>{$file} = self!resources-dir.add($candi<files>{$file});
                    @candi.push: $candi;
                }
            }
        }
        @candi
    }

    method !matching-dist(CompUnit::DependencySpecification $spec) {
        if $spec.from eq 'Perl6' {
            my $repo-version = self!repository-version;
            my $lookup = $.prefix.add('short').add(nqp::sha1($spec.short-name));
            if $lookup.e {
                my @dists = (
                        $repo-version < 1
                        ?? $lookup.lines.unique.map({
                                $_ => self!read-dist($_)
                            })
                        !! $lookup.dir.map({
                                my ($ver, $auth, $api, $source, $checksum) = $_.slurp.split("\n");
                                $_.basename => {
                                    ver      => Version.new( $ver || 0 ),
                                    auth     => $auth,
                                    api      => $api,
                                    source   => $source || Any,
                                    checksum => $checksum || Str,
                                }
                            })
                    ).grep({
                        $_.value<auth> ~~ $spec.auth-matcher
                        and $_.value<ver> ~~ (($spec.version-matcher ~~ Bool)
                            ?? $spec.version-matcher # fast path for matching Version.new(*)
                            !! Version.new($spec.version-matcher))
                    });
                for @dists.sort(*.value<ver>).reverse.map(*.kv) -> ($dist-id, $dist) {
                    return ($dist-id, $dist);
                }
            }
        }
        Nil
    }

    method !lazy-distribution($dist-id) {
        class :: does Distribution::Locally {
            has $.dist-id;
            has $.read-dist;
            has $!installed-dist;
            method !dist {
                $!installed-dist //= InstalledDistribution.new($.read-dist()(), :$.prefix)
            }
            method meta(--> Hash:D)                      { self!dist.meta }
            method content($content-id --> IO::Handle:D) { self!dist.content($content-id) }
            method Str()                               { self!dist.Str }
        }.new(
            :$dist-id,
            :read-dist(-> { self!read-dist($dist-id) })
            :$.prefix,
        )
    }

    method resolve(
        CompUnit::DependencySpecification $spec,
        --> CompUnit:D)
    {
        my ($dist-id, $dist) = self!matching-dist($spec);
        if $dist-id {
            # xxx: replace :distribution with meta6
            return CompUnit.new(
                :handle(CompUnit::Handle),
                :short-name($spec.short-name),
                :version($dist<ver>),
                :auth($dist<auth> // Str),
                :repo(self),
                :repo-id($dist<source> // self!read-dist($dist-id)<provides>{$spec.short-name}.values[0]<file>),
                :distribution(self!lazy-distribution($dist-id)),
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
        my ($dist-id, $dist) = self!matching-dist($spec);
        if $dist-id {
            return %!loaded{~$spec} if %!loaded{~$spec}:exists;
            my $source-file-name = $dist<source>
                // do {
                    my $provides = self!read-dist($dist-id)<provides>;
                    X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw
                        unless $provides{$spec.short-name}:exists;
                    $provides{$spec.short-name}.values[0]<file>
                };
            my $loader = $.prefix.add('sources').add($source-file-name);
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :$dist-id);
            my $id = $loader.basename;
            my $repo-prefix = self!repo-prefix;
            my $handle = $precomp.try-load(
                CompUnit::PrecompilationDependency::File.new(
                    :id(CompUnit::PrecompilationId.new($id)),
                    :src($repo-prefix ?? $repo-prefix ~ $loader.relative($.prefix) !! $loader.absolute),
                    :checksum($dist<checksum>:exists ?? $dist<checksum> !! Str),
                    :$spec,
                ),
                :source($loader),
                :@precomp-stores,
            );
            my $precompiled = defined $handle;
            $handle //= CompUnit::Loader.load-source-file($loader);

            # xxx: replace :distribution with meta6
            my $compunit = CompUnit.new(
                :$handle,
                :short-name($spec.short-name),
                :version($dist<ver>),
                :auth($dist<auth> // Str),
                :repo(self),
                :repo-id($id),
                :$precompiled,
                :distribution(self!lazy-distribution($dist-id)),
            );
            return %!loaded{~$spec} = $compunit;
        }
        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method resource($dist-id, $key) {
        my $dist = %!dist-metas{$dist-id} //= Rakudo::Internals::JSON.from-json(self!dist-dir.add($dist-id).slurp);
        # need to strip the leading resources/ on old repositories
        self!resources-dir.add($dist<files>{$key.substr(self!repository-version < 2 ?? 10 !! 0)})
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

    method distribution($id) {
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
The distribution $name does not seem to have a \"provides\" section in its META.info file,
and so the packages will not be installed in the correct location.
Please ask the author to add a \"provides\" section, mapping every exposed namespace to a
file location in the distribution.
See http://design.perl6.org/S22.html#provides for more information.\n";
    }
}

# vim: ft=perl6 expandtab sw=4
