class CompUnit::Repository::Installation does CompUnit::Repository::Locally does CompUnit::Repository::Installable {
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));
    has %!loaded;
    has $!precomp;
    has $!id;
    has Int $!version;

    my $verbose := nqp::getenvhash<RAKUDO_LOG_PRECOMP>;

    submethod BUILD(:$!prefix, :$!lock, :$!WHICH, :$!next-repo --> Nil) { }

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
    shift @*ARGS if $name;
    shift @*ARGS if $auth;
    shift @*ARGS if $ver;
    $name //= \'#dist-name#\';
    my @installations = $*REPO.repo-chain.grep(CompUnit::Repository::Installable);
    my @binaries = flat @installations.map: { .files(\'bin/#name#\', :$name, :$auth, :$ver) };
    unless +@binaries {
        @binaries = flat @installations.map: { .files(\'bin/#name#\') };
        if +@binaries {
            note q:to/SORRY/;
                ===SORRY!===
                No candidate found for \'#name#\' that match your criteria.
                Did you perhaps mean one of these?
                SORRY
            my %caps = :name([\'Distribution\', 12]), :auth([\'Author(ity)\', 11]), :ver([\'Version\', 7]);
            for @binaries -> $dist {
                for %caps.kv -> $caption, @opts {
                    @opts[1] = max @opts[1], ($dist{$caption} // \'\').Str.chars
                }
            }
            note \'  \' ~ %caps.values.map({ sprintf(\'%-*s\', .[1], .[0]) }).join(\' | \');
            for @binaries -> $dist {
                note \'  \' ~ %caps.kv.map( -> $k, $v { sprintf(\'%-*s\', $v.[1], $dist{$k} // \'\') } ).join(\' | \')
            }
        }
        else {
            note "===SORRY!===\nNo candidate found for \'#name#\'.\n";
        }
        exit 1;
    }

    exit run($*EXECUTABLE, @binaries[0].hash.<files><bin/#name#>, @*ARGS).exitcode
}';

    method !sources-dir() {
        my $sources = $.prefix.child('sources');
        $sources.mkdir unless $sources.e;
        $sources
    }

    method !resources-dir() {
        my $resources = $.prefix.child('resources');
        $resources.mkdir unless $resources.e;
        $resources
    }

    method !dist-dir() {
        my $dist = $.prefix.child('dist');
        $dist.mkdir unless $dist.e;
        $dist
    }

    method !bin-dir() {
        my $bin = $.prefix.child('bin');
        $bin.mkdir unless $bin.e;
        $bin
    }

    method !add-short-name($name, $dist) {
        my $short-dir = $.prefix.child('short');
        my $id = nqp::sha1($name);
        my $lookup = $short-dir.child($id);
        $lookup.mkdir;
        $lookup.child($dist.id).spurt("{$dist.ver // ''}\n{$dist.auth // ''}\n{$dist.api // ''}\n");
    }

    method !remove-dist-from-short-name-lookup-files($dist) {
        my $short-dir = $.prefix.child('short');
        return unless $short-dir.e;

        my $id = $dist.id;

        for $short-dir.dir -> $dir {
            $dir.child($id).unlink;
        }
    }

    method !file-id(Str $name, Str $dist-id) {
        my $id = $name ~ $dist-id;
        nqp::sha1($id)
    }

    method name(--> Str) {
        CompUnit::RepositoryRegistry.name-for-repository(self)
    }

    method !repo-prefix() {
        my $repo-prefix = self.name // '';
        $repo-prefix ~= '#' if $repo-prefix;
        $repo-prefix
    }

    method !read-dist($id) {
        my $dist = Rakudo::Internals::JSON.from-json($.prefix.child('dist').child($id).slurp);
        $dist<ver> = $dist<ver> ?? Version.new( ~$dist<ver> ) !! Version.new('0');
        $dist
    }

    method !repository-version(--> Int) {
        return $!version if defined $!version;
        my $version-file = $.prefix.child('version');
        return $!version = 0 unless $version-file ~~ :f;
        $!version = $version-file.slurp.Int
    }

    method !upgrade-repository(Int $version) {
        my $short-dir = $.prefix.child('short');
        mkdir $short-dir unless $short-dir.e;
        my $precomp-dir = $.prefix.child('precomp');
        mkdir $precomp-dir unless $precomp-dir.e;
        self!sources-dir;
        self!resources-dir;
        self!dist-dir;
        self!bin-dir;
        if ($version < 1) {
            $.prefix.child('version').spurt('1');
            for $short-dir.dir -> $file {
                my @ids = $file.lines.unique;
                $file.unlink;
                $file.mkdir;
                for @ids -> $id {
                    my $dist = self!read-dist($id);
                    $file.child($id).spurt("{$dist<ver> // ''}\n{$dist<auth> // ''}\n{$dist<api> // ''}\n");
                }
            }
        }
        $!version = 1;
    }

    proto method install(|) {*}
    multi method install($dist, %sources, %scripts?, %resources?, :$force) {
        # XXX: Deprecation shim
        my %meta6 = %(
            name     => $dist.?name,
            ver      => $dist.?ver // $dist.?version,
            auth     => $dist.?auth // $dist.?authority,
            provides => %sources,
            files    => (grep *.defined, |%scripts, |%resources),
        );
        my $new-dist = class {
            also does Distribution;
            method meta { %meta6 }
            method content($address is copy) {
                $address = Rakudo::Internals.IS-WIN ?? ~$address.subst('\\', '/', :g) !! ~$address;
                # everything is already an absolute path
                my $handle = IO::Handle.new: path => IO::Path.new($address.IO.absolute);
                $handle // $handle.throw;
            }
        }
        samewith($new-dist, :$force);
    }
    multi method install(Distribution $distribution, Bool :$force) {
        my $dist  = CompUnit::Repository::Distribution.new($distribution);
        my %files = $dist.meta<files>.grep(*.defined).map: -> $link {
            $link ~~ Str ?? ($link => $link) !! ($link.keys[0] => $link.values[0])
        }

        $!lock.protect( {
        my @*MODULES;
        my $path   = self!writeable-path or die "No writeable path found, $.prefix not writeable";
        my $lock = $.prefix.child('repo.lock').open(:create, :w);
        $lock.lock(2);

        my $version = self!repository-version;
        self!upgrade-repository($version) unless $version == 1;

        my $dist-id = $dist.id;
        my $dist-dir = self!dist-dir;
        if not $force and $dist-dir.child($dist-id) ~~ :e {
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
            my $id          = self!file-id($name, $dist-id);
            my $destination = $sources-dir.child($id);
            self!add-short-name($name, $dist);
            %provides{ $name } = $file => {
                :file($id),
                :time(try $file.IO.modified.Num),
                :$!cver
            };
            note("Installing {$name} for {$dist.meta<name>}") if $verbose and $name ne $dist.meta<name>;
            my $handle  = $dist.content($file);
            my $content = $handle.open.slurp-rest(:bin);
            $destination.spurt($content);
            $handle.close;
        }

        # bin/ scripts
        for %files.kv -> $name-path, $file is copy {
            next unless $name-path.starts-with('bin/');
            my $id          = self!file-id($file, $dist-id);
            my $destination = $resources-dir.child($id); # wrappers are put in bin/; originals in resources/
            my $withoutext  = $name-path.subst(/\.[exe|bat]$/, '');
            for '', '-j', '-m' -> $be {
                $.prefix.child("$withoutext$be").IO.spurt:
                    $perl_wrapper.subst('#name#', $name-path.IO.basename, :g).subst('#perl#', "perl6$be").subst('#dist-name#', $dist.meta<name>);
                if $is-win {
                    $.prefix.child("$withoutext$be.bat").IO.spurt:
                        $windows_wrapper.subst('#perl#', "perl6$be", :g);
                }
                else {
                    $.prefix.child("$withoutext$be").IO.chmod(0o755);
                }
            }
            self!add-short-name($name-path, $dist);
            %links{$name-path} = $id;
            my $handle  = $dist.content($file);
            my $content = $handle.open.slurp-rest(:bin);
            $destination.spurt($content);
            $handle.close;
        }

        # resources/
        for %files.kv -> $name-path, $file is copy {
            next unless $name-path.starts-with('resources/');
            # $name is 'resources/libraries/p5helper' while $file is 'resources/libraries/libp5helper.so'
            my $id             = self!file-id($name-path, $dist-id) ~ '.' ~ $file.IO.extension;
            my $destination    = $resources-dir.child($id);
            %links{$name-path} = $id;
            my $handle  = $dist.content($file);
            my $content = $handle.open.slurp-rest(:bin);
            $destination.spurt($content);
            $handle.close;
        }

        my %meta = %($dist.meta);
        %meta<files>    = %links;    # add our new name-path => conent-id mapping
        %meta<provides> = %provides; # new meta data added to provides
        $dist-dir.child($dist-id).spurt: Rakudo::Internals::JSON.to-json($dist.Hash);
        # XXX: $dist-dir.child($dist-id).spurt: to-json($dist.meta.hash);

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

            my @provides = $dist.provides.kv.map(-> $source-name, $ext {($source-name, $ext.values[0]<file>)});
            my $compiler-id = $*PERL.compiler.id;
            for @provides -> ($source-name, $id) {
                $precomp.store.delete($compiler-id, $id);
            }
            for %provides.kv -> $source-name, $source-meta {
                my $id = $source-meta.values[0]<file>;
                my $source = $sources-dir.child($id);
                my $source-file = $repo-prefix ?? $repo-prefix ~ $source.relative($.prefix) !! $source;

                if %done{$id} {
                    note "(Already did $id)" if $verbose;
                    next;
                }
                note("Precompiling $id ($source-name)") if $verbose;
                $precomp.precompile(
                    $source.IO,
                    $id,
                    :source-name("$source-file ($source-name)"),
                );
                %done{$id} = 1;
            }
            PROCESS::<$REPO> := $head;
        }

        $lock.unlock;
    } ) }

    method uninstall(Distribution $distribution) {
        # xxx: currently needs to be passed in a distribution object that
        # has meta<files> pointing at content-ids, so you cannot yet just
        # pass in the original meta data and have it discovered and deleted
        # (i.e. update resolve to return such a ::Installation::Distribution)
        my $dist  = CompUnit::Repository::Distribution.new($distribution);
        my %provides      = $dist.meta<provides>;
        my %files         = $dist.meta<files>;
        my $sources-dir   = self.prefix.child('sources');
        my $resources-dir = self.prefix.child('resources');
        my $bin-dir       = self.prefix.child('bin');
        my $dist-dir      = self.prefix.child('dist');

        self!remove-dist-from-short-name-lookup-files($dist);
        for %files.grep({$_.key ~~ /^bin\//}) {
            # actual bin scripts are in resources/, these are just the wrappers
            $bin-dir.child($_.key).unlink;
            $bin-dir.child($_.key ~ "-m").unlink;
            $bin-dir.child($_.key ~ "-j").unlink;
        }
        $sources-dir.child($_).unlink for %provides.map(*.values[0].values[0]<file>);
        $resources-dir.child($_).unlink for %files.values.grep({ !$resources-dir.child($_).IO.e });
        $dist-dir.child($dist.id).unlink;
    }

    method files($file, :$name!, :$auth, :$ver) {
        my @candi;
        my $prefix = self.prefix;
        my $lookup = $prefix.child('short').child(nqp::sha1($name));
        if $lookup.e {
            my $version = self!repository-version;
            my @dists = $version < 1
                ?? $lookup.lines.unique.map({
                        self!read-dist($_)
                    })
                !! $lookup.dir.map({
                        my ($ver, $auth, $api) = $_.slurp.split("\n");
                        (id => $_.basename, ver => Version.new( $ver || 0 ), auth => $auth, api => $api).hash
                    });
            for @dists.grep({$_<auth> ~~ $auth and $_<ver> ~~ $ver}) -> $dist is copy {
                $dist = self!read-dist($dist<id>) if $version >= 1;
                with $dist<files>{$file} {
                    my $candi   = %$dist;
                    $candi<files>{$file} = $prefix.abspath ~ '/resources/' ~ $candi<files>{$file}
                        unless $candi<files>{$file} ~~ /^$prefix/;
                    @candi.push: $candi;
                }
            }
        }
        @candi
    }

    method !matching-dist(CompUnit::DependencySpecification $spec) {
        if $spec.from eq 'Perl6' {
            my $version = self!repository-version;
            my $lookup = $.prefix.child('short').child(nqp::sha1($spec.short-name));
            if $lookup.e {
                my @dists = (
                        $version < 1
                        ?? $lookup.lines.unique.map({
                                $_ => self!read-dist($_)
                            })
                        !! $lookup.dir.map({
                                my ($ver, $auth, $api) = $_.slurp.split("\n");
                                $_.basename => {ver => Version.new( $ver || 0 ), auth => $auth, api => $api}
                            })
                    ).grep({
                        $_.value<auth> ~~ $spec.auth-matcher
                        and $_.value<ver> ~~ $spec.version-matcher
                    });
                for @dists.sort(*.value<ver>).reverse.map(*.kv) -> ($dist-id, $dist) {
                    return ($dist-id, $version < 1 ?? $dist !! self!read-dist($dist-id));
                }
            }
        }
        Nil
    }

    method resolve(
        CompUnit::DependencySpecification $spec,
    )
        returns CompUnit
    {
        my ($dist-id, $dist) = self!matching-dist($spec);
        if $dist-id {
            my $loader = $.prefix.child('sources').child(
                $dist<provides>{$spec.short-name}.values[0]<file>
            );
            my $id = $loader.basename;

            # xxx: replace :distribution with meta6
            return CompUnit.new(
                :handle(CompUnit::Handle),
                :short-name($spec.short-name),
                :version($dist<ver>),
                :auth($dist<auth> // Str),
                :repo(self),
                :repo-id($id),
                :distribution(Distribution::Hash.new($dist.hash, :$.prefix)),
            );
        }
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
        my ($dist-id, $dist) = self!matching-dist($spec);
        if $dist-id {
            return %!loaded{$spec.short-name} if %!loaded{$spec.short-name}:exists;
            my $loader = $.prefix.child('sources').child(
                $dist<provides>{$spec.short-name}.values[0]<file>
            );
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :$dist-id);
            my $id = $loader.basename;
            my $repo-prefix = self!repo-prefix;
            my $handle = $precomp.try-load(
                CompUnit::PrecompilationDependency::File.new(
                    :$id,
                    :src($repo-prefix ?? $repo-prefix ~ $loader.relative($.prefix) !! $loader.abspath),
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
                :distribution(Distribution::Hash.new($dist.hash, :$.prefix)),
            );
            return %!loaded{$compunit.short-name} = $compunit;
        }
        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method resource($dist-id, $key) {
        my $dist = Rakudo::Internals::JSON.from-json(self!dist-dir.child($dist-id).slurp);
        self!resources-dir.child($dist<files>{$key})
    }

    method id() {
        return $!id if $!id;
        my $name = self.path-spec;
        $name ~= ',' ~ self.next-repo.id if self.next-repo;
        my $dist-dir = $.prefix.child('dist');
        $!id = nqp::sha1(nqp::sha1($name) ~ ($dist-dir.e ?? $dist-dir.dir !! ''))
    }

    method short-id() { 'inst' }

    method loaded() returns Iterable {
        return %!loaded.values;
    }

    method precomp-store() returns CompUnit::PrecompilationStore {
        CompUnit::PrecompilationStore::File.new(
            :prefix(self.prefix.child('precomp')),
        )
    }

    method precomp-repository() returns CompUnit::PrecompilationRepository {
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
