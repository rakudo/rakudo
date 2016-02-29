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
        $short-dir.mkdir unless $short-dir.e;
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

    method !repo-prefix() {
        my $repo-prefix = CompUnit::RepositoryRegistry.name-for-repository(self) // '';
        $repo-prefix ~= '#' if $repo-prefix;
        $repo-prefix
    }

    method !read-dist($id) {
        my $dist = from-json($.prefix.child('dist').child($id).slurp);
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
        if ($version < 1) {
            $.prefix.child('version').spurt('1');
            my $short-dir = $.prefix.child('short');
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

    method install(Distribution $dist, %sources, %scripts?, %resources?, :$force) {
        $!lock.protect( {
        my @*MODULES;
        my $path   = self!writeable-path or die "No writeable path found, $.prefix not writeable";
        my $lock //= $.prefix.child('repo.lock').open(:create, :w);
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

        self!add-short-name($dist.name, $dist); # so scripts can find their dist

        # Walk the to be installed files, decide whether we put them into
        # "provides" or just "files".
        for %sources.kv -> $name, $file is copy {
            $file           = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            # $name is "Inline::Perl5" while $file is "lib/Inline/Perl5.pm6"
            my $id          = self!file-id($name, $dist-id);
            my $destination = $sources-dir.child($id);
            self!add-short-name($name, $dist);
            $dist.provides{ $name } = {
                pm => {
                    :file($id),
                    :time(try $file.IO.modified.Num),
                    :$!cver
                }
            };
            note("Installing {$name} for {$dist.name}") if $verbose and $name ne $dist.name;
            copy($file, $destination);
        }

        for %scripts.kv -> $basename, $file is copy {
            $file           = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            my $id          = self!file-id($file, $dist-id);
            my $destination = $resources-dir.child($id);
            my $withoutext  = $basename.subst(/\.[exe|bat]$/, '');
            for '', '-j', '-m' -> $be {
                "$path/bin/$withoutext$be".IO.spurt:
                    $perl_wrapper.subst('#name#', $basename, :g).subst('#perl#', "perl6$be").subst('#dist-name#', $dist.name);
                if $is-win {
                    "$path/bin/$withoutext$be.bat".IO.spurt:
                        $windows_wrapper.subst('#perl#', "perl6$be", :g);
                }
                else {
                    "$path/bin/$withoutext$be".IO.chmod(0o755);
                }
            }
            self!add-short-name($basename, $dist);
            $dist.files{"bin/$basename"} = $id;
            copy($file, $destination);
        }

        for %resources.kv -> $name, $file is copy {
            $file              = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            # $name is 'libraries/p5helper' while $file is 'resources/libraries/libp5helper.so'
            my $id             = self!file-id($name, $dist-id) ~ '.' ~ $file.IO.extension;
            my $destination    = $resources-dir.child($id);
            $dist.files{$name} = $id;
            copy($file, $destination);
        }

        $dist-dir.child($dist-id).spurt: to-json($dist.Hash);

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
            for @provides -> ($source-name, $id) {
                my $source = $sources-dir.child($id);
                my $rev-deps-file = ($precomp.store.path($compiler-id, $id) ~ '.rev-deps').IO;
                my @rev-deps      = $rev-deps-file.e ?? $rev-deps-file.lines !! ();

                if %done{$id} {
                    note "(Already did $id)" if $verbose;
                    next;
                }
                note("Precompiling $id ($source-name)") if $verbose;
                $precomp.precompile(
                    $source.IO,
                    $id,
                    :source-name("$repo-prefix$source.relative($.prefix) ($source-name)"),
                );
                %done{$id} = 1;
                for @rev-deps -> $rev-dep-id {
                    if %done{$rev-dep-id} {
                        note "(Already did $rev-dep-id)" if $verbose;
                        next;
                    }
                    note("Precompiling rev-dep $rev-dep-id") if $verbose;
                    my $source = $sources-dir.child($rev-dep-id);
                    $precomp.precompile(
                        $source,
                        $rev-dep-id,
                        :force,
                        :source-name($repo-prefix ~ $source.relative($.prefix))
                    ) if $source.e;
                    %done{$rev-dep-id} = 1;
                }
            }
            PROCESS::<$REPO> := $head;
        }

        $lock.unlock;
    } ) }

    method uninstall(Distribution $dist) {
        my %provides      = $dist.provides;
        my %files         = $dist.files;
        my $sources-dir   = self.prefix.child('sources');
        my $resources-dir = self.prefix.child('resources');
        my $bin-dir       = self.prefix.child('bin');
        my $dist-dir      = self.prefix.child('dist');

        self!remove-dist-from-short-name-lookup-files($dist);
        $bin-dir.child($_.value).unlink for %files.grep: {$_.key ~~ /^bin\//};
        $sources-dir.child($_).unlink for %provides.map(*.value<pm><file>);
        $resources-dir.child($_).unlink for %files.values;
        $dist-dir.child($dist.id).unlink;
    }

    method files($file, :$name, :$auth, :$ver) {
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
                $dist<provides>{$spec.short-name}<pm pm6>.first(*.so)<file>
            );
            my $id = $loader.basename;
            return CompUnit.new(
                :handle(CompUnit::Handle),
                :short-name($spec.short-name),
                :version($dist<ver>),
                :auth($dist<auth> // Str),
                :repo(self),
                :repo-id($id),
                :distribution(Distribution.new(|$dist)),
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
                $dist<provides>{$spec.short-name}<pm pm6>.first(*.so)<file>
            );
            my $*RESOURCES = Distribution::Resources.new(:repo(self), :$dist-id);
            my $id = $loader.basename;
            my $repo-prefix = self!repo-prefix;
            my $handle = $precomp.try-load(
                $id,
                $loader,
                :source-name("$repo-prefix$loader.relative($.prefix) ($spec.short-name())"),
                :$spec,
                :@precomp-stores,
            );
            my $precompiled = defined $handle;
            $handle //= CompUnit::Loader.load-source-file($loader);

            my $compunit = CompUnit.new(
                :$handle,
                :short-name($spec.short-name),
                :version($dist<ver>),
                :auth($dist<auth> // Str),
                :repo(self),
                :repo-id($id),
                :$precompiled,
                :distribution(Distribution.new(|$dist)),
            );
            return %!loaded{$compunit.short-name} = $compunit;
        }
        return self.next-repo.need($spec, $precomp, :@precomp-stores) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method resource($dist-id, $key) {
        my $dist = from-json(self!dist-dir.child($dist-id).slurp);
        self!resources-dir.child($dist<files>{$key})
    }

    method id() {
        return $!id if $!id;
        $!id = self.CompUnit::Repository::Locally::id();
        my $dist-dir = $.prefix.child('dist');
        $!id = nqp::sha1($!id ~ ($dist-dir.e ?? $dist-dir.dir !! ''))
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
