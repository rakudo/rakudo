class CompUnit::Repository::Installation does CompUnit::Repository::Locally does CompUnit::Repository::Installable {
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));
    has %!loaded;
    has $!precomp;
    has $!id;

    submethod BUILD(:$!prefix, :$!lock, :$!WHICH, :$!next-repo) { }

    method writeable-path {
        $.prefix.w ?? $.prefix !! IO::Path;
    }

    method can-install() {
        $.prefix.w
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

    exit run($*EXECUTABLE-NAME, @binaries[0].hash.<files><bin/#name#>, @*ARGS).exitcode
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
        my $lookup = $short-dir.child($id).open(:a);
        $lookup.say: $dist.id;
        $lookup.close;
    }

    method !precomp-id(Str $name) {
        my $id = $name ~ self.path-spec;
        $id ~= ',' ~ self.next-repo.id if self.next-repo;
        nqp::sha1($id)
    }

    method install(Distribution $dist, %sources, %scripts?, %resources?, :$force) {
        $!lock.protect( {
        my @*MODULES;
        my $path   = self.writeable-path or die "No writeable path found";
        my $lock //= $.prefix.child('repo.lock').open(:create, :w);
        $lock.lock(2);

        my $dist-dir = self!dist-dir;
        if not $force and $dist-dir.child($dist.id) ~~ :e {
            $lock.unlock;
            fail "$dist already installed";
        }

        my $sources-dir   = self!sources-dir;
        my $resources-dir = self!resources-dir;
        my $bin-dir       = self!bin-dir;
        state $is-win   //= $*DISTRO.is-win; # only look up once

        # Walk the to be installed files, decide whether we put them into
        # "provides" or just "files".
        for %sources.kv -> $name, $file is copy {
            $file           = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            my $id          = self!precomp-id($file);
            my $destination = $sources-dir.child($id);
            self!add-short-name($name, $dist);
            $dist.provides{ $name } = {
                pm => {
                    :file($id),
                    :time(try $file.IO.modified.Num),
                    :$!cver
                }
            };
            copy($file, $destination);
        }

        for %scripts.kv -> $basename, $file is copy {
            $file           = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            my $id          = self!precomp-id($file);
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
            my $id             = self!precomp-id($file) ~ '.' ~ $file.IO.extension;
            my $destination    = $resources-dir.child($id);
            $dist.files{$name} = $id;
            copy($file, $destination);
        }

        $dist-dir.child($dist.id).spurt: to-json($dist.Hash);

        my $precomp = $*REPO.precomp-repository;
        for $dist.provides.values.map(*.values[0]<file>) -> $file-id {
            my $source = $sources-dir.child($file-id);
            if $precomp.may-precomp {
                my $id            = self!precomp-id($source.Str);
                my $rev-deps-file = ($precomp.store.path($*PERL.compiler.id, $id) ~ '.rev-deps').IO;
                my @rev-deps      = $rev-deps-file.e ?? $rev-deps-file.lines !! ();

                $precomp.precompile($source.IO, $id);
                for @rev-deps -> $rev-dep-id {
                    my $source = $sources-dir.child($rev-dep-id);
                    $precomp.precompile($source, $rev-dep-id) if $source.e;
                }
            }
        }

        # reset cached id so it's generated again on next access.
        # identity changes with every installation of a dist.
        $!id = Any;
        $lock.unlock;
    } ) }

    method files($file, :$name, :$auth, :$ver) {
        my @candi;
        my $prefix = self.prefix;
        my $lookup = $prefix.child('short').child(nqp::sha1($name));
        if $lookup.e {
            my $dist-dir = self!dist-dir;
            for $lookup.lines -> $dist-id {
                my $dist = from-json($dist-dir.child($dist-id).slurp);
                my $dver = $dist<ver>
                        ?? nqp::istype($dist<ver>,Version)
                            ?? $dist<ver>
                            !! Version.new( $dist<ver> )
                        !! Version.new('0');

                if (!$name || $dist<name> ~~ $name)
                && (!$auth || $dist<auth> ~~ $auth)
                && (!$ver  || $dver ~~ $ver) {
                    with $dist<files>{$file} {
                        my $candi   = %$dist;
                        $candi<ver> = $dver;
                        $candi<files>{$file} = $prefix.abspath ~ '/resources/' ~ $candi<files>{$file}
                            unless $candi<files>{$file} ~~ /^$prefix/;
                        @candi.push: $candi;
                    }
                }
            }
        }
        @candi
    }

    method need(
        CompUnit::DependencySpecification $spec,
        CompUnit::PrecompilationRepository $precomp = self.precomp-repository(),
    )
        returns CompUnit:D
    {
        my $lookup = $.prefix.child('short').child(nqp::sha1($spec.short-name));
        if $lookup.e {
            my $dist-dir = self!dist-dir;
            for $lookup.lines -> $dist-id {
                my $dist = from-json($dist-dir.child($dist-id).slurp);
                my $dver = $dist<ver>
                        ?? nqp::istype($dist<ver>,Version)
                            ?? $dist<ver>
                            !! Version.new( ~$dist<ver> )
                        !! Version.new('0');

                if $dist<auth> ~~ $spec.auth-matcher
                    && $dver ~~ $spec.version-matcher
                    && $dist<provides>{$spec.short-name}
                {
                    return %!loaded{$spec.short-name} if %!loaded{$spec.short-name}:exists;
                    my $loader = $.prefix.child('sources').child(
                        $dist<provides>{$spec.short-name}<pm pm6>.first(*.so)<file>
                    );
                    my $*RESOURCES = Distribution::Resources.new(:repo(self), :$dist-id);
                    my $handle;
                    my $id = self!precomp-id($loader.Str);
                    if $precomp.may-precomp {
                        say "$id $loader" if $*W and $*W.is_precompilation_mode;
                        $handle = (
                            $precomp.load($id, :since($loader.modified)) # already precompiled?
                            or $precomp.precompile($loader, $id) and $precomp.load($id) # if not do it now
                        );
                    }
                    my $precompiled = defined $handle;
                    $handle //= CompUnit::Loader.load-source-file($loader);
                    my $compunit = CompUnit.new(
                        :$handle,
                        :short-name($spec.short-name),
                        :version($dver),
                        :auth($dist<auth> // Str),
                        :repo(self),
                        :repo-id($id),
                        :$precompiled,
                        :distribution(Distribution.new(|$dist)),
                    );
                    return %!loaded{$compunit.short-name} = $compunit;
                }
            }
        }
        return self.next-repo.need($spec, $precomp) if self.next-repo;
        X::CompUnit::UnsatisfiedDependency.new(:specification($spec)).throw;
    }

    method load(IO::Path:D $file) returns CompUnit:D {
        return self.next-repo.load($file) if self.next-repo;
        nqp::die("Could not find $file in:\n" ~ $*REPO.repo-chain.map(*.Str).join("\n").indent(4));
    }

    method resource($dist-id, $key) {
        my $dist = from-json(self!dist-dir.child($dist-id).slurp);
        self!resources-dir.child($dist<files>{$key})
    }

    method id() {
        return $!id if $!id;
        $!id = self.CompUnit::Repository::Locally::id();
        $!id = nqp::sha1($!id ~ self!dist-dir.dir)
    }

    method short-id() { 'inst' }

    method loaded() returns Iterable {
        return %!loaded.values;
    }

    method precomp-repository() returns CompUnit::PrecompilationRepository {
        $!precomp := CompUnit::PrecompilationRepository::Default.new(
            :store(
                CompUnit::PrecompilationStore::File.new(
                    :prefix(self.prefix.child('precomp')),
                )
            ),
        ) unless $!precomp;
        $!precomp
    }

    sub provides-warning($is-win, $name) {
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
