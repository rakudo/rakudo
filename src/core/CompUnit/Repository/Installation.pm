class CompUnit::Repository::Installation does CompUnit::Repository::Locally does CompUnit::Repository::Installable {
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));
    has %!loaded;
    has $!precomp;

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
sub MAIN(:$name, :$auth, :$ver, *@, *%) {
    shift @*ARGS if $name;
    shift @*ARGS if $auth;
    shift @*ARGS if $ver;
    my @installations = @*INC.grep( { .starts-with("inst#") } )\
        .map: { CompUnit::Repository::Installation.new(PARSE-INCLUDE-SPEC($_).[*-1]) };
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
        $sources.mkdir;
        $sources
    }

    method !dist-dir() {
        my $dist = $.prefix.child('dist');
        $dist.mkdir;
        $dist
    }

    method !add-short-name($name, $dist) {
        my $short-dir = $.prefix.child('short');
        $short-dir.mkdir;
        my $id = nqp::sha1($name);
        my $lookup = $short-dir.child($id).open(:a);
        $lookup.say: $dist.id;
        $lookup.close;
    }

    method !manifest() {
        my $manifest := $.prefix.child("MANIFEST");
        my $repo = $manifest.e ?? from-json($manifest.slurp) !! {};
        $repo<file-count> //= 0;
        $repo
    }

    method install(:$dist!, *@files) {
        $!lock.protect( {
        my $path     = self.writeable-path or die "No writeable path found";

        my $d = CompUnitRepo::Distribution.new( |$dist.metainfo );

        my $lock //= $.prefix.child('repo.lock').open(:create, :w);
        $lock.lock(2);

        my $dist-dir    = self!dist-dir;
        if $dist-dir.child($d.id) ~~ :e {
            $lock.unlock;
            fail "$d already installed";
        }

        my $sources-dir = self!sources-dir;
        my $repo := self!manifest;
        my $file-id := $repo<file-count>;
        state $is-win //= $*DISTRO.is-win; # only look up once

        # Build patterns to choose what goes into "provides" section.
        my $ext = regex { [pm|pm6|jar|moarvm] };
        my @provides;
        for %($d.provides).kv -> $k, $v is copy {
            $v = $v.subst('\\', '/', :g);
            $v.=subst(/ [pm|pm6]? \.<$ext>$/, '.');
            @provides.push: regex { $v [ [pm|pm6] \. ]? <ext=.$ext> { make $k } }
        }

        # Initialize "provides" section.
        # when we do not use .kv, we error out when trying to store into Pairs
        $d.provides.kv.map: -> $k, $ { $d.provides.{$k} = {} }

        # Walk the to be installed files, decide whether we put them into
        # "provides" or just "files".
        my $has-provides;
        for @files -> $file is copy {
            $file = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            my $destination = $sources-dir.child($file-id);
            if [||] @provides>>.ACCEPTS($file) -> $/ {
                my $name = $/.ast;
                self!add-short-name($name, $d);
                $has-provides = True;
                $d.provides{ $name }{ $<ext> } = {
                    :file($file-id),
                    :time(try $file.IO.modified.Num),
                    :$!cver
                }
            }
            else {
                if $file ~~ /^bin<[\\\/]>/ {
                    mkdir "$path/bin" unless "$path/bin".IO.d;
                    my $basename   = $file.IO.basename;
                    my $withoutext = $basename;
                    $withoutext.=subst(/\.[exe|bat]$/, '');
                    for '','-j','-m' -> $be {
                        "$path/bin/$withoutext$be".IO.spurt:
                            $perl_wrapper.subst('#name#', $basename, :g).subst('#perl#', "perl6$be");
                        if $is-win {
                            "$path/bin/$withoutext$be.bat".IO.spurt:
                                $windows_wrapper.subst('#perl#', "perl6$be", :g);
                        }
                        else {
                            "$path/bin/$withoutext$be".IO.chmod(0o755);
                        }
                    }
                }
                $d.files{$file} = $file-id
            }
            copy($file, $destination);
            $file-id++;
        }

        provides-warning($is-win, $d.name) if !$has-provides and $d.files.keys.first(/^blib\W/);

        $dist-dir.child($d.id).spurt: to-json($d.Hash);

        "$path/MANIFEST".IO.spurt: to-json( $repo );

        my $precomp = self.precomp-repository;
        for $d.provides.values.map(*.values[0]<file>.Int).sort -> $file-id {
            my $source = $sources-dir.child($file-id);
            if $precomp.may-precomp {
                my $id = nqp::sha1($source ~ self.id);
                $precomp.precompile($source.IO, $id);
            }
        }
        $lock.unlock;
    } ) }

    method files($file, :$name, :$auth, :$ver) {
        my @candi;
        my $lookup = $.prefix.child('short').child(nqp::sha1($name));
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
                        $candi<files>{$file} = $.prefix.abspath ~ '/' ~ $candi<files>{$file}
                            unless $candi<files>{$file} ~~ /^$.prefix/;
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
                    my $loader = $.prefix.child('sources').child(
                        $dist<provides>{$spec.short-name}<pm pm6>.first(*.so)<file>
                    );
                    my $handle;
                    if $precomp.may-precomp {
                        my $id = nqp::sha1($loader ~ self.id);
                        if $*W and $*W.is_precompilation_mode {
                            say $id;
                            $handle = $precomp.load($id) or $precomp.precompile($loader, $id);
                        }
                        $handle //= $precomp.load($id);
                    }
                    $handle //= CompUnit::Loader.load-source-file($loader);
                    my $compunit = CompUnit.new(
                        :$handle,
                        :name($spec.short-name),
                        :repo(self),
                    );
                    return %!loaded{$compunit.name} = $compunit;
                }
            }
        }
        return self.next-repo.need($spec, $precomp) if self.next-repo;
        nqp::die("Could not find $spec in:\n" ~ $*REPO.repo-chain.map(*.Str).join("\n").indent(4));
    }

    method load(IO::Path:D $file) returns CompUnit:D {
        return self.next-repo.load($file) if self.next-repo;
        nqp::die("Could not find $file in:\n" ~ $*REPO.repo-chain.map(*.Str).join("\n").indent(4));
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
        my $color = %*ENV<RAKUDO_ERROR_COLOR> // !$is-win;
        my ($red, $green, $yellow, $clear) = $color
            ?? ("\e[31m", "\e[32m", "\e[33m", "\e[0m")
            !! ("", "", "", "");
        my $eject = $is-win ?? "<HERE>" !! "\x[23CF]";

        note "$red==={$clear}WARNING!$red===$clear
The distribution $name does not seem to have a \"provides\" section in its META.info file,
and so the packages will not be installed in the correct location.
Please ask the author to add a \"provides\" section, mapping every exposed namespace to a
file location in the distribution.
See http://design.perl6.org/S22.html#provides for more information.\n";
    }
}

# vim: ft=perl6 expandtab sw=4
