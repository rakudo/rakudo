class CompUnitRepo::Local::Installation does CompUnitRepo::Locally does CompUnit::Repository {
    has %!dists;
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));

    submethod BUILD(:$!IO, :$!lock, :$!WHICH, :$!next-repo) {
        my $manifest := $!IO.child("MANIFEST");
        my $abspath  := $!IO.abspath;
        %!dists{$abspath} = $manifest.e
          ?? from-json($manifest.slurp)
          !! {};
        %!dists{$abspath}<file-count> //= 0;
        %!dists{$abspath}<dist-count> //= 0;
        %!dists{$abspath}<dists>      //= [ ];
    }

    method writeable-path {
        %!dists.keys.first( *.IO.w )
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
        .map: { CompUnitRepo::Local::Installation.new(PARSE-INCLUDE-SPEC($_).[*-1]) };
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

    method install(:$dist!, *@files) {
        $!lock.protect( {
        my $path     = self.writeable-path or die "No writeable path found";
        my $repo     = %!dists{$path};
        my $file-id := $repo<file-count>;
        my $d        = CompUnitRepo::Distribution.new( |$dist.metainfo );
        state $is-win //= $*DISTRO.is-win; # only look up once
        if $repo<dists>.first({ ($_<name> // '') eq  ($d.name // '') &&
                                ($_<auth> // '') eq  ($d.auth // '') &&
                               ~($_<ver>  //  0) eq ~($d.ver  //  0) }) -> $installed {
            $d.id = $installed<id>
        }
        else {
            $d.id = $repo<dist-count>++
        }

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
            if [||] @provides>>.ACCEPTS($file) -> $/ {
                $has-provides = True;
                $d.provides{ $/.ast }{ $<ext> } = {
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
            copy($file, $path ~ '/' ~ $file-id);
            $file-id++;
        }

        if !$has-provides && $d.files.keys.first(/^blib\W/) {
            my $color = %*ENV<RAKUDO_ERROR_COLOR> // !$is-win;
            my ($red, $green, $yellow, $clear) = $color
                ?? ("\e[31m", "\e[32m", "\e[33m", "\e[0m")
                !! ("", "", "", "");
            my $eject = $is-win ?? "<HERE>" !! "\x[23CF]";

            note "$red==={$clear}WARNING!$red===$clear
The distribution $d.name() does not seem to have a \"provides\" section in its META.info file,
and so the packages will not be installed in the correct location.
Please ask the author to add a \"provides\" section, mapping every exposed namespace to a
file location in the distribution.
See http://design.perl6.org/S22.html#provides for more information.\n";
        }

        $repo<dists>[$d.id] = $d.Hash;

        # XXX Create path if needed.
        "$path/MANIFEST".IO.spurt: to-json( $repo )
    } ) }

    method files($file, :$name, :$auth, :$ver) {
        my @candi;
        for %!dists.kv -> $path, $repo {
            for @($repo<dists>) -> $dist {
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
                        $candi<files>{$file} = $path ~ '/' ~ $candi<files>{$file}
                            unless $candi<files>{$file} ~~ /^$path/;
                        @candi.push: $candi;
                    }
                }
            }
        }
        @candi
    }

    method candidates($name, :$file, :$auth, :$ver) {
        my @candi;
        for %!dists.kv -> $path, $repo {
            for @($repo<dists>) -> $dist {
                my $dver = $dist<ver>
                        ?? nqp::istype($dist<ver>,Version)
                            ?? $dist<ver>
                            !! Version.new( ~$dist<ver> )
                        !! Version.new('0');

                if (!$auth || $dist<auth> ~~ $auth)
                && (!$ver  || $dver ~~ $ver)
                && $dist<provides>{$name} {
                    my $candi   = %$dist;
                    $candi<ver> = $dver;
                    for $candi<provides>.kv -> $ln, $files {
                        for $files.kv -> $type, $file {
                            $candi<provides>{$ln}{$type}<file> = $path ~ '/' ~ $file<file>
                                unless $candi<provides>{$ln}{$type}<file> ~~ /^$path/
                        }
                    }
                    my $loader = $candi<provides>{$name}<pm pm6>.first(*.so)<file>;
                    with $candi<provides>{$name}{$*VM.precomp-ext} -> $pc {
                        if $*PERL<compiler>.version eqv $pc<cver> {
                            return CompUnit.new($loader, :has_precomp($pc<file>), :repo(self));
                        }
                    }
                    return CompUnit.new($loader, :repo(self));
                }
            }
        }
        ();
    }

    method short-id() { 'inst' }
}
