class CompUnitRepo::Local::Installation does CompUnitRepo::Locally {
    has %!dists;
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));

    method BUILD(:$!IO, :$!lock, :$!WHICH) {
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
#perl# "%~dp0\%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
#perl# "%~dp0\%0" %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have Perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem \';
__END__
:endofperl
';
    my $perl_wrapper = '#!/usr/bin/env #perl#
sub MAIN(:$name, :$auth, :$ver, *@pos, *%named) {
    my @binaries = CompUnitRepo.files(\'bin/#name#\', :$name, :$auth, :$ver);
    unless +@binaries {
        @binaries = CompUnitRepo.files(\'bin/#name#\');
        if +@binaries {
            note q:to/SORRY/;
                ===SORRY!===
                No candidate found for \'#name#\' that match your criteria.
                Did you perhaps mean one of these?
                SORRY
            my %caps = :name([\'Distribution\', 12]), :auth([\'Author(ity)\', 11]), :ver([\'Version\', 7]);
            for @binaries -> $dist {
                for %caps.kv -> $caption, @opts is rw {
                    @opts[1] = max @opts[1], ($dist{$caption} // \'\').Str.chars
                }
            }
            note \'  \' ~ %caps.values.map({ sprintf(\'%-*s\', .[1], .[0]) }).join(\' | \');
            for @binaries -> $dist {
                note \'  \' ~ %caps.kv.map( -> $k, $v { sprintf(\'%-*s\', $v.[1], $dist{$k}) } ).join(\' | \')
            }
        }
        else {
            note "===SORRY!===\nNo candidate found for \'#name#\'.\n";
        }
        exit 1;
    }

    my $options = join(\' \', %named.map({\'--\' ~ .key ~ \'=\' ~ .value}), @pos);
    exit shell("$*EXECUTABLE_NAME {@binaries[0]<files><bin/#name#>} $options").exit
}';

    method install(:$dist!, *@files) {
        $!lock.protect( {
        my $path     = self.writeable-path or die "No writeable path found";
        my $repo     = %!dists{$path};
        my $file-id := $repo<file-count>;
        my $d        = CompUnitRepo::Distribution.new( |$dist.metainfo );
        if $repo<dists>.first({ ($_<name> // '') eq  ($d.name // '') &&
                                ($_<auth> // '') eq  ($d.auth // '') &&
                               ~($_<ver>  //  0) eq ~($d.ver  //  0) }) -> $installed {
            $d.id = $installed<id>
        }
        else {
            $d.id = $repo<dist-count>++
        }
        
        # Build patterns to choose what goes into "provides" section.
        my $ext = regex { [pm|pm6|pir|pbc|jar|moarvm] };
        my @provides;
        for %($d.provides).kv -> $k, $v is copy {
            $v.=subst(/\.<$ext>$/, '.');
            @provides.push: regex { $v <ext=.$ext> { make $k } }
        }
        
        # Initialize "provides" section.
        for %($d.provides).kv -> $k, $v is rw {
            # when we do not use .kv, we error out when trying to store into Pairs
            $v = {};
        }
        
        # Walk the to be installed files, decide whether we put them into
        # "provides" or just "files".
        for @files -> $file is copy {
            $file.=Str;
            if [||] @provides>>.ACCEPTS($file) -> $/ {
                $d.provides{ $/.ast }{ $<ext> } = {
                    :file($file-id),
                    :time(try $file.IO.modified),
                    :$!cver
                }
            }
            else {
                if $file ~~ /^bin<[\\\/]>/ {
                    mkdir "$path/bin" unless "$path/bin".IO.d;
                    my $basename   = $file.IO.path.basename;
                    my $withoutext = $basename;
                    $withoutext.=subst(/\.[exe|bat]$/, '');
                    for '', < -p -j -m > -> $be {
                        "$path/bin/$withoutext$be".IO.spurt:
                            $perl_wrapper.subst('#name#', $basename, :g).subst('#perl#', "perl6$be");
                        if $*DISTRO.is-win {
                            "$path/bin/$withoutext$be.bat".IO.spurt:
                                $windows_wrapper.subst('#perl#', "perl6$be");
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
        
        $repo<dists>[$d.id] = $d.Hash;
        
        # XXX Create path if needed.
        "$path/MANIFEST".IO.spurt: to-json( $repo )
    } ) }

    method files($file, :$name, :$auth, :$ver) {
        my @candi;
        for %!dists.kv -> $path, $repo {
            for @($repo<dists>) -> $dist {
                my $dver = $dist<ver>
                        ?? $dist<ver> ~~ Version
                            ?? $dist<ver>
                            !! Version.new( $dist<ver> )
                        !! Version.new('0');
                
                if (!$name || $dist<name> ~~ $name)
                && (!$auth || $dist<auth> ~~ $auth)
                && (!$ver  || $dver ~~ $ver)
                && $dist<files>{$file} {
                    my $candi   = %$dist;
                    $candi<ver> = $dver;
                    $candi<files>{$file} = $path ~ '/' ~ $candi<files>{$file}
                        unless $candi<files>{$file} ~~ /^$path/;
                    @candi.push: $candi;
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
                        ?? $dist<ver> ~~ Version
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
                    @candi.push: $candi;
                }
            }
        }
        @candi
    }

    method short-id() { 'inst' }
}
