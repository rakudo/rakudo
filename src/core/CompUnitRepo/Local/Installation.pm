class CompUnitRepo::Local::Installation does CompUnitRepo::Locally {
    has $!repo;
    has $!cver = nqp::hllize(nqp::atkey(nqp::gethllsym('perl6', '$COMPILER_CONFIG'), 'version'));
    has $!abspath;

    my %extensions =
      Perl6 => <pm6 pm>,
      Perl5 => <pm5 pm>,
      NQP   => <nqp>,
      JVM   => ();

    method serialize() {
        my Mu $sh := nqp::list_s();
        my $name   = 'CURLI_' ~ nqp::time_n();
        my Mu $sc := nqp::createsc(nqp::unbox_s($name));
        nqp::setobjsc($!repo, $sc);
        nqp::scsetobj($sc, 0, $!repo);
        my $serialized = nqp::serialize($sc, $sh);
        nqp::scdisclaim($sc);
        nqp::shift_s($sh); # strip null string which is at front
        $name ~ "\n" ~ nqp::p6box_s(nqp::join("\n", $sh)) ~ "\n" ~ $serialized
    }

    method deserialize() {
        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably
        my $manifest      := $!IO.child("MANIFEST.$precomp-ext");
        return Hash unless $manifest.e;
        my $b64            = $manifest.slurp;
        my Mu $sh         := nqp::list_s();
        my @lines          = $b64.split("\n");
        my str $name       = nqp::unbox_s(@lines.shift);
        my str $serialized = nqp::unbox_s(@lines.pop);
        nqp::push_s($sh, nqp::null_s());
        nqp::push_s($sh, nqp::unbox_s($_)) for @lines;

        my Mu $sc := nqp::createsc(nqp::unbox_s($name));
        my $conflicts := nqp::list();
        my Mu $obj;
        try {
            nqp::deserialize($serialized, $sc, $sh, nqp::list(), $conflicts);
            $obj := nqp::scgetobj($sc, 0);
        }
        nqp::scdisclaim($sc);
        $obj || Hash
    }

    method BUILD(:$!IO, :$!lock, :$!WHICH) {
        $!abspath           := $!IO.abspath;
        $!repo               = self.deserialize();
        $!repo<file-count> //= 0;
        $!repo<dist-count> //= 0;
        $!repo<dists>      //= [ ];
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
sub MAIN(:$name, :$auth, Str() :$ver = "", *@, *%) {
    shift @*ARGS if $name;
    shift @*ARGS if $auth;
    shift @*ARGS if $ver;
    my @installations = @*INC.grep( { .starts-with("inst#") } ).map({ INCLUDE-SPEC2CUR($_) });
    my @binaries = @installations>>.files(\'bin/#name#\', :$name, :$auth, :$ver);
    unless +@binaries {
        @binaries = @installations>>.files(\'bin/#name#\');
        if +@binaries {
            note q:to/SORRY/;
                ===SORRY!===
                No candidate found for \'#name#\' that match your criteria.
                Did you perhaps mean one of these?
                SORRY
            my %caps = :name(["Distribution", 12]), :auth(["Author(ity)", 11]), :ver(["Version", 7]);
            for @binaries -> $dist {
                for %caps.kv -> $caption, @opts is rw {
                    @opts[1] = max @opts[1], $dist."$caption"().Str.chars
                }
            }
            note "  " ~ %caps.values.map({ sprintf(\'%-*s\', .[1], .[0]) }).join(" | ");
            for @binaries -> $dist {
                note "  " ~ %caps.kv.map( -> $k, $v { sprintf(\'%-*s\', $v.[1], $dist."$k"()) } ).join(" | ")
            }
        }
        else {
            note "===SORRY!===\nNo candidate found for \'#name#\'.\n";
        }
        exit 1;
    }

    exit run($*EXECUTABLE_NAME, @binaries[0].files<bin/#name#>, @*ARGS).exitcode
}';

    method install(:$dist!, *@files) {
        $!lock.protect( {
        state $is-win        //= $*DISTRO.is-win; # only look up once
        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably
        my $file-id           := $!repo<file-count>;
        my $d                  = Distribution.new( |$dist.metainfo );
        $d.id                  = $!repo<dist-id>{$d.auth}{$d.name}{$d.ver} //= $!repo<dist-count>++;

        # Build patterns to choose what goes into "provides" section.
        my $ext = regex { [pm|pm6|jar|moarvm] };
        my @provides;
        for %($d.provides).kv -> $k, $v is copy {
            $v = $v.subst('\\', '/', :g);
            $v.=subst(/ [pm|pm6]? \.<$ext>$/, '.');
            @provides.push: regex { $v [ [pm|pm6] \. ]? <ext=.$ext> { make $k } }
        }

        # Initialize "provides" section.
        for %($d.provides).kv -> $k, $v is rw {
            # when we do not use .kv, we error out when trying to store into Pairs
            $v = {};
        }

        # Walk the to be installed files, decide whether we put them into
        # "provides" or just "files".
        my $has-provides;
        for @files -> $file is copy {
            $file = $is-win ?? ~$file.subst('\\', '/', :g) !! ~$file;
            # This is a module that shall be 'use'd.
            if [||] @provides>>.ACCEPTS($file) -> $/ {
                $has-provides = True;
                my $name      = $/.ast;
                my $extension = ~$<ext>;
                # Create a CompUnit and attach it to the Distribution.
                if $extension eq $precomp-ext {
                    $d.provides{$name}<precomp>
                        = CompUnit.new("$!abspath/$file-id", :$name, :auth($d.auth), :ver($d.ver),
                        :$extension, :!has-source, :has-precomp);
                    copy($file, "$!abspath/$file-id.$extension");
                }
                else {
                    $d.provides{$name}<source>
                        = CompUnit.new("$!abspath/$file-id", :$name, :auth($d.auth), :ver($d.ver),
                        :$extension, :has-source);
                    copy($file, "$!abspath/$file-id");
                }
            }
            # This is a file we might want to expose via %?RESOURCE.
            else {
                if $file ~~ /^bin<[\\\/]>/ {
                    mkdir "$!abspath/bin" unless "$!abspath/bin".IO.d;
                    my $basename   = $file.IO.basename;
                    my $withoutext = $basename;
                    $withoutext.=subst(/\.[exe|bat]$/, '');
                    # Create wrapper scripts that we put in PATH.
                    for '', < -p -j -m > -> $be {
                        "$!abspath/bin/$withoutext$be".IO.spurt:
                            $perl_wrapper.subst('#name#', $basename, :g).subst('#perl#', "perl6$be");
                        if $is-win {
                            "$!abspath/bin/$withoutext$be.bat".IO.spurt:
                                $windows_wrapper.subst('#perl#', "perl6$be", :g);
                        }
                        else {
                            "$!abspath/bin/$withoutext$be".IO.chmod(0o755);
                        }
                    }
                }
                $d.files{$file} = "$!abspath/$file-id";
                copy($file, "$!abspath/$file-id");
            }
            $file-id++;
        }

        # Put provided CompUnits in a hash table of the repository, prefer precompiled CUs.
        for $d.provides.keys -> $name {
            $!repo<provides>{$name}.push: $d.provides{$name}<precomp> // $d.provides{$name}<source>;
        }

        # Warn about not sufficient meta information.
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

        # Add the Distribution to the repository.
        $!repo<dists>[$d.id] = $d;

        # XXX Create path if needed.
        "$!abspath/MANIFEST.$precomp-ext".IO.spurt: self.serialize();
    } ) }

    method files($file, :$name, :$auth, :$ver) {
        my @candi;
        for @($!repo<dists>) -> $dist {
            if (!$name || $dist.name ~~ $name)
            && (!$auth || $dist.auth ~~ $auth)
            && (!$ver  || $dist.ver  ~~ $ver)
            && $dist.files{$file} {
                @candi.push: $dist;
            }
        }
        @candi
    }

    method candidates($name, :$file, :$auth, :$ver) {
        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably
        my @candi;
        for @($!repo<provides>{$name}) -> $cu {
            if (!$auth || $cu.auth ~~ $auth)
            && (!$ver  || $cu.ver  ~~ $ver) {
                @candi.push: $cu
            }
        }
        @candi.sort: { $^b.ver cmp $^a.ver }
    }

    method short-id() { 'inst' }
}
