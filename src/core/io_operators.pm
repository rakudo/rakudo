my class IO::ArgFiles { ... }

sub print(|) {
    my $args := nqp::p6argvmarray();
    my $out := $*OUT;
    $out.print(nqp::shift($args)) while $args;
    Bool::True
}

proto sub say(|) { * }
multi sub say()              { $*OUT.print: "\n" }
multi sub say(Obsolete:D \o) { $*OUT.print: o.gist ~ "\n" }
multi sub say(Str:D \x)      { $*OUT.print: x ~ "\n" }
multi sub say(\x)            { $*OUT.print: x.gist ~ "\n" }
multi sub say(|) {
    my $args := nqp::p6argvmarray();
    my $out := $*OUT;
    $out.print(nqp::shift($args).gist) while $args;
    $out.print("\n");
}

proto sub note(|) { * }
multi sub note() {
    $*ERR.print("Noted\n");
}
multi sub note(Str:D \x) {
    my $err := $*ERR;
    $err.print(x);
    $err.print("\n");
}
multi sub note(\x) {
    my $err := $*ERR;
    $err.print(x.gist);
    $err.print("\n");
}
multi sub note(|) {
    my $args := nqp::p6argvmarray();
    my $err := $*ERR;
    $err.print(nqp::shift($args).gist) while $args;
    $err.print("\n");
}

sub gist(|) {
    nqp::p6parcel(nqp::p6argvmarray(), Mu).gist
}

sub prompt($msg) {
    my $out := $*OUT;
    $out.print($msg);
    $out.flush();
    $*IN.get;
}

proto sub dir(|) { * }
multi sub dir(Mu :$test, :$absolute) {
    DIR-GATHER($*CWD.Str, $test);
}
multi sub dir(:$Str!,Mu :$test, :$absolute) {
    $Str
      ?? DIR-GATHER-STR($*CWD.Str,$test)
      !! DIR-GATHER($*CWD.Str, $test);
}
multi sub dir($dir as Str, :$CWD = $*CWD, Mu :$test, :$absolute) {
    DIR-GATHER(
      MAKE-CLEAN-PARTS(MAKE-ABSOLUTE-PATH($dir,$CWD.Str)).join('/'), $test,
    );
}
multi sub dir($dir as Str, :$Str!, :$CWD = $*CWD, Mu :$test, :$absolute) {
    $Str
      ?? DIR-GATHER-STR(
           MAKE-CLEAN-PARTS(MAKE-ABSOLUTE-PATH($dir,$CWD.Str)).join('/'),
           $test)
      !! DIR-GATHER(
           MAKE-CLEAN-PARTS(MAKE-ABSOLUTE-PATH($dir,$CWD.Str)).join('/'),
           $test);
}

proto sub open(|) { * }
multi sub open( $path,:$r,:$w,:$rw,:$a,:$p,:$enc,:$nodepr,|c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc and !$nodepr;

    # we want a pipe
    if $p {
        DEPRECATED('pipe($path,...)',|<2014.12 2015.12>,:what(':p for pipe'))
          if !$nodepr;
        return pipe($path,:$enc,|c);
    }

    # we want a special handle
    elsif $path eq '-' {
        return IO::Dup.new( :fileno( +?($w || $rw) ) );
    }

    # want a normal handle
    my $abspath := MAKE-ABSOLUTE-PATH($path,$*CWD);
    fail X::IO::Directory.new(:$path, :trying<open>)
      if FILETEST-E($abspath) && FILETEST-D($abspath);

    my $mode := ($rw || $w) ?? 'w' !! ($a ?? 'wa' !! 'r' );
    # TODO: catch error, and fail()
    my Mu $PIO := nqp::open(nqp::unbox_s($abspath),nqp::unbox_s($mode));
    IO::Handle.new(:$abspath,:$PIO,:$enc,|c);
}

proto sub pipe(|) { * }
multi sub pipe( $command as Str,:$enc,:$nodepr,|c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc and !$nodepr;

    my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
    my Mu $hash-without := nqp::hash();
    my Mu $enviter := nqp::iterator($hash-with-containers);

    my $envelem;
    while $enviter {
        $envelem := nqp::shift($enviter);
        nqp::bindkey(
          $hash-without,
          nqp::iterkey_s($envelem),
          nqp::decont(nqp::iterval($envelem)),
        );
    }

    my str $errpath;   # what is this good for?
    # TODO: catch error, and fail()
    my $PIO := nqp::openpipe(
      nqp::unbox_s($command),
      nqp::unbox_s($*CWD.chop),
      $hash-without,
      $errpath,
    );

    IO::Pipe.new(:$command,:$PIO,:$enc,|c);
}

proto sub lines(|) { * }
multi sub lines($what = $*ARGFILES, $limit = Inf, *%named) {
    $limit == Inf || nqp::istype($limit,Whatever)
      ?? $what.lines(|%named)
      !! $what.lines($limit, |%named);
}

proto sub get(|) { * }
multi sub get($fh = $*ARGFILES) {
    $fh.get()
}

proto sub getc(|) { * }
multi sub getc($fh = $*ARGFILES) {
    $fh.getc()
}

proto sub close(|) { * }
multi sub close($fh) {
    $fh.close()
}

proto sub slurp(|) { * }
multi sub slurp(IO::ArgFiles:D $io = $*ARGFILES) {
    my $result := $io.slurp;
    $result // $result.throw;
}
multi sub slurp(IO::Handle:D $io = $*ARGFILES, :$enc, |c) {
    DEPRECATED('slurp($path,...)',|<2014.10 2015.10>,:what<slurp($handle,...)>);
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := $io.slurp-rest(:$enc,|c);
    $result // $result.throw;
}
multi sub slurp(Cool:D $path, :$enc, |c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := SLURP-PATH(MAKE-ABSOLUTE-PATH($path,$*CWD),:$enc,|c);
    $result // $result.throw;
}

proto sub spurt(|) { * }
multi sub spurt(IO::Handle:D $fh, $what,|c ) {
    DEPRECATED('spurt($path,...)',|<2014.10 2015.10>,:what<spurt($handle,...)>);
    my $result := $fh.spurt($what,:nodepr,|c);
    $result // $result.throw;
}
multi sub spurt(Cool:D $path,$what,:$enc,|c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := SPURT-PATH(MAKE-ABSOLUTE-PATH($path,$*CWD),$what,:$enc,|c);
    $result // $result.throw;
}

{
    sub chdir($path as Str) {
        nqp::chdir(nqp::unbox_s($path));
        $*CWD = IO::Path.new(cwd());
        return True;
        CATCH {
            default {
                X::IO::Chdir.new(
                    :$path,
                    os-error => .Str,
                ).throw;
            }
        }
    }
    PROCESS::<&chdir> := &chdir;
}

sub chdir($path as Str) {

    if !nqp::istype($*CWD,IO::Dir) {   # canary until 2014.10
        warn "\$*CWD is a {$*CWD.^name}, not an IO::Dir!!!";
        $*CWD = $*CWD.IO;
    }

    my $newCWD = CHANGE-DIRECTORY($path,$*CWD.Str,&FILETEST-X);
    $newCWD // $newCWD.throw;
    $*CWD = $newCWD;
}

sub indir($path as Str, $what, :$test = <r w>) {
    my $newCWD := CHANGE-DIRECTORY($path,$*CWD.Str,&FILETEST-RWX);
    $newCWD // $newCWD.throw;

    {
        my $*CWD = $newCWD;  # temp doesn't work in core settings :-(
        $what();
    }
}

sub tmpdir($path as Str) {
    my $newTMPDIR := CHANGE-DIRECTORY($path,$*TMPDIR.Str,&FILETEST-RWX);
    $newTMPDIR // $newTMPDIR.throw;
    $*TMPDIR = $newTMPDIR;
}

sub homedir($path as Str, :$test = <r w x>) {
    my $newHOME := CHANGE-DIRECTORY($path,$*HOME.Str,&FILETEST-RWX);
    $newHOME // $newHOME.throw;
    $*HOME = $newHOME;
}

sub chmod($mode, *@filenames, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my @ok;
    for @filenames -> $file {
        @ok.push($file) if $file.IO(:$SPEC,:$CWD).chmod($mode);
    }
    @ok;
#    @filenames.grep( *.IO(:$SPEC,:$CWD).chmod($mode) ).eager;
}
sub unlink(*@filenames, :$SPEC = $*SPEC, :$CWD = $*CWD)       {
    my @ok;
    for @filenames -> $file {
        @ok.push($file) if $file.IO(:$SPEC,:$CWD).unlink;
    }
    @ok;
#    @filenames.grep( *.IO(:$SPEC,:$CWD).unlink ).eager;
}
sub rmdir(*@filenames, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my @ok;
    for @filenames -> $file {
        @ok.push($file) if $file.IO(:$SPEC,:$CWD).rmdir;
    }
    @ok;
#    @filenames.grep( *.IO(:$SPEC,:$CWD).rmdir ).eager;
}

proto sub mkdir(|) { * }
multi sub mkdir(Int $mode, *@dirnames, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    @dirnames.grep( *.IO(:$SPEC,:$CWD).mkdir($mode) ).eager;
}
multi sub mkdir($path, $mode = 0o777, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    $path.IO(:$SPEC,:$CWD).mkdir($mode) ?? ($path,) !! ();
}

sub rename($from, $to, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my $result := $from.IO(:$SPEC,:$CWD).rename($to,:$SPEC,:$CWD);
    $result // $result.throw;
}
sub copy($from, $to, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my $result := $from.IO(:$SPEC,:$CWD).copy($to,:$SPEC,:$CWD);
    $result // $result.throw;
}
sub symlink($target, $name, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my $result := $target.IO(:$SPEC,:$CWD).symlink($name,:$SPEC,:$CWD);
    $result // $result.throw;
}
sub link($target, $name, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my $result := $target.IO(:$SPEC,:$CWD).link($name,:$SPEC,:$CWD);
    $result // $result.throw;
}

# deprecations
{
    sub cwd() {
        return nqp::p6box_s(nqp::cwd());
        CATCH { default {
            fail X::IO::Cwd.new( os-error => .Str,);
        } }
    }
#    PROCESS::<&cwd> := Deprecation.obsolete(
#      :name('&*cwd'),
#      :value(&cwd),
#      :instead('chdir'),
#    );
}

sub cwd() {
    DEPRECATED('$*CWD', |<2014.10 2015.10>);
    $*CWD.chop;
}

# vim: ft=perl6 expandtab sw=4
