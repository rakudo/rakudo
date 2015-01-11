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
multi sub dir(Mu :$test) {
    DIR-GATHER($*CWD.Str, $test);
}
multi sub dir(:$Str!,Mu :$test) {
    $Str
      ?? DIR-GATHER-STR($*CWD.Str,$test)
      !! DIR-GATHER($*CWD.Str, $test);
}
multi sub dir($dir as Str, :$CWD = $*CWD, Mu :$test) {
    DIR-GATHER(
      MAKE-CLEAN-PARTS(
        MAKE-ABSOLUTE-PATH(FORWARD-SLASH($dir),$CWD.Str)
      ).join('/'), $test,
    );
}
multi sub dir($dir as Str, :$Str!, :$CWD = $*CWD, Mu :$test) {
    $Str
      ?? DIR-GATHER-STR(
           MAKE-CLEAN-PARTS(
             MAKE-ABSOLUTE-PATH(FORWARD-SLASH($dir),$CWD.Str)
           ).join('/'),$test
         )
      !! DIR-GATHER(
           MAKE-CLEAN-PARTS(
             MAKE-ABSOLUTE-PATH(FORWARD-SLASH($dir),$CWD.Str)
           ).join('/'),$test
         );
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
    my $abspath := MAKE-ABSOLUTE-PATH($path,$*CWD.Str);
    fail X::IO::Directory.new(:$path, :trying<open>)
      if FILETEST-e($abspath) && FILETEST-d($abspath);

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
multi sub slurp() {
    my $io := $*ARGFILES;
    my $result := nqp::istype($io,IO::ArgFiles) ?? $io.slurp !! $io.slurp-rest;
    $result // $result.throw;
}
multi sub slurp(IO::ArgFiles:D $io) {
    my $result := $io.slurp;
    $result // $result.throw;
}
multi sub slurp(PIO:D $io, :$enc, |c) {
    DEPRECATED('slurp($path,...)',|<2014.10 2015.10>,:what<slurp($handle,...)>);
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := $io.slurp-rest(:$enc,|c);
    $result // $result.throw;
}
multi sub slurp(Any:D $path as Str, :$enc, |c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := SLURP-PATH(MAKE-ABSOLUTE-PATH($path,$*CWD.Str),:$enc,|c);
    $result // $result.throw;
}

sub slurp-rest(PIO:D $io, :$enc, |c) {
    my $result := $io.slurp-rest(:$enc, |c);
    $result // $result.throw;
}

proto sub spurt(|) { * }
multi sub spurt(PIO:D $fh,\what,|c ) {
    DEPRECATED('spurt($path,...)',|<2014.10 2015.10>,:what<spurt($handle,...)>);
    my $result := $fh.spurt(what,:nodepr,|c);
    $result // $result.throw;
}
multi sub spurt(Any:D $path as Str,\what,:$enc,|c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := SPURT-PATH(MAKE-ABSOLUTE-PATH($path,$*CWD.Str),what,:$enc,|c);
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

    my $newCWD = CHANGE-DIRECTORY($path,$*CWD.Str,&FILETEST-x);
    $newCWD // $newCWD.throw;
    $*CWD = $newCWD;
}

sub indir($path as Str, $what) {
    my $newCWD := CHANGE-DIRECTORY($path,$*CWD.Str,&FILETEST-rwx);
    $newCWD // $newCWD.throw;

    {
        my $*CWD = $newCWD;  # temp doesn't work in core settings :-(
        $what();
    }
}

sub tmpdir($path as Str) {
    my $newTMPDIR := CHANGE-DIRECTORY($path,$*TMPDIR.Str,&FILETEST-rwx);
    $newTMPDIR // $newTMPDIR.throw;
    $*TMPDIR = $newTMPDIR;
}

sub homedir($path as Str) {
    my $newHOME := CHANGE-DIRECTORY($path,$*HOME.Str,&FILETEST-rwx);
    $newHOME // $newHOME.throw;
    $*HOME = $newHOME;
}

sub chmod($mode, *@filenames, :$CWD as Str = $*CWD) {
    @filenames.grep( { CHMOD-PATH(MAKE-ABSOLUTE-PATH($_,$CWD),$mode) } ).eager;
}
sub unlink(*@filenames, :$CWD as Str = $*CWD)       {
    @filenames.grep( { UNLINK-PATH(MAKE-ABSOLUTE-PATH($_,$CWD)) } ).eager;
}
sub rmdir(*@filenames, :$CWD as Str = $*CWD) {
    @filenames.grep( { REMOVE-DIR(MAKE-ABSOLUTE-PATH($_,$CWD)) } ).eager;
}

proto sub mkdir(|) { * }
multi sub mkdir(Int $mode, *@dirnames, :$CWD as Str = $*CWD) {
    @dirnames.grep( { MAKE-DIR(MAKE-ABSOLUTE-PATH($_,$CWD),$mode) } ).eager;
}
multi sub mkdir($path, $mode = 0o777, :$CWD as Str = $*CWD) {
    MAKE-DIR(MAKE-ABSOLUTE-PATH($path,$CWD),$mode);
}

sub rename($from, $to, :$CWD as Str = $*CWD, |c) {
    my $result := RENAME-PATH(
      MAKE-ABSOLUTE-PATH($from,$CWD),MAKE-ABSOLUTE-PATH($to,$CWD),|c
    );
    $result // $result.throw;
}
sub copy($from, $to, :$CWD as Str = $*CWD, |c) {
    my $result := COPY-FILE(
      MAKE-ABSOLUTE-PATH($from,$CWD),MAKE-ABSOLUTE-PATH($to,$CWD),|c
    );
    $result // $result.throw;
}
sub symlink($target, $name, :$CWD as Str = $*CWD) {
    my $result := SYMLINK-PATH(
      MAKE-ABSOLUTE-PATH($target,$CWD),MAKE-ABSOLUTE-PATH($name,$CWD)
    );
    $result // $result.throw;
}
sub link($target, $name, :$CWD as Str = $*CWD) {
    my $result := LINK-FILE(
      MAKE-ABSOLUTE-PATH($target,$CWD),MAKE-ABSOLUTE-PATH($name,$CWD)
    );
    $result // $result.throw;
}

sub cwd() {
    DEPRECATED('$*CWD', |<2014.10 2015.10>);
    $*CWD.chop;
}

# vim: ft=perl6 expandtab sw=4
