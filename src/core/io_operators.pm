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
multi sub dir(Str() $dir, Str() :$CWD = $*CWD, Mu :$test) {
    DIR-GATHER(
      MAKE-CLEAN-PARTS(
        MAKE-ABSOLUTE-PATH(FORWARD-SLASH($dir),$CWD)
      ).join('/'), $test,
    );
}
multi sub dir(Str() $dir, :$Str!, Str() :$CWD = $*CWD, Mu :$test) {
    $Str
      ?? DIR-GATHER-STR(
           MAKE-CLEAN-PARTS(
             MAKE-ABSOLUTE-PATH(FORWARD-SLASH($dir),$CWD)
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
multi sub pipe(Str() $command,:$enc,:$nodepr,|c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc and !$nodepr;

    my str $errpath;   # what is this good for?
    # TODO: catch error, and fail()
    my $PIO := nqp::openpipe(
      nqp::unbox_s($command),
      nqp::unbox_s($*CWD.chop),
      CLONE-HASH-DECONTAINERIZED(%*ENV),
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
multi sub slurp(Str() $path, :$enc, |c) {
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
multi sub spurt(Str() $path,\what,:$enc,|c) {
    DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
      if $enc;
    my $result := SPURT-PATH(MAKE-ABSOLUTE-PATH($path,$*CWD.Str),what,:$enc,|c);
    $result // $result.throw;
}

{
    sub chdir(Str() $path) {
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

sub chdir(Str() $path, Str() $CWD = $*CWD) {
    my $newCWD := CHANGE-DIRECTORY($path,$CWD,&FILETEST-x);
    return $newCWD if nqp::istype($newCWD,Failure);

    $*CWD = $newCWD;
}

sub indir(Str() $path, &what, Str() $CWD = $*CWD) {
    my $newCWD := CHANGE-DIRECTORY($path,$CWD,&FILETEST-rwx);
    return $newCWD if nqp::istype($newCWD,Failure);

    { # need separate scope to prevent confusion with default $CWD
        my $*CWD = $newCWD;
        what();
    }
}

sub tmpdir(Str() $path, Str() $CWD = $*CWD) {
    my $newTMPDIR := CHANGE-DIRECTORY($path,$CWD,&FILETEST-rwx);
    return $newTMPDIR if nqp::istype($newTMPDIR,Failure);

    $*TMPDIR; # make sure we have a PROCESS:: one
    $*TMPDIR = $newTMPDIR;
}

sub homedir(Str() $path, Str() $CWD = $*CWD) {
    my $newHOME := CHANGE-DIRECTORY($path,$CWD,&FILETEST-rwx);
    return $newHOME if nqp::istype($newHOME,Failure);

    $*HOME; # make sure we have a PROCESS:: one
    $*HOME = $newHOME;
}

sub chmod($mode, *@filenames, Str() :$CWD = $*CWD) {
    @filenames.grep( { CHMOD-PATH(MAKE-ABSOLUTE-PATH($_,$CWD),$mode) } ).eager;
}
sub unlink(*@filenames, Str() :$CWD = $*CWD)       {
    @filenames.grep( { UNLINK-PATH(MAKE-ABSOLUTE-PATH($_,$CWD)) } ).eager;
}
sub rmdir(*@filenames, Str() :$CWD = $*CWD) {
    @filenames.grep( { REMOVE-DIR(MAKE-ABSOLUTE-PATH($_,$CWD)) } ).eager;
}

proto sub mkdir(|) { * }
multi sub mkdir(Int $mode, *@dirnames, Str() :$CWD = $*CWD) {
    @dirnames.grep( { MAKE-DIR(MAKE-ABSOLUTE-PATH($_,$CWD),$mode) } ).eager;
}
multi sub mkdir($path, $mode = 0o777, Str() :$CWD = $*CWD) {
    MAKE-DIR(MAKE-ABSOLUTE-PATH($path,$CWD),$mode);
}

sub rename($from, $to, Str() :$CWD = $*CWD, |c) {
    my $result := RENAME-PATH(
      MAKE-ABSOLUTE-PATH($from,$CWD),MAKE-ABSOLUTE-PATH($to,$CWD),|c
    );
    $result // $result.throw;
}
sub move($from, $to, Str() :$CWD = $*CWD, |c) {
    my $result := MOVE-PATH(
      MAKE-ABSOLUTE-PATH($from,$CWD),MAKE-ABSOLUTE-PATH($to,$CWD),|c
    );
    $result // $result.throw;
}
sub copy($from, $to, Str() :$CWD = $*CWD, |c) {
    my $result := COPY-FILE(
      MAKE-ABSOLUTE-PATH($from,$CWD),MAKE-ABSOLUTE-PATH($to,$CWD),|c
    );
    $result // $result.throw;
}
sub symlink($target, $name, Str() :$CWD = $*CWD) {
    my $result := SYMLINK-PATH(
      MAKE-ABSOLUTE-PATH($target,$CWD),MAKE-ABSOLUTE-PATH($name,$CWD)
    );
    $result // $result.throw;
}
sub link($target, $name, Str() :$CWD = $*CWD) {
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
