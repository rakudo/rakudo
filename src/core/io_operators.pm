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
multi sub dir(*%_) {
    $*SPEC.curdir.IO.dir(:!absolute, |%_)
}
multi sub dir(IO::Path:D $path, |c) {
    $path.dir(|c)
}
multi sub dir(Cool $path, |c) {
    $path.IO.dir(|c)
}

proto sub open(|) { * }
multi sub open($path, :$chomp = True, :$enc = 'utf8', |c) {
    my $handle = IO::Handle.new(:path($path.IO));
    $handle // $handle.throw;
    $handle.open(:$chomp,:$enc,|c);
}

proto sub pipe(|) { * }
multi sub pipe($path, :$chomp = True, :$enc = 'utf8', |c) {
    my $handle = IO::Handle.new(:path($path.IO));
    $handle // $handle.throw;
    $handle.pipe(:$chomp,:$enc,|c);
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
multi sub slurp(IO::ArgFiles:D $io = $*ARGFILES, :$bin, :$enc = 'utf8', |c) {
    my $result := $io.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}
multi sub slurp(IO::Handle:D $io = $*ARGFILES, :$bin, :$enc = 'utf8', |c) {
    DEPRECATED('slurp($path,...)',|<2014.10 2015.10>,:what<slurp($handle,...)>);
    my $result := $io.slurp-rest(:$bin, :$enc, |c);
    $result // $result.throw;
}
multi sub slurp(Cool:D $path, :$bin = False, :$enc = 'utf8', |c) {
    my $result := $path.IO.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}

proto sub spurt(|) { * }
multi sub spurt(IO::Handle $fh, $what, :$enc = 'utf8', |c ) {
    DEPRECATED('spurt($path,...)',|<2014.10 2015.10>,:what<spurt($handle,...)>);
    my $result := $fh.spurt($what, :$enc, :nodepr, |c);
    $result // $result.throw;
}
multi sub spurt(Cool $path, $what, :$enc = 'utf8', |c) {
    my $result := $path.IO.spurt($what, :$enc, |c);
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

sub chdir(Str() $path, :$test = 'r') {

    if !nqp::istype($*CWD,IO::Path) {   # canary until 2014.10
        warn "\$*CWD is a {$*CWD.^name}, not an IO::Path!!!";
        $*CWD = $*CWD.IO;
    }

    my $newCWD := $*CWD.chdir($path,:$test);
    $newCWD // $newCWD.throw;

    $*CWD = $newCWD;
}

sub indir(Str() $path, $what, :$test = <r w>) {
    my $newCWD := $*CWD.chdir($path,:$test);
    $newCWD // $newCWD.throw;

    {
        my $*CWD = $newCWD;  # temp doesn't work in core settings :-(
        $what();
    }
}

sub tmpdir(Str() $path, :$test = <r w x>) {
    my $newTMPDIR := $*TMPDIR.chdir($path,:$test);
    $newTMPDIR // $newTMPDIR.throw;

    $*TMPDIR = $newTMPDIR;
}

sub homedir(Str() $path, :$test = <r w x>) {
    my $newHOME := $*HOME.chdir($path,:$test);
    $newHOME // $newHOME.throw;

    $*HOME = $newHOME;
}

PROCESS::<$IN> =
  IO::Handle.new(:path(IO::Special.new(:what(<< <STDIN>  >>)))).open;
PROCESS::<$OUT> =
  IO::Handle.new(:path(IO::Special.new(:what(<< <STDOUT> >>)))).open;
PROCESS::<$ERR> =
  IO::Handle.new(:path(IO::Special.new(:what(<< <STDERR> >>)))).open;

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

sub cwd() {
    DEPRECATED('$*CWD', |<2014.10 2015.10>);
    $*CWD;
}

# vim: ft=perl6 expandtab sw=4
