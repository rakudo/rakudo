my class IO::ArgFiles { ... }

proto sub print(|) { * }
multi sub print(*@args) {
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.Str)) for @args;
    $*OUT.print($str);
    Bool::True
}
multi sub print(Str:D \x) {
    $*OUT.print(x);
}

# Once we have an nqp::say that looks at the *output* line separator of the
# PIO, then we can stop concatenating .nl to each string before .print, but
# instead call nqp::say directly.

proto sub say(|) { * }
multi sub say() { $*OUT.print-nl }
multi sub say(Str:D \x) {
    my $out := $*OUT;
    my str $str = nqp::concat(nqp::unbox_s(x),$out.nl);
    $out.print($str);
}
multi sub say(Mu:D \args) {
    my $out := $*OUT;
    my str $str;
    my \iterator := nqp::istype(args, Iterable) ?? args.iterator !! args.list.iterator;
    until (my \value := iterator.pull-one) =:= IterationEnd {
        $str = nqp::concat($str,nqp::unbox_s(value.gist));
    }
    $str = nqp::concat($str,$out.nl);
    $out.print($str);
}
multi sub say(**@args is rw) {
    my $out := $*OUT;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.gist)) for @args;
    $str = nqp::concat($str,$out.nl);
    $out.print($str);
}

proto sub note(|) { * }
multi sub note() {
    my $err := $*ERR;
    my str $str = nqp::concat("Noted",$err.nl);
    $err.print($str);
}
multi sub note(Str:D \x) {
    my $err := $*ERR;
    my str $str = nqp::concat(nqp::unbox_s(x),$err.nl);
    $err.print($str);
}
multi sub note(\args) {
    my $err := $*ERR;
    my str $str;
    my \iterator := nqp::istype(args, Iterable) ?? args.iterator !! args.list.iterator;
    until (my \value := iterator.pull-one) =:= IterationEnd {
        $str = nqp::concat($str,nqp::unbox_s(value.gist));
    }
    $str = nqp::concat($str,$err.nl);
    $err.print($str);
}
multi sub note(**@args is rw) {
    my $err := $*ERR;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.gist)) for @args;
    $str = nqp::concat($str,$err.nl);
    $err.print($str);
}

sub gist(|) {
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified',
        nqp::p6argvmarray()).gist
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
    nqp::istype($limit,Whatever) || $limit == Inf
      ?? $what.lines(|%named)
      !! $what.lines($limit, |%named);
}

proto sub words(|) { * }
multi sub words($what, $limit = Inf, *%named) {
    nqp::istype($limit,Whatever) || $limit == Inf
      ?? $what.words(|%named)
      !! $what.words($limit, |%named);
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
    DEPRECATED('slurp($path,...)',|<2014.10 2015.09>,:what<slurp($handle,...)>);
    my $result := $io.slurp-rest(:$bin, :$enc, |c);
    $result // $result.throw;
}
multi sub slurp(Cool:D $path, :$bin = False, :$enc = 'utf8', |c) {
    my $result := $path.IO.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}

proto sub spurt(|) { * }
multi sub spurt(IO::Handle $fh, $contents, :$enc = 'utf8', |c ) {
    DEPRECATED('spurt($path,...)',|<2014.10 2015.09>,:what<spurt($handle,...)>);
    my $result := $fh.spurt($contents, :$enc, :nodepr, |c);
    $result // $result.throw;
}
multi sub spurt(Cool $path, $contents, :$enc = 'utf8', |c) {
    my $result := $path.IO.spurt($contents, :$enc, |c);
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

sub rename($from, $to, :$SPEC = $*SPEC, :$CWD = $*CWD, :$createonly) {
    my $result := $from.IO(:$SPEC,:$CWD).rename($to,:$SPEC,:$CWD,:$createonly);
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
    DEPRECATED('$*CWD', |<2014.10 2015.09>);
    $*CWD;
}

# vim: ft=perl6 expandtab sw=4
