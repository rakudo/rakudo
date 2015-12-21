my class IO::ArgFiles { ... }

proto sub print(|) { * }
multi sub print(Str:D \x) {
    $*OUT.print(x);
}
multi sub print(\x) {
    $*OUT.print(x.Str);
}
multi sub print(**@args is raw) {
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.Str)) for @args;
    $*OUT.print($str);
}

# Once we have an nqp::say that looks at the *output* line separator of the
# PIO, then we can stop concatenating .nl-out to each string before .print, but
# instead call nqp::say directly.

proto sub say(|) { * }
multi sub say() { $*OUT.print-nl }
multi sub say(Str:D \x) {
    my $out := $*OUT;
    my str $str = nqp::concat(nqp::unbox_s(x),$out.nl-out);
    $out.print($str);
}
multi sub say(\x) {
    my $out := $*OUT;
    my str $str = nqp::concat(nqp::unbox_s(x.gist),$out.nl-out);
    $out.print($str);
}
multi sub say(**@args is raw) {
    my $out := $*OUT;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.gist)) for @args;
    $str = nqp::concat($str,$out.nl-out);
    $out.print($str);
}

proto sub put(|) { * }
multi sub put() { $*OUT.print-nl }
multi sub put(Str:D \x) {
    my $out := $*OUT;
    my str $str = nqp::concat(nqp::unbox_s(x),$out.nl-out);
    $out.print($str);
}
multi sub put(\x) {
    my $out := $*OUT;
    my str $str = nqp::concat(nqp::unbox_s(x.Str),$out.nl-out);
    $out.print($str);
}
multi sub put(**@args is raw) {
    my $out := $*OUT;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.Str)) for @args;
    $str = nqp::concat($str,$out.nl-out);
    $out.print($str);
}

proto sub note(|) { * }
multi sub note() {
    my $err := $*ERR;
    my str $str = nqp::concat("Noted",$err.nl-out);
    $err.print($str);
}
multi sub note(Str:D \x) {
    my $err := $*ERR;
    my str $str = nqp::concat(nqp::unbox_s(x),$err.nl-out);
    $err.print($str);
}
multi sub note(**@args is raw) {
    my $err := $*ERR;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.gist)) for @args;
    $str = nqp::concat($str,$err.nl-out);
    $err.print($str);
}

sub gist(|) {
    my \args := nqp::p6argvmarray();
    nqp::elems(args) == 1
        ?? nqp::atpos(args, 0).gist
        !! nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', args).gist
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
multi sub open(IO() $path, |c) {
    IO::Handle.new(:$path).open(|c);
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
multi sub slurp(Cool:D $path, :$bin = False, :$enc = 'utf8', |c) {
    my $result := $path.IO.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}

sub spurt(Cool $path, $contents, :$enc = 'utf8', |c) {
    my $result := $path.IO.spurt($contents, :$enc, |c);
    $result // $result.throw;
}

{
    sub chdir(Str() $path) {
        nqp::chdir(nqp::unbox_s($path));
        $*CWD = IO::Path.new(nqp::cwd());
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
sub copy($from, $to, :$SPEC = $*SPEC, :$CWD = $*CWD, :$createonly) {
    my $result := $from.IO(:$SPEC,:$CWD).copy($to,:$SPEC,:$CWD, :$createonly);
    $result // $result.throw;
}
sub move($from, $to, :$createonly) {
    try {
        copy($from, $to, :$createonly);
        unlink($from);
        return True;

        CATCH {
            when X::IO::Copy|X::IO::Unlink {
                fail X::IO::Move.new(
                    :from(.from),
                    :to(.to),
                    :os-error(.os-error),
                );
            }
        }
    }
}
sub symlink($target, $name, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my $result := $target.IO(:$SPEC,:$CWD).symlink($name,:$SPEC,:$CWD);
    $result // $result.throw;
}
sub link($target, $name, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    my $result := $target.IO(:$SPEC,:$CWD).link($name,:$SPEC,:$CWD);
    $result // $result.throw;
}

# vim: ft=perl6 expandtab sw=4
