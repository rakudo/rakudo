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

proto sub say(|) { * }
multi sub say() { $*OUT.print-nl }
multi sub say(Str:D \x) {
    my $out := $*OUT;
    $out.print(nqp::concat(nqp::unbox_s(x),$out.nl-out));
}
multi sub say(\x) {
    my $out := $*OUT;
    $out.print(nqp::concat(nqp::unbox_s(x.gist),$out.nl-out));
}
multi sub say(**@args is raw) {
    my str $str;
    my $iter := @args.iterator;
    nqp::until(
      nqp::eqaddr(($_ := $iter.pull-one), IterationEnd),
      $str = nqp::concat($str, nqp::unbox_s(.gist)));
    my $out := $*OUT;
    $out.print(nqp::concat($str,$out.nl-out));
}

proto sub put(|) { * }
multi sub put() { $*OUT.print-nl }
multi sub put(Str:D \x) {
    my $out := $*OUT;
    $out.print(nqp::concat(nqp::unbox_s(x),$out.nl-out));
}
multi sub put(\x) {
    my $out := $*OUT;
    $out.print(nqp::concat(nqp::unbox_s(x.Str),$out.nl-out));
}
multi sub put(**@args is raw) {
    my str $str;
    my $iter := @args.iterator;
    nqp::until(
      nqp::eqaddr(($_ := $iter.pull-one), IterationEnd),
      $str = nqp::concat($str, nqp::unbox_s(.Str)));
    my $out := $*OUT;
    $out.print(nqp::concat($str,$out.nl-out));
}

proto sub note(|) { * }
multi sub note() {
    my $err := $*ERR;
    $err.print(nqp::concat("Noted",$err.nl-out));
}
multi sub note(Str:D \x) {
    my $err := $*ERR;
    $err.print(nqp::concat(nqp::unbox_s(x),$err.nl-out));
}
multi sub note(**@args is raw) {
    my $err := $*ERR;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.gist)) for @args;
    $err.print(nqp::concat($str,$err.nl-out));
}

sub gist(|) {
    my \args := nqp::p6argvmarray();
    nqp::elems(args) == 1
        ?? nqp::atpos(args, 0).gist
        !! nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', args).gist
}

multi sub prompt() {
    $*IN.get
}
multi sub prompt($msg) {
    my $out := $*OUT;
    $out.print($msg);
    $out.flush();
    $*IN.get;
}

proto sub dir(|) { * }
multi sub dir(*%_) { $*SPEC.curdir.IO.dir(:!absolute, |%_) }
multi sub dir(IO::Path:D $path, |c) { $path.dir(|c) }
multi sub dir(IO()       $path, |c) { $path.dir(|c) }

proto sub open(|) { * }
multi sub open(IO() $path, |c) { IO::Handle.new(:$path).open(|c) }

proto sub lines(|) { * }
multi sub lines($what = $*ARGFILES, |c) { $what.lines(|c) }

proto sub words(|) { * }
multi sub words($what = $*ARGFILES, |c) { $what.words(|c) }

proto sub get  (|) { * }
multi sub get  (IO::Handle:D $fh = $*ARGFILES) { $fh.get  }

proto sub getc (|) { * }
multi sub getc (IO::Handle:D $fh = $*ARGFILES) { $fh.getc }

proto sub close(|) { * }
multi sub close(IO::Handle:D $fh) { $fh.close }

proto sub slurp(|) { * }
multi sub slurp(IO::Handle:D $fh = $*ARGFILES, |c) { $fh.slurp(|c) }
multi sub slurp(IO() $path, |c) { $path.slurp(|c) }

proto sub spurt(|) { * }
multi sub spurt(IO::Handle:D $fh,   |c) { $fh  .spurt(|c) }
multi sub spurt(IO()       $path, |c) { $path.spurt(|c) }

{
    sub chdir(IO() $path) {
        CATCH {
            default {
                return Failure.new: X::IO::Chdir.new: :$path, :os-error(.Str);
            }
        }
        nqp::chdir(nqp::unbox_s($path.absolute));
        $*CWD = IO::Path.new(nqp::cwd());
    }
    PROCESS::<&chdir> := &chdir;
}

sub chdir(|c) {
    nqp::if(nqp::istype(($_ := $*CWD.chdir(|c)), Failure), $_, $*CWD = $_)
}

proto sub indir(|) {*}
multi sub indir(IO() $path, &what, :$test!) {
    DEPRECATED(
        :what<:$test argument>,
        'individual named parameters (e.g. :r, :w, :x)',
        "v2017.03.101.ga.5800.a.1", "v6.d", :up(*),
    );
    indir $path, &what, |$test.words.map(* => True).Hash;
}
multi sub indir(IO() $path, &what, :$d = True, :$r, :$w, :$x) {
    {   # NOTE: we need this extra block so that the IO() coercer doesn't
        # use our (empty at the time) $*CWD when making the IO::Path object
        nqp::stmts(
          $d && nqp::isfalse($path.d) && X::IO::Chdir.new(
            :$path, :os-error(
              $path.e ?? 'is not a directory' !! 'does not exist')).fail,
          $r && nqp::isfalse($path.r) && X::IO::Chdir.new(
            :$path, :os-error("did not pass :r test")).fail,
          $w && nqp::isfalse($path.w) && X::IO::Chdir.new(
            :$path, :os-error("did not pass :w test")).fail,
          $x && nqp::isfalse($path.x) && X::IO::Chdir.new(
            :$path, :os-error("did not pass :x test")).fail,
          # $*CWD gets stringified with .Str in IO::Path.new, so we need to
          # ensure it's set to an absolute path
          my $*CWD = $path.WHAT.new: $path.absolute,
            :SPEC($path.SPEC), :CWD($path.SPEC.rootdir))
        && what
    }
}

PROCESS::<$IN> =
  IO::Handle.new(:path(IO::Special.new('<STDIN>'))).open;
PROCESS::<$OUT> =
  IO::Handle.new(:path(IO::Special.new('<STDOUT>'))).open;
PROCESS::<$ERR> =
  IO::Handle.new(:path(IO::Special.new('<STDERR>'))).open;

sub chmod($mode, *@filenames) {
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.chmod($mode) }
    @ok;
}
sub unlink(*@filenames) {
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.unlink }
    @ok;
}
sub rmdir(*@filenames) {
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.rmdir }
    @ok;
}
sub mkdir(IO() $path, Int() $mode = 0o777) { $path.mkdir($mode) }

sub rename(IO() $from, IO() $to, :$createonly) {
    $from.rename($to, :$createonly)
}
sub copy(IO() $from, IO() $to, :$createonly) { $from.copy($to, :$createonly) }
sub move(IO() $from, IO() $to, :$createonly) { $from.move($to, :$createonly) }

sub symlink(IO() $target, IO() $name) { $target.symlink($name) }
sub    link(IO() $target, IO() $name) { $target   .link($name) }

# vim: ft=perl6 expandtab sw=4
