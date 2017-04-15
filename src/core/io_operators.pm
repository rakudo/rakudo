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
    $out.print(nqp::concat(nqp::unbox_s(x),$out.nl-out));
}
multi sub say(\x) {
    my $out := $*OUT;
    $out.print(nqp::concat(nqp::unbox_s(x.gist),$out.nl-out));
}
multi sub say(**@args is raw) {
    my $out := $*OUT;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.gist)) for @args;
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
    my $out := $*OUT;
    my str $str;
    $str = nqp::concat($str,nqp::unbox_s(.Str)) for @args;
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
multi sub open(IO() $path, |c) { IO::Handle.new(:$path).open(|c) }

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

proto sub spurt(|) { * }
multi sub spurt(IO() $path, |c) { $path.spurt(|c) }

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

        nqp::if(
            nqp::stmts(
                nqp::unless(
                    nqp::unless(nqp::isfalse($d), $path.d),
                    fail X::IO::Chdir.new: :$path, :os-error(
                        nqp::if($path.e, 'is not a directory', 'does not exist')
                    )
                ),
                nqp::unless(
                    nqp::unless(nqp::isfalse($r), $path.r),
                    fail X::IO::Chdir.new: :$path,
                        :os-error("did not pass :r test")
                ),
                nqp::unless(
                    nqp::unless(nqp::isfalse($w), $path.w),
                    fail X::IO::Chdir.new: :$path,
                        :os-error("did not pass :w test")
                ),
                nqp::unless(
                    nqp::unless(nqp::isfalse($x), $path.x),
                    fail X::IO::Chdir.new: :$path,
                        :os-error("did not pass :x test")
                ),
                my $*CWD = $path,
            ),
            what
        )
    }
}

PROCESS::<$IN> =
  IO::Handle.new(:path(IO::Special.new('<STDIN>'))).open;
PROCESS::<$OUT> =
  IO::Handle.new(:path(IO::Special.new('<STDOUT>'))).open;
PROCESS::<$ERR> =
  IO::Handle.new(:path(IO::Special.new('<STDERR>'))).open;

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
sub mkdir(IO() $path, Int() $mode = 0o777) { $path.mkdir($mode) }

sub rename(IO() $from, IO() $to, :$createonly) {
    $from.rename($to, :$createonly)
}
sub copy(IO() $from, IO() $to, :$createonly) { $from.copy($to, :$createonly) }
sub move(IO() $from, IO() $to, :$createonly) { $from.move($to, :$createonly) }

sub symlink(IO() $target, IO() $name) { $target.symlink($name) }
sub    link(IO() $target, IO() $name) { $target   .link($name) }

# vim: ft=perl6 expandtab sw=4
