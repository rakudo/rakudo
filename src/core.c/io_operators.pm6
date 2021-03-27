my class IO::ArgFiles { ... }

proto sub printf($, |) {*}
multi sub printf(Str(Cool) $format, Junction:D \j) {
    my $out := $*OUT;
    j.THREAD: { $out.print: sprintf $format, |$_ }
}
multi sub printf(Str(Cool) $format, |) {
   my $args := nqp::p6argvmarray;
   nqp::shift($args);
   $*OUT.print: sprintf $format, nqp::hllize($args)
}

proto sub print(|) {*}
multi sub print(--> True) { }    # nothing to do
multi sub print(Junction:D \j) {
    my $out := $*OUT;
    j.THREAD: { $out.print: .Str }
}
multi sub print(Str:D \x) { $*OUT.print(x) }
multi sub print(\x) { $*OUT.print(x.Str) }
multi sub print(|) {
    $*OUT.print:
      nqp::join("",Rakudo::Internals.StrList2list_s(nqp::p6argvmarray))
}

# To ensure that classes that mimic the $*OUT / $*ERR API (which are only
# required to provide a ".print" method), all logic is done in the subs
# here, and then passed on to the .print method.
proto sub say(|) {*}
multi sub say() {
    $_ := $*OUT;
    .print: .nl-out
}
multi sub say(\x) {
    $_ := $*OUT;
    .print: nqp::concat(x.gist,.nl-out)
}
multi sub say(|) {
    my $parts := Rakudo::Internals.GistList2list_s(nqp::p6argvmarray);
    $_ := $*OUT;
    nqp::push_s($parts,.nl-out);
    .print: nqp::join("",$parts)
}

proto sub put(|) {*}
multi sub put() {
    $_ := $*OUT;
    .print: .nl-out
}
multi sub put(Junction:D \j) {
    my $out := $*OUT;
    j.THREAD: { $out.print: nqp::concat(.Str,$out.nl-out) }
}
multi sub put(\x) {
    $_ := $*OUT;
    .print: nqp::concat(x.Str,.nl-out)
}
multi sub put(|) {
    my $parts := Rakudo::Internals.StrList2list_s(nqp::p6argvmarray);
    $_ := $*OUT;
    nqp::push_s($parts,.nl-out);
    .print: nqp::join("",$parts)
}

proto sub note(|) {*}
multi sub note() {
    $_ := $*ERR;
    .print: nqp::concat("Noted",.nl-out)
}
multi sub note(\x) {
    $_ := $*ERR;
    .print: nqp::concat(x.gist,.nl-out)
}
multi sub note(|) {
    my $parts := Rakudo::Internals.GistList2list_s(nqp::p6argvmarray);
    $_ := $*ERR;
    nqp::push_s($parts,.nl-out);
    .print: nqp::join("",$parts)
}

proto sub gist(|) {*}
multi sub gist(|) {
    my \args := nqp::p6argvmarray();
    nqp::elems(args) == 1
        ?? nqp::atpos(args, 0).gist
        !! nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', args).gist
}

proto sub prompt($?, *%) {*}
multi sub prompt() {
    nqp::defined(my \res := $*IN.get) ?? val(res) !! res;
}
multi sub prompt($msg) {
    my $out := $*OUT;
    $out.print($msg);
    $out.flush();
    nqp::defined(my \res := $*IN.get) ?? val(res) !! res;
}

proto sub dir(|) {*}
multi sub dir(IO() $path, :$test!) { $path.dir(:$test) }
multi sub dir(IO() $path         ) { $path.dir         }
multi sub dir(:$test!) { IO::Path.new($*SPEC.curdir).dir(:$test) }
multi sub dir(       ) { IO::Path.new($*SPEC.curdir).dir         }

proto sub open($, |) {*}
multi sub open(IO() $path, |c) { IO::Handle.new(:$path).open(|c) }

proto sub lines($?, $?, *%) {*}
multi sub lines(*%_) {
    nqp::elems(nqp::getattr(%_,Map,'$!storage'))
      ?? $*ARGFILES.lines(|%_)
      !! $*ARGFILES.lines
}
multi sub lines($what, *%_) {
    nqp::elems(nqp::getattr(%_,Map,'$!storage'))
      ?? $what.lines(|%_)
      !! $what.lines
}
multi sub lines($what, $number, *%_) {
    nqp::elems(nqp::getattr(%_,Map,'$!storage'))
      ?? $what.lines($number, |%_)
      !! $what.lines($number)
}

proto sub words($?, $?, *%) {*}
multi sub words(*%_) {
    nqp::elems(nqp::getattr(%_,Map,'$!storage'))
      ?? $*ARGFILES.words(|%_)
      !! $*ARGFILES.words
}
multi sub words($what, *%_) {
    nqp::elems(nqp::getattr(%_,Map,'$!storage'))
      ?? $what.words(|%_)
      !! $what.words
}
multi sub words($what, $number, *%_) {
    nqp::elems(nqp::getattr(%_,Map,'$!storage'))
      ?? $what.words($number, |%_)
      !! $what.words($number)
}

proto sub get  ($?, *%) {*}
multi sub get  (IO::Handle:D $fh = $*ARGFILES) { $fh.get  }

proto sub getc ($?, *%) {*}
multi sub getc (IO::Handle:D $fh = $*ARGFILES) { $fh.getc }

proto sub close($, *%) {*}
multi sub close(IO::Handle:D $fh) { $fh.close }
multi sub close(Channel:D $channel) { $channel.close }

proto sub slurp(|) {*}
multi sub slurp(*%_) { $*ARGFILES.slurp(|%_) }
multi sub slurp(IO::Handle:D $fh, *%_) { $fh.slurp(|%_) }
multi sub slurp(IO() $path, :$bin!) { $path.slurp(:$bin) }
multi sub slurp(IO() $path, :$enc ) { $path.slurp(:$enc) }
multi sub slurp(IO() $path        ) { $path.slurp(:enc<utf8>) }

proto sub spurt($, |) {*}
# Don't do anything special for the IO::Handle, as using spurt() as a sub
# when you've gone through the trouble of creating an IO::Handle, is not
# so likely, as you would probably just call the .spurt method on the handle.
multi sub spurt(IO::Handle:D $fh, $data, *%_) is default {
    $fh.spurt($data, |%_)
}
multi sub spurt(IO() $path) {
    $path.spurt
}
multi sub spurt(IO() $path, Blob:D \data, :$append!) {
    $path.spurt(data, :$append)
}
multi sub spurt(IO() $path, Blob:D \data, :$createonly!) {
    $path.spurt(data, :$createonly)
}
multi sub spurt(IO() $path, Blob:D \data) {
    $path.spurt(data)
}
multi sub spurt(IO() $path, \text, :$append!, :$enc) {
    $path.spurt(text, :$append, :$enc)
}
multi sub spurt(IO() $path, \text, :$createonly!, :$enc) {
    $path.spurt(text, :$createonly, :$enc)
}
multi sub spurt(IO() $path, \text, :$enc!) { $path.spurt(text, :$enc) }
multi sub spurt(IO() $path, \text        ) { $path.spurt(text, :enc<utf8>) }

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

proto sub chdir(|) {*}
multi sub chdir(|c) {
    nqp::istype(($_ := $*CWD.chdir(|c)),Failure) ?? $_ !! ($*CWD = $_)
}

proto sub indir($, $, *%) {*}
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

# Set up the standard STDIN/STDOUT/STDERR by first setting up the skeletons
# of the IO::Handle objects that can be setup at compile time.  Then, when
# running the mainline of the setting at startup, plug in the low level
# handles and set up the encoder and decoders.  This shaves off about 1.5%
# of bare startup.
{
    my constant NL-IN    = ["\x0A", "\r\n"];
    my constant NL-OUT   = "\n";
    my constant ENCODING = "utf8";

    my sub setup-handle(str $what) {
        my $handle := nqp::p6bindattrinvres(
          nqp::create(IO::Handle),IO::Handle,'$!path',nqp::p6bindattrinvres(
            nqp::create(IO::Special),IO::Special,'$!what',$what
          )
        );
        nqp::getattr($handle,IO::Handle,'$!chomp')    = True;
        nqp::getattr($handle,IO::Handle,'$!nl-in')    = NL-IN;
        nqp::getattr($handle,IO::Handle,'$!nl-out')   = NL-OUT;
        nqp::getattr($handle,IO::Handle,'$!encoding') = ENCODING;
        $handle
    }

    # Set up the skeletons at compile time
    my constant STDIN  = setup-handle('<STDIN>');
    my constant STDOUT = setup-handle('<STDOUT>');
    my constant STDERR = setup-handle('<STDERR>');

    my sub activate-handle(Mu \HANDLE, Mu \PIO) {
        nqp::setbuffersizefh(PIO,8192) unless nqp::isttyfh(PIO);

        my $encoding = Encoding::Registry.find(ENCODING);
        nqp::bindattr(
          HANDLE,IO::Handle,'$!decoder',$encoding.decoder(:translate-nl)
        ).set-line-separators(NL-IN);
        nqp::bindattr(
          HANDLE,IO::Handle,'$!encoder',$encoding.encoder(:translate-nl)
        );
        nqp::p6bindattrinvres(HANDLE,IO::Handle,'$!PIO',PIO)
    }

    # Activate the skeletons at runtime
    PROCESS::<$IN>  = activate-handle(STDIN,  nqp::getstdin);
    PROCESS::<$OUT> = activate-handle(STDOUT, nqp::getstdout);
    PROCESS::<$ERR> = activate-handle(STDERR, nqp::getstderr);
}

proto sub chmod($, |) {*}
multi sub chmod($mode, *@filenames) {
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.chmod($mode) }
    @ok;
}

proto sub unlink(|) {*}
multi sub unlink(*@filenames) {
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.unlink }
    @ok;
}

proto sub rmdir(|) {*}
multi sub rmdir(*@filenames) {
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.rmdir }
    @ok;
}

proto sub mkdir($, $?, *%) {*}
multi sub mkdir(IO() $path, Int() $mode = 0o777) { $path.mkdir($mode) }

proto sub rename($, $, *%) {*}
multi sub rename(IO() $from, IO() $to, :$createonly) {
    $from.rename($to, :$createonly)
}

proto sub copy($, $, *%) {*}
multi sub copy(IO() $from, IO() $to, :$createonly) {
    $from.copy($to, :$createonly)
}

proto sub move($, $, *%) {*}
multi sub move(IO() $from, IO() $to, :$createonly) {
    $from.move($to, :$createonly)
}

proto sub symlink($, $, *%) {*}
multi sub symlink(IO() $target, IO() $name, Bool :$absolute = True) {
    $target.symlink($name, :$absolute)
}

proto sub link($, $, *%) {*}
multi sub link(IO() $target, IO() $name) { $target.link($name) }

# vim: expandtab shiftwidth=4
