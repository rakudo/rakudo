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

sub dir(Cool $path = '.', |c) {
    $path.IO.dir(|c)
}

proto sub open(|) { * }
multi sub open($path, :$r, :$w, :$rw, :$a, :$p, :$bin, :$chomp = True, :$enc = 'utf8') {
    my $handle = IO::Handle.new;
    $handle.open($path,:$r,:$w,:$rw,:$a,:$p,:$bin,:$chomp,:$enc) && $handle;
}

proto sub lines(|) { * }
multi sub lines($what = $*ARGFILES, $limit = Inf, *%named) {
    $limit == Inf || $limit ~~ Whatever
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
multi sub slurp(IO::Handle $io = $*ARGFILES, :$bin, :$enc = 'utf8', |c) {
    my $result := $io.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}
multi sub slurp($path, :$bin = False, :$enc = 'utf8', |c) {
    my $result := $path.IO.slurp(:$bin, :$enc, |c);
    $result // $result.throw;
}

proto sub spurt(|) { * }
multi sub spurt(IO::Handle $fh, $what, :$enc = 'utf8', |c ) {
    my $result := $fh.spurt($what, :$enc, |c);
    $result // $result.throw;
}
multi sub spurt(Cool $path, $what, :$enc = 'utf8', |c) {
    my $result := $path.IO.spurt($what, :$enc, |c);
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

proto sub chdir(|) { * }
multi sub chdir(IO::Path:D $path) { chdir $path.Str }
multi sub chdir($path as Str) {
    my $newpath = IO::Path.new(IO::Spec.canonpath($path));
    if $newpath.is-relative {
        my $tmp = $*CWD;
        for IO::Spec.splitdir($newpath) -> $segment {
            given $segment {
                when '..' { $tmp .= parent; }
                when '.' { }
                default { $tmp .= child($segment); }
            }
        }
        $newpath = $tmp;
    }
    if $newpath.d {
        $*CWD = $newpath; 
    } else {
        X::IO::Chdir.new(
            path => $newpath,
            os-error => 'Directory does not exist'
        ).throw;
    }
}

PROCESS::<$OUT> = open('-', :w);
PROCESS::<$IN>  = open('-');
PROCESS::<$ERR> = IO::Handle.new;
nqp::bindattr(nqp::decont(PROCESS::<$ERR>),
  IO::Handle, '$!PIO', nqp::getstderr());

sub chmod($mode, *@filenames, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    @filenames.grep( *.IO(:$SPEC,:$CWD).chmod($mode) ).eager;
}
sub unlink(*@filenames, :$SPEC = $*SPEC, :$CWD = $*CWD)       {
    @filenames.grep( *.IO(:$SPEC,:$CWD).unlink ).eager;
}
sub rmdir(*@filenames, :$SPEC = $*SPEC, :$CWD = $*CWD) {
    @filenames.grep( *.IO(:$SPEC,:$CWD).rmdir ).eager;
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
    DEPRECATED('$*CWD');
    $*CWD;
} 

# vim: ft=perl6 expandtab sw=4
