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

sub dir(Cool $path = '.', Mu :$test = none('.', '..')) {
    $path.path.contents(:$test)
}

sub unlink($path as Str) {
    my $abspath = IO::Spec.rel2abs($path);
    nqp::unlink($abspath);
    return True;
    CATCH {
        default {
            X::IO::Unlink.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
}

sub rmdir($path as Str) {
    my $abspath = IO::Spec.rel2abs($path);
    nqp::rmdir($abspath);
    return True;
    CATCH {
        default {
            X::IO::Rmdir.new(
                :$path,
                os-error => .Str,
            ).throw;
        }
    }
}

proto sub open(|) { * }
multi sub open($path, :$r is copy, :$w is copy, :$rw, :$a, :$p, :$bin, :$chomp = Bool::True, :enc(:$encoding) = 'utf8') {
    IO::Handle.new.open($path, :$r, :$w, :$rw, :$a, :$p, :$bin, :$chomp, :$encoding);
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
multi sub slurp($filename, :$bin = False, :$enc = 'utf8') {
    $filename.IO.slurp(:$bin, :$enc);
}

multi sub slurp(IO::Handle $io = $*ARGFILES, :$bin, :$enc) {
    $io.slurp(:$bin, :$enc);
}

proto sub spurt(|) { * }
multi sub spurt(IO::Handle $fh,
                Cool $contents,
                :encoding(:$enc) = 'utf8',
                :$createonly,
                :$append) {
    $fh.spurt($contents, :$enc, :$createonly, :$append);
}
multi sub spurt(IO::Handle $fh,
                Blob $contents,
                :$createonly,
                :$append) {
    $fh.spurt($contents, :$createonly, :$append);
}

multi sub spurt(Cool $filename,
                Cool $contents,
                :encoding(:$enc) = 'utf8',
                :$createonly,
                :$append) {
    $filename.IO.spurt($contents, :$enc, :$createonly, :$append);
}

multi sub spurt(Cool $filename,
                Blob $contents,
                :$createonly,
                :$append) {
    $filename.IO.spurt($contents, :$createonly, :$append);
}

{
    proto sub cwd(|) { * }
    multi sub cwd() {
        return nqp::p6box_s(
            nqp::cwd()
        );
        CATCH {
            default {
                X::IO::Cwd.new(
                    os-error => .Str,
                ).throw;
            }
        }
    }
    PROCESS::<&cwd> := &cwd;
}

proto sub cwd(|) { * }
multi sub cwd() {
    $*CWD
} 

{
    proto sub chdir(|) { * }
    multi sub chdir($path as Str) {
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

proto sub mkdir(|) { * }
multi sub mkdir($path as Str, $mode = 0o777) {
    my $abspath = IO::Spec.rel2abs($path);
    nqp::mkdir($abspath, $mode);
    return True;
    CATCH {
        default {
            X::IO::Mkdir.new(
                :$path,
                :$mode,
                os-error => .Str,
            ).throw;
        }
    }
}

PROCESS::<$OUT> = open('-', :w);
PROCESS::<$IN>  = open('-');
PROCESS::<$ERR> = IO::Handle.new;
nqp::bindattr(nqp::decont(PROCESS::<$ERR>),
  IO::Handle, '$!PIO', nqp::getstderr());

sub rename(Cool $from as Str, Cool $to as Str) {
    my $absfrom = IO::Spec.rel2abs($from);
    my $absto = IO::Spec.rel2abs($to);
    nqp::rename(nqp::unbox_s($absfrom), nqp::unbox_s($absto));
    return True;
    CATCH {
        default {
            if .Str ~~ /'rename failed: '(.*)/ {
                X::IO::Rename.new(
                    :$from,
                    :$to,
                    os-error => $0.Str,
                ).throw;
            } else {
                die "Unexpected error: $_";
            }
        }
    }
}
sub copy(Cool $from as Str, Cool $to as Str) {
    my $absfrom = IO::Spec.rel2abs($from);
    my $absto = IO::Spec.rel2abs($to);
    nqp::copy(nqp::unbox_s($absfrom), nqp::unbox_s($absto));
    return True;
    CATCH {
        default {
            X::IO::Copy.new(
                :$from,
                :$to,
                os-error => .Str,
            ).throw;
        }
    }
}
sub symlink(Cool $target as Str, Cool $name as Str) {
    my $abstarget = IO::Spec.rel2abs($target);
    nqp::symlink(nqp::unbox_s($abstarget), nqp::unbox_s($name));
    return True;
    CATCH {
        default {
            X::IO::Symlink.new(
                :$target,
                :$name,
                os-error => .Str,
            ).throw;
        }
    }
}
sub link(Cool $target as Str, Cool $name as Str) {
    my $abstarget = IO::Spec.rel2abs($target);
    nqp::link(nqp::unbox_s($abstarget), nqp::unbox_s($name));
    return True;
    CATCH {
        default {
            X::IO::Link.new(
                :$target,
                :$name,
                os-error => .Str,
            ).throw;
        }
    }
}

sub chmod($mode, $filename) { $filename.path.absolute.chmod($mode); $filename }

# vim: ft=perl6 expandtab sw=4
