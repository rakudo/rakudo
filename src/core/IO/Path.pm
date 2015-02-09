my class Instant { ... }

my class IO::Path is Cool {
    has IO::Spec $.SPEC;
    has Str      $.CWD;
    has Str      $.path;
    has Bool $!is-absolute;
    has Str  $!abspath;  # should be native for faster file tests, but segfaults
    has Bool $!e;
    has %!parts;

    multi method ACCEPTS(IO::Path:D: IO::Path:D \other) {
        nqp::p6bool(nqp::iseq_s($.abspath, nqp::unbox_s(other.path.abspath)));
    }

    multi method ACCEPTS(IO::Path:D: Mu \that) {
        nqp::p6bool(nqp::iseq_s($.abspath,nqp::unbox_s(IO::Path.new(|that).abspath)));
    }

    submethod BUILD(Str() :$!path!, :$!SPEC!, Str() :$!CWD!) { }

    method new-from-absolute-path($path, :$SPEC = $*SPEC, :$CWD = $*CWD) {
        method !fap() {
            $!is-absolute = True;
            $!abspath := $path;
            self;
        }

        self.bless(:$path, :$SPEC, :$CWD)!fap;
    }

    multi method new(IO::Path: $path, :$SPEC = $*SPEC, :$CWD = $*CWD) {
        self.bless(:$path, :$SPEC, :$CWD);
    }
    multi method new(IO::Path:
      :$basename,
      :$directory!,
      :$volume = '',
      :$SPEC   = $*SPEC,
      :$CWD    = $*CWD,
    ) {
        DEPRECATED(':dirname', |<2014.10 2015.10>, :what<IO::Path.new with :directory>);
        self.bless(
          :path($SPEC.join($volume,$directory,$basename)), :$SPEC, :$CWD);
    }
    multi method new(IO::Path:
      :$basename!,
      :$dirname = '.',
      :$volume  = '',
      :$SPEC    = $*SPEC,
      :$CWD     = $*CWD,
    ) {
        self.bless(:path($SPEC.join($volume,$dirname,$basename)),:$SPEC,:$CWD);
    }

    method abspath() {
        $!abspath //= substr($!path,0,1) eq '-'
          ?? ''
          !! $!SPEC.rel2abs($!path,$!CWD);
    }
    method is-absolute() {
        $!is-absolute //= $!SPEC.is-absolute($!path);
    }
    method is-relative() {
        !( $!is-absolute //= $!SPEC.is-absolute($!path) );
    }

    method parts                  {
        %!parts ||= $!SPEC.split($!path);
    }
    method volume(IO::Path:D:)   { %.parts<volume>   }
    method dirname(IO::Path:D:)  { %.parts<dirname>  }
    method basename(IO::Path:D:) { %.parts<basename> }
    method extension(IO::Path:D:) { MAKE-EXT(self.basename) }

    # core can't do 'basename handles <Numeric Bridge Int>'
    method Numeric(IO::Path:D:) { self.basename.Numeric }
    method Bridge (IO::Path:D:) { self.basename.Bridge  }
    method Int    (IO::Path:D:) { self.basename.Int     }

    multi method Str (IO::Path:D:) { $!path }
    multi method gist(IO::Path:D:) {
        qq|"$.abspath".IO|;
    }
    multi method perl(IO::Path:D:) {
        ($.is-absolute
          ?? "q|$.abspath|.IO(:SPEC({$!SPEC.^name}))"
          !! "q|$.path|.IO(:SPEC({$!SPEC.^name}),:CWD<$!CWD>)"
        ).subst(:global, '\\', '\\\\');
    }

    method succ(IO::Path:D:) {
        self.bless(
          :path($!SPEC.join($.volume,$.dirname,$.basename.succ)),
          :$!SPEC,
          :$!CWD,
        );
    }
    method pred(IO::Path:D:) {
        self.bless(
          :path($!SPEC.join($.volume,$.dirname,$.basename.pred)),
          :$!SPEC,
          :$!CWD,
        );
    }

    method IO(IO::Path:D: |c) { self }

    method open(IO::Path:D: |c) {
        my $handle = IO::Handle.new(:path(self));
        $handle // $handle.throw;
        $handle.open(|c);
    }

    method pipe(IO::Path:D: |c) {
        my $handle = IO::Handle.new(:path(self));
        $handle // $handle.throw;
        $handle.pipe(|c);
    }

#?if moar
    method watch(IO::Path:D:) {
        IO::Notification.watch_path($.abspath);
    }
#?endif

    proto method absolute(|) { * }
    multi method absolute (IO::Path:D:) { $.abspath }
    multi method absolute (IO::Path:D: $CWD) {
        self.is-absolute
          ?? $.abspath
          !! $!SPEC.rel2abs($!path, $CWD);
    }

    method relative (IO::Path:D: $CWD = $*CWD) {
        $!SPEC.abs2rel($.abspath, $CWD);
    }

    method cleanup (IO::Path:D:) {
        self.bless(:path($!SPEC.canonpath($!path)), :$!SPEC, :$!CWD);
    }
    method resolve (IO::Path:D:) {
        # NYI: requires readlink()
        X::NYI.new(feature=>'IO::Path.resolve').fail;
    }

    method parent(IO::Path:D:) {    # XXX needs work
        my $curdir := $!SPEC.curdir;
        my $updir  := $!SPEC.updir;

        if self.is-absolute {
            return self.bless(
              :path($!SPEC.join($.volume, $.dirname, '')),
              :$!SPEC,
              :$!CWD,
            );
        }
        elsif $.dirname eq $curdir and $.basename eq $curdir {
            return self.bless(
              :path($!SPEC.join($.volume,$curdir,$updir)),
              :$!SPEC,
              :$!CWD,
            );
        }
        elsif $.dirname eq $curdir && $.basename eq $updir
           or !grep({$_ ne $updir}, $!SPEC.splitdir($.dirname)) {
            return self.bless(    # If all updirs, then add one more
              :path($!SPEC.join($.volume,$!SPEC.catdir($.dirname,$updir),$.basename)),
              :$!SPEC,
              :$!CWD,
            );
        }
        else {
            return self.bless(
              :path($!SPEC.join($.volume, $.dirname, '')),
              :$!SPEC,
              :$!CWD,
            );
        }
    }

    method child (IO::Path:D: $child) {
        self.bless(:path($!SPEC.catfile($!path,$child)), :$!SPEC, :$!CWD);
    }

    proto method chdir(|) { * }
    multi method chdir(IO::Path:U: $path, :$test = 'r') {
        $*CWD.chdir($path,:$test);
    }
    multi method chdir(IO::Path:D: Str() $path is copy, :$test = 'r') {
        if !$!SPEC.is-absolute($path) {
            my ($volume,$dirs) = $!SPEC.splitpath(self, :nofile);
            my @dirs = $!SPEC.splitdir($dirs);
            @dirs.shift; # the first is always empty for absolute dirs
            for $!SPEC.splitdir($path) -> $dir {
                if $dir eq '..' {
                    @dirs.pop if @dirs;
                }
                elsif $dir ne '.' {
                    @dirs.push: $dir;
                }
            }
            @dirs.push('') if !@dirs;  # need at least the rootdir
            $path = join($!SPEC.dir-sep, $volume, @dirs);
        }
        my $dir = IO::Path.new-from-absolute-path($path,:$!SPEC,:CWD(self));

        # basic sanity
        unless $dir.d {
            fail X::IO::Chdir.new(
              :$path,
              :os-error( $dir.e
                ?? "is not a directory"
                !! "does not exist"),
            );
        }

        if $test eq 'r' {
            return $dir if $dir.r;
        }
        elsif $test eq 'r w' {
            return $dir if $dir.r and $dir.w;
        }
        elsif $test eq 'r w x' {
            return $dir if $dir.r and $dir.w and $dir.x;
        }

        fail X::IO::Chdir.new(
          :$path,
          :os-error("did not pass 'd $test' test"),
        );
    }

    proto method rename(|) { * }
    multi method rename(IO::Path:D: IO::Path:D $to, :$createonly) {
        if $createonly and $to.e {
            fail X::IO::Rename.new(
              :from($.abspath),
              :$to,
              :os-error(':createonly specified and destination exists'),
            );
        }
        nqp::rename($.abspath, nqp::unbox_s($to.abspath));
        CATCH { default {
            fail X::IO::Rename.new(
              :from($!abspath), :$to($to.abspath), :os-error(.Str) );
        } }
        True;
    }
    multi method rename(IO::Path:D: $to, :$CWD = $*CWD, |c) {
        self.rename($to.IO(:$!SPEC,:$CWD),|c);
    }

    proto method copy(|) { * }
    multi method copy(IO::Path:D: IO::Path:D $to, :$createonly) {
        if $createonly and $to.e {
            fail X::IO::Copy.new(
              :from($.abspath),
              :$to,
              :os-error(':createonly specified and destination exists'),
            );
        }
        nqp::copy($.abspath, nqp::unbox_s($to.abspath));
        CATCH { default {
            fail X::IO::Copy.new(
              :from($!abspath), :$to, :os-error(.Str) );
        } }
        True;
    }
    multi method copy(IO::Path:D: $to, :$CWD  = $*CWD, |c) {
        self.copy($to.IO(:$!SPEC,:$CWD),|c);
    }

    method chmod(IO::Path:D: Int(Any) $mode) {
        nqp::chmod($.abspath, nqp::unbox_i($mode));
        CATCH { default {
            fail X::IO::Chmod.new(
              :path($!abspath), :$mode, :os-error(.Str) );
        } }
        True;
    }
    method unlink(IO::Path:D:) {
        nqp::unlink($.abspath);
        CATCH { default {
            fail X::IO::Unlink.new( :path($!abspath), os-error => .Str );
        } }
        True;
    }

    method symlink(IO::Path:D: $name is copy, :$CWD  = $*CWD) {
        $name = $name.IO(:$!SPEC,:$CWD).path;
        nqp::symlink(nqp::unbox_s($name), $.abspath);
        CATCH { default {
            fail X::IO::Symlink.new(:target($!abspath), :$name, os-error => .Str);
        } }
        True;
    }

    method link(IO::Path:D: $name is copy, :$CWD  = $*CWD) {
        $name = $name.IO(:$!SPEC,:$CWD).path;
        nqp::link(nqp::unbox_s($name), $.abspath);
        CATCH { default {
            fail X::IO::Link.new(:target($!abspath), :$name, os-error => .Str);
        } }
        True;
    }

    method mkdir(IO::Path:D: $mode = 0o777) {
        nqp::mkdir($.abspath, $mode);
        CATCH { default {
            fail X::IO::Mkdir.new(:path($!abspath), :$mode, os-error => .Str);
        } }
        True;
    }

    method rmdir(IO::Path:D:) {
        nqp::rmdir($.abspath);
        CATCH { default {
            fail X::IO::Rmdir.new(:path($!abspath), os-error => .Str);
        } }
        True;
    }

    method contents(IO::Path:D: |c) {
        DEPRECATED('dir', |<2014.10 2015.10>);
        self.dir(|c);
    }

    method dir(IO::Path:D:
        Mu :$test = $*SPEC.curupdir,
        :$absolute,
        :$Str,
        :$CWD = $*CWD,
    ) {

        CATCH { default {
            fail X::IO::Dir.new(
              :path(nqp::box_s($.abspath,Str)), :os-error(.Str) );
        } }
        my $cwd_chars = $CWD.chars;

#?if moar
        my str $cwd = nqp::cwd();
        nqp::chdir(nqp::unbox_s($.abspath));
#?endif
        my $abspath-sep := $.abspath eq $!SPEC.dir-sep
          ?? $!SPEC.dir-sep
          !! $.abspath ~ $!SPEC.dir-sep;

#?if parrot
        my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s($.abspath));
        my int $elems = nqp::elems($RSA);
        gather
            loop (my int $i = 0; $i < $elems; $i = $i + 1) {
                my Str $elem = nqp::p6box_s(pir::trans_encoding__Ssi(
                  nqp::atpos_s($RSA, $i),
                  pir::find_encoding__Is('utf8')));
#?endif
#?if !parrot
        my Mu $dirh := nqp::opendir(nqp::unbox_s($.abspath));
        gather {
#?endif
#?if jvm
            for <. ..> -> $elem {
                if $test.ACCEPTS($elem) {
                    $Str
                      ?? $absolute
                        ?? take $abspath-sep ~ $elem
                        !! take substr($abspath-sep ~ $elem,$cwd_chars + 1)
                      !! $absolute
                        ?? take IO::Path.new-from-absolute-path($abspath-sep ~ $elem,:$!SPEC,:$CWD)
                        !! take substr($abspath-sep ~ $elem,$cwd_chars + 1).IO(:$!SPEC,:$CWD);
                }
            }
#?endif
#?if !parrot
            loop {
                my str $str_elem = nqp::nextfiledir($dirh);
                if nqp::isnull_s($str_elem) || nqp::chars($str_elem) == 0 {
                    nqp::closedir($dirh);
                    last;
                }
                my Str $elem = nqp::box_s($str_elem,Str);
#?endif
#?if jvm
                if $test.ACCEPTS($!SPEC.basename($elem)) {
#?endif
#?if !jvm
                if $test.ACCEPTS($elem) {
                    $elem = $abspath-sep ~ $elem; # make absolute
#?endif
                    $Str
                      ?? !$absolute && !$.is-absolute
                        ?? take substr($elem,$cwd_chars + 1)
                        !! take $elem
                      !! !$absolute && !$.is-absolute
                        ?? take substr($elem,$cwd_chars + 1).IO(:$!SPEC,:$CWD)
                        !! take IO::Path.new-from-absolute-path($elem,:$!SPEC,:$CWD);
                }
#?if moar
                nqp::chdir($cwd);
#?endif
            }
#?if !parrot
        }
#?endif
    }

    proto method slurp() { * }
    multi method slurp(IO::Path:D: :$bin, :$enc) {
        my $handle = self.open;
        $handle // $handle.throw;

        my Mu $PIO := nqp::getattr(nqp::decont($handle),IO::Handle,'$!PIO');
        if $bin {
            my $Buf := buf8.new();
            loop {
                my $buf := buf8.new();
                nqp::readfh($PIO,$buf,65536);
                last if $buf.bytes == 0;
                $Buf := $Buf ~ $buf;
            }
            $handle.close;
            $Buf;
        }
        else {
            $handle.encoding($enc) if $enc.defined;
            my $slurped := nqp::p6box_s(nqp::readallfh($PIO));
            $handle.close;
            $slurped;
        }
    }

    method !spurt($what, :$enc, :$append, :$createonly, :$bin, |c) {
        if $createonly and $.e {
            fail("File '$!path' already exists, and :createonly was specified");
        }
        my $mode = $append ?? :a !! :w;
        my $handle = self.open(:enc($enc // 'utf8'), :$bin, |$mode, |c);
        $handle // $handle.throw;

        my $spurt := $bin
          ?? $handle.write($what)
          !! $handle.print($what);
        $handle.close;  # can't use LEAVE in settings :-(
        $spurt;
    }

    proto method spurt(|) { * }
    multi method spurt(IO::Path:D: Blob $what, :$bin, |c) {
        self!spurt($what, :bin, |c );
    }
    multi method spurt(IO::Path:D: Cool $what, :$bin, |c) {
        self!spurt($what, :!bin, |c );
    }

    proto method lines() { * }
    multi method lines(IO::Path:D: |c) {
        my $handle = self.open(|c);
        $handle && $handle.lines(:close, |c);
    }

    proto method words() { * }
    multi method words(IO::Path:D: |c) {
        my $handle = self.open(|c);
        $handle && $handle.words(:close, |c);
    }

    my %t =
      e => -> $p { True }, # if we get here, it exists
      d => -> $p { nqp::p6bool(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_ISDIR)) },
      f => -> $p { nqp::p6bool(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_ISREG)) },
      s => -> $p { %t.at_key("f")($p) && nqp::box_i(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_FILESIZE),Int) },
      l => -> $p { nqp::p6bool(nqp::fileislink(nqp::unbox_s($p))) },
      r => -> $p { nqp::p6bool(nqp::filereadable(nqp::unbox_s($p))) },
      w => -> $p { nqp::p6bool(nqp::filewritable(nqp::unbox_s($p))) },
      x => -> $p { nqp::p6bool(nqp::fileexecutable(nqp::unbox_s($p))) },
      z => -> $p { %t.at_key("f")($p) && nqp::p6bool(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_FILESIZE) == 0) },

      "!e" => -> $p { False }, # if we get here, it exists
      "!d" => -> $p { !%t.at_key("d")($p) },
      "!f" => -> $p { !%t.at_key("f")($p) },
      "!l" => -> $p { !%t.at_key("l")($p) },
      "!r" => -> $p { !%t.at_key("r")($p) },
      "!w" => -> $p { !%t.at_key("w")($p) },
      "!x" => -> $p { !%t.at_key("x")($p) },
      "!z" => -> $p { !%t.at_key("z")($p) },
    ;

    method all(*@tests) {
        return False if !@tests or !$.e;

        my $result = True;
        for @tests -> $t {
            die "Unknown test $t" unless %t.exists_key($t);
            last unless $result = $result && %t.at_key($t)($!abspath);
        }

        $result;
    }

    method e() { $!e //= FILETEST-E($.abspath) }

    method d() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<d>) if !$.e;
        FILETEST-D($!abspath);
    }

    method f() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<f>) if !$.e;
        FILETEST-F($!abspath);
    }

    method s() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<s>) if !$.e;
        fail X::IO::NotAFile.new(:path(self.Str),:trying<s>)     if !$.f;
        FILETEST-S($!abspath);
    }

    method l() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<l>) if !$.e;
        FILETEST-L($!abspath);
    }

    method r() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<r>) if !$.e;
        FILETEST-R($!abspath);
    }

    method w() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<w>) if !$.e;
        FILETEST-W($!abspath);
    }

    method rw() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<w>) if !$.e;
        FILETEST-RW($!abspath);
    }

    method x() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<x>) if !$.e;
        FILETEST-X($!abspath);
    }

    method rwx() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<w>) if !$.e;
        FILETEST-RWX($!abspath);
    }

    method z() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<z>) if !$.e;
        fail X::IO::NotAFile.new(:path(self.Str),:trying<z>)     if !$.f;
        FILETEST-Z($!abspath);
    }

    method modified() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<modified>) if !$.e;
        FILETEST-MODIFIED($!abspath);
    }

    method accessed() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<accessed>) if !$.e;
        FILETEST-ACCESSED($!abspath);
    }

    method changed() {
        fail X::IO::DoesNotExist.new(:path(self.Str),:trying<changed>) if !$.e;
        FILETEST-CHANGED($!abspath);
    }

    method directory() {
        DEPRECATED("dirname", |<2014.10 2015.10>);
        self.dirname;
    }
}

my class IO::Path::Cygwin is IO::Path {
    method new(|c) { IO::Path.new(|c, :SPEC(IO::Spec::Cygwin) ) }
}
my class IO::Path::QNX is IO::Path {
    method new(|c) { IO::Path.new(|c, :SPEC(IO::Spec::QNX) ) }
}
my class IO::Path::Unix is IO::Path {
    method new(|c) { IO::Path.new(|c, :SPEC(IO::Spec::Unix) ) }
}
my class IO::Path::Win32 is IO::Path {
    method new(|c) { IO::Path.new(|c, :SPEC(IO::Spec::Win32) ) }
}

# vim: ft=perl6 expandtab sw=4
