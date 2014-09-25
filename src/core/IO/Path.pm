my class IO::Path is Cool does IO::FileTestable {
    method SPEC { IO::Spec.MODULE };
    has Str $.path;

    method dir() {
        die "IO::Path.dir is deprecated in favor of .directory";
    }

    multi method ACCEPTS(IO::Path:D: IO::Path:D \other) {
        self.cleanup.parts eqv other.cleanup.parts
    }

    multi method ACCEPTS(IO::Path:D: Mu \other) {
        self.cleanup.parts eqv IO::Path.new(|other).cleanup.parts
    }

    submethod BUILD(:$!path!, :$dir) {
        die "Named paramter :dir in IO::Path.new deprecated in favor of :directory"
            if defined $dir;
    }

    multi method new(IO::Path: :$basename!, :$directory = '.', :$volume = '') {
        self.bless: path=>$.SPEC.join($volume, $directory, $basename);
    }

    multi method new(IO::Path: $path as Str) {
        self.bless(:$path)
    }

    method path(IO::Path:D:) {
        self;
    }

    method parts {
        $.SPEC.split($!path).hash
    }
    method basename(IO::Path:D:) {
        self.parts<basename>
    }
    method directory(IO::Path:D:) {
        self.parts<directory>
    }
    method volume(IO::Path:D:) {
        self.parts<volume>
    }

    multi method Str(IO::Path:D:) {
         $!path;
    }
    multi method gist(IO::Path:D:) {
        "{self.^name}<{ $!path }>";
    }
    multi method perl(IO::Path:D:) {
         "IO::Path.new(path => " ~ $.Str.perl ~ ")";
    }
    multi method Numeric(IO::Path:D:) {
        self.basename.Numeric;
    }
    method Bridge(IO::Path:D:) {
        self.basename.Bridge;
    }
    method Int(IO::Path:D:) {
        self.basename.Int;
    }

    method succ(IO::Path:D:) {
        self.new(:$.volume, :$.directory, basename=> $.basename.succ)
    }
    method pred(IO::Path:D:) {
        self.new(:$.volume, :$.directory, basename=> $.basename.pred)
    }

    method IO(IO::Path:D: |c) {
        IO::Path.new($!path, |c);
    }
    method open(IO::Path:D: |c) {
        my $handle = IO::Handle.new(:path($!path));
        $handle && $handle.open(|c);
    }

#?if moar
    method watch(IO::Path:D:) {
        IO::Notification.watch_path($!path);
    }
#?endif

    method is-absolute(IO::Path:D:) {
        $.SPEC.is-absolute($!path);
    }
    method is-relative(IO::Path:D:) {
        ! $.SPEC.is-absolute($!path);
    }
    method absolute (IO::Path:D: $base = ~$*CWD) {
        return self.new($.SPEC.rel2abs($!path, $base));
    }
    method relative (IO::Path:D: $relative_to_directory = ~$*CWD) {
        return self.new($.SPEC.abs2rel($!path, $relative_to_directory));
    }

    method cleanup (IO::Path:D: :$parent) {
        return self.new($.SPEC.canonpath($!path, :$parent));
    }
    method resolve (IO::Path:D:) {
        # NYI: requires readlink()
        X::NYI.new(feature=>'IO::Path.resolve').fail;
    }

    method parent(IO::Path:D:) {
        if self.is-absolute {
            return self.new($.SPEC.join($.volume, $.directory, ''));
        }
        elsif all($.basename, $.directory) eq $.SPEC.curdir {
            return self.new(:$.volume, directory=>$.SPEC.curdir,
                             basename=>$.SPEC.updir);
        }
        elsif $.basename eq $.SPEC.updir && $.directory eq $.SPEC.curdir 
           or !grep({$_ ne $.SPEC.updir}, $.SPEC.splitdir($.directory)) {  
            return self.new(    # If all updirs, then add one more
                :$.volume,
                directory => $.SPEC.catdir($.directory, $.SPEC.updir),
                :$.basename );
        }
        else {
            return self.new( $.SPEC.join($.volume, $.directory, '') );
        }
    }

    method child (IO::Path:D: $childname) {
        self.new: path => $.SPEC.catfile($!path, $childname);
    }

    method copy(IO::Path:D: $dest, :$createonly = False) {
        my $absdest = IO::Spec.rel2abs($dest);
        if $createonly and $absdest.e {
            fail(X::IO::Copy.new(from => $!path, to => $dest,
                    os-error => "Destination file $dest exists and :createonly passed to copy."));
        }
        try {
            nqp::copy(nqp::unbox_s(IO::Spec.rel2abs($!path)), nqp::unbox_s(~$absdest));
        }
        $! ?? fail(X::IO::Copy.new(from => $!path, to => $dest, os-error => ~$!)) !! True
    }

    method chmod(IO::Path:D: Int $mode) {
        nqp::chmod(nqp::unbox_s(IO::Spec.rel2abs($!path)), nqp::unbox_i($mode.Int));
        return True;
        CATCH {
            default {
                X::IO::Chmod.new(
                    :$!path,
                    :$mode,
                    os-error => .Str,
                ).throw;
            }
        }
    }

    method contents(IO::Path:D: Mu :$test = { $_ ne '.' && $_ ne '..' }) {

        CATCH {
            default {
                X::IO::Dir.new(
                    :$!path,
                    os-error => .Str,
                ).throw;
            }
        }
#?if parrot
        my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s(self.absolute.Str));
        my int $elems = nqp::elems($RSA);
        gather loop (my int $i = 0; $i < $elems; $i = $i + 1) {
            my Str $file := nqp::p6box_s(pir::trans_encoding__Ssi(
              nqp::atpos_s($RSA, $i),
              pir::find_encoding__Is('utf8')));
            if $file ~~ $test {
                take self.child($file);
            }
        }
#?endif
#?if jvm
        my $cwd_chars = $*CWD.chars;
#?endif
#?if !parrot
        my Mu $dirh := nqp::opendir(self.absolute.Str);
        my $next = 1;
        gather {
            take $_.path if $_ ~~ $test for ".", "..";
            my $SPEC = $.SPEC;
            loop {
                my str $elem = nqp::nextfiledir($dirh);
                if nqp::isnull_s($elem) || nqp::chars($elem) == 0 {
                    nqp::closedir($dirh);
                    last;
                }
                elsif $elem ne '.' | '..' {
#?endif
#?if jvm
                    # jvm's nextfiledir gives us absolute paths back, moar does not.
                    $elem = nqp::substr($elem, $cwd_chars + 1) if self.is-relative;
#?endif
#?if moar
                    $elem = $SPEC.catfile($!path, $elem) if $!path ne '.';
#?endif
#?if !parrot
                    if nqp::substr($elem, 0, 2) eq "./" | ".\\" {
                        $elem = nqp::substr($elem, 2);
                    }
                    take IO::Path.new($elem) if $test.ACCEPTS($elem);
                }
            }
        }
#?endif
    }

    proto method slurp() { * }
    multi method slurp(IO::Path:D: |c) {
        my $handle = self.open(|c);
        $handle && do {
            my $slurp := $handle.slurp(|c);
            $handle.close;  # can't use LEAVE in settings :-(
            $slurp;
        }
    }

    proto method spurt(|) { * }
    multi method spurt(IO::Path:D: $what, :$enc = 'utf8', :$append, :$createonly, |c) {
        if $createonly and self.e {
            fail("File '$!path' already exists, and :createonly was specified");
        }
        my $mode = $append ?? :a !! :w;
        my $handle = self.open(:$enc, |$mode, |c);
        $handle && do {
            my $spurt := $handle.spurt($what, :$enc, |c);
            $handle.close;  # can't use LEAVE in settings :-(
            $spurt;
        }
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
}

my class IO::Path::Unix   is IO::Path { method SPEC { IO::Spec::Unix   };  }
my class IO::Path::Win32  is IO::Path { method SPEC { IO::Spec::Win32  };  }
my class IO::Path::Cygwin is IO::Path { method SPEC { IO::Spec::Cygwin };  }
my class IO::Path::QNX    is IO::Path { method SPEC { IO::Spec::QNX    };  }

# vim: ft=perl6 expandtab sw=4
