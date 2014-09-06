my class IO::Handle does IO::FileTestable {
    has $!PIO;
    has int $.ins;
    has $.chomp = Bool::True;
    has $.path;
    has Bool $!isDir;

    proto method open(|) { * }
    multi method open($path? is copy, :$r is copy, :$w is copy, :$rw, :$a, :$p, :$bin, :$chomp = Bool::True,
            :enc(:$encoding) = 'utf8') {
        $path //= $!path;
        my $is_std_handle = $path eq "-";
        $r = $w = True if $rw;
        my $abspath = !$is_std_handle && defined($*CWD) ?? IO::Spec.rel2abs($path) !! $path;
        $!isDir = Bool::True if !$is_std_handle &&
            nqp::p6bool(nqp::stat($abspath.Str, nqp::const::STAT_EXISTS))
            && nqp::p6bool(nqp::stat($abspath.Str, nqp::const::STAT_ISDIR));
        fail (X::IO::Directory.new(:$path, :trying<open( :w )>))
            if $w && $!isDir;
#?if parrot
        my $mode =  $p ?? ($w ||  $a ?? 'wp' !! 'rp') !!
                   ($w ?? 'w' !! ($a ?? 'wa' !! 'r' ));
        # TODO: catch error, and fail()
        nqp::bindattr(self, IO::Handle, '$!PIO',
             $is_std_handle
                ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                !! nqp::open(nqp::unbox_s($abspath.Str), nqp::unbox_s($mode))
        );
#?endif
#?if !parrot
        if $p {
            #~ my $mode =  $p ?? ($w ||  $a ?? 'wp' !! 'rp');

            my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
            my Mu $hash-without         := nqp::hash();
            my Mu $enviter := nqp::iterator($hash-with-containers);
            my $envelem;
            while $enviter {
                $envelem := nqp::shift($enviter);
                nqp::bindkey($hash-without, nqp::iterkey_s($envelem), nqp::decont(nqp::iterval($envelem)))
            }

            my $errpath = '';
            nqp::bindattr(self, IO::Handle, '$!PIO',
                 $is_std_handle
                    ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                    !! nqp::openpipe(nqp::unbox_s($abspath.Str), nqp::unbox_s($*CWD.Str), $hash-without, nqp::unbox_s($errpath))
            );
        }
        else {
            my $mode =  $w ?? 'w' !! ($a ?? 'wa' !! 'r' );
            # TODO: catch error, and fail()
            nqp::bindattr(self, IO::Handle, '$!PIO',
                 $is_std_handle
                    ?? ( $w || $a ?? nqp::getstdout() !! nqp::getstdin() )
                    !! nqp::open(nqp::unbox_s($abspath.Str), nqp::unbox_s($mode))
            );
        }
#?endif
        $!path = $path;
        $!chomp = $chomp;
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($encoding)) unless $bin;
        self;
    }

    method close() {
        # TODO:b catch errors
        nqp::closefh($!PIO) if nqp::defined($!PIO);
        $!PIO := Mu;
        Bool::True;
    }

    method eof() {
        nqp::p6bool(nqp::eoffh($!PIO));
    }

    method get() {
        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<get>)) if $!isDir;
        return Str if self.eof;
        my Str $x = nqp::p6box_s(nqp::readlinefh($!PIO));
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x.=chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins = $!ins + 1;
        $x;
    }
    
    method getc() {
        unless $!PIO {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<getc>)) if $!isDir;
        my $c = nqp::p6box_s(nqp::getcfh($!PIO));
        fail if $c eq '';
        $c;
    }

    proto method lines (|) { * }
    multi method lines() {
        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<lines>)) if $!isDir;

        if $.chomp {
            gather until nqp::eoffh($!PIO) {
                take nqp::readlinefh($!PIO).chomp;
                $!ins = $!ins + 1;
            }
        }
        else {
            gather until nqp::eoffh($!PIO) {
                take nqp::p6box_s(nqp::readlinefh($!PIO));
                $!ins = $!ins + 1;
            }
        }
    }
    multi method lines($limit) {
        unless nqp::defined($!PIO) {
            self.open($!path, :chomp($.chomp));
        }
        fail (X::IO::Directory.new(:$!path, :trying<lines>)) if $!isDir;

        my $count = $limit;
        if $.chomp {
            gather while $count-- {
                last if nqp::eoffh($!PIO);
                take nqp::readlinefh($!PIO).chomp;
                $!ins = $!ins + 1;
            }
        }
        else {
            gather while $count-- {
                last if nqp::eoffh($!PIO);
                take nqp::p6box_s(nqp::readlinefh($!PIO));
                $!ins = $!ins + 1;
            }
        }
    }

    method read(IO::Handle:D: Cool:D $bytes as Int) {
        fail (X::IO::Directory.new(:$!path, :trying<read>)) if $!isDir;
        my $buf := buf8.new();
        nqp::readfh($!PIO, $buf, nqp::unbox_i($bytes));
        $buf;
    }

    # second arguemnt should probably be an enum
    # valid values for $whence:
    #   0 -- seek from beginning of file
    #   1 -- seek relative to current position
    #   2 -- seek from the end of the file
    method seek(IO::Handle:D: Int:D $offset, Int:D $whence) {
        nqp::seekfh($!PIO, $offset, $whence);
        True;
    }

    method tell(IO::Handle:D:) returns Int {
        nqp::p6box_i(
            nqp::tellfh($!PIO)
        );
    }

    method write(IO::Handle:D: Blob:D $buf) {
        fail (X::IO::Directory.new(:$!path, :trying<write>)) if $!isDir;
        nqp::writefh($!PIO, nqp::decont($buf));
        True;
    }

    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t() {
        self.opened && nqp::p6bool($!PIO.isatty)
    }


    proto method print(|) { * }
    multi method print(IO::Handle:D: Str:D \x) {
        fail (X::IO::Directory.new(:$!path, :trying<print>)) if $!isDir;
        nqp::printfh($!PIO, nqp::unbox_s(x));
        Bool::True
    }
    multi method print(IO::Handle:D: *@list) {
        fail (X::IO::Directory.new(:$!path, :trying<print>)) if $!isDir;
        nqp::printfh($!PIO, nqp::unbox_s(@list.shift.Str)) while @list.gimme(1);
        Bool::True
    }

    multi method say(IO::Handle:D: |) {
        fail (X::IO::Directory.new(:$!path, :trying<say>)) if $!isDir;
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        self.print: nqp::shift($args).gist while $args;
        self.print: "\n";
    }
    
    method slurp(:$bin, :enc($encoding)) {
        self.open(:r, :$bin) unless self.opened;
        fail (X::IO::Directory.new(:$!path, :trying<slurp>, :use<dir>))
            if $!isDir;
        self.encoding($encoding) if $encoding.defined;

        if $bin {
            my $Buf = buf8.new();
            loop {
                my $current  = self.read(10_000);
                $Buf ~= $current;
                last if $current.bytes == 0;
            }
            self.close;
            $Buf;
        }
        else {
            my $contents = nqp::p6box_s(nqp::readallfh($!PIO));
            self.close();
            $contents
        } 
    }

    proto method spurt(|) { * }
    multi method spurt(Cool $contents,
                    :encoding(:$enc) = 'utf8',
                    :$createonly, :$append) {
        fail("File '" ~ self.path ~ "' already exists, but :createonly was give to spurt")
            if $createonly && self.e;
        fail (X::IO::Directory.new(:$!path, :trying<spurt>, :use<mkdir>))
            if self.d();
        if self.opened {
            self.encoding($enc);
        } else {
            my $mode = $append ?? :a !! :w;
            self.open(:$enc, |$mode);
        }
        self.print($contents);
        self.close;
    }
    
    multi method spurt(Blob $contents,
                    :$createonly,
                    :$append) {
        fail("File '" ~ self.path ~ "' already exists, but :createonly was give to spurt")
                if $createonly && self.e;
        fail (X::IO::Directory.new(:$!path, :trying<spurt>, :use<mkdir>))
            if self.d();
        unless self.opened {
            my $mode = $append ?? :a !! :w;
            self.open(:bin, |$mode);
        }
        self.write($contents);
        self.close;
    }

    # not spec'd
    method copy($dest) {
        warn "IO::Handle.copy is deprecated.  Please use IO::Path.copy instead.";
        try {
            nqp::copy(nqp::unbox_s(IO::Spec.rel2abs(~$!path)), 
                      nqp::unbox_s(IO::Spec.rel2abs(~$dest)));
        }
        $! ?? fail(X::IO::Copy.new(from => $!path, to => $dest, os-error => ~$!)) !! True
    }

    method chmod(Int $mode) {
        self.path.absolute.chmod($mode)
    }

    method IO { self }

    method path {  IO::Path.new($!path)  }

    multi method Str (IO::Handle:D:) {  $!path  }

    multi method gist (IO::Handle:D:) {
        self.opened
            ?? "IO::Handle<$!path>(opened, at line {$.ins} / octet {$.tell})"
            !! "IO::Handle<$!path>(closed)"
    }

    multi method perl (IO::Handle:D:) {
        "IO::Handle.new(path => {$!path.perl}, ins => {$!ins.perl}, chomp => {$!chomp.perl}, Directory => {$!isDir.Bool})"
    }


    method flush() {
        fail("File handle not open, so cannot flush")
            unless nqp::defined($!PIO);
        nqp::flushfh($!PIO);
        True;
    }

    method encoding($enc?) {
        $enc.defined
            ?? nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc))
            !! $!PIO.encoding
    }
}

# vim: ft=perl6 expandtab sw=4
