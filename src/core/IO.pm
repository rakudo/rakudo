class IO is Cool {
    has $!PIO;
    has $!ins;
    has $.autoflush is rw;

    has $.path;
    has $.stat = ::IO::Stat.new(path => $.path);

    multi method close() is export {
        try {
            ?$!PIO.close()
        }
        $! ?? fail($!) !! Bool::True
    }

    multi method eof() is export {
        ?$!PIO.eof();
    }

    multi method get() {
        my $x = $!PIO.readline;
        fail if $.eof && $x eq '';
        $!ins++;
        $x.chomp;
    }

    multi method ins() {
        $!ins;
    }

    multi method lines($limit = *) {
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        gather while !$.eof && $l-- > 0 {
           my $line = $.get;
           if $line.defined {
               take $line;
           }
        }
    }

    multi method open($filename, :$r, :$w, :$a, :$bin) {
        if $!PIO { $!PIO.close; $!PIO = Nil; }
        my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r');
        $!PIO = $filename eq '-'
                ?? pir::getstdin__P()
                !! pir::open__PSS($filename, $mode);
        unless pir::istrue__IP($!PIO) {
            fail("Unable to open file '$filename'");
        }
        $!PIO.encoding($bin ?? 'binary' !! 'utf8');
        self;
    }

    multi method print(*@items) {
        try {
            for @items -> $item {
                (pir::descalarref__PP($!PIO)).print($item);
            }
            pir::descalarref__PP($!PIO).flush() if $.autoflush;
        }
        $! ?? fail($!) !! Bool::True;
    }

    multi method printf($format, *@args) {
        self.print(sprintf($format, |@args));
    }

    multi method say(*@items) {
        self.print(@items, "\n");
    }

    multi method read(Int $bytes) {
        my $pio = $!PIO;
        my @bytes = Q:PIR {
            .local int nbytes, byte
            .local pmc bytebuffer, it, result
            .local pmc pio
            pio = find_lex '$pio'
            pio = deref_unless_object pio
            $P0 = find_lex '$bytes'
            nbytes = $P0
            $S0 = pio.'read'(nbytes)
            bytebuffer = new ['ByteBuffer']
            bytebuffer = $S0

            result = new ['Parcel']
            it = iter bytebuffer
          bytes_loop:
            unless it goto done
            byte = shift it
            push result, byte
            goto bytes_loop
          done:
            %r = result
        };
        return Buf.new(@bytes);
    }

    multi method write(Buf $buf) {
        my @contents = $buf.contents;
        my $pio = $!PIO;
        Q:PIR {
            $P0 = find_lex '@contents'

            .local pmc bb
            .local string s
            bb = new ['ByteBuffer']
            .local pmc it
            .local int i
            it = iter $P0
            i = 0
          loop:
            unless it goto done
            $P1 = shift it
            $I1 = $P1
            bb[i] = $I1
            inc i
            goto loop
          done:
            s = bb.'get_string_as'(binary:"")
            .local pmc pio
            pio = find_lex '$pio'
            pio = deref_unless_object pio
            pio.'print'(s)
        };
    }

    multi method getc() {
        my $c = $!PIO.read(1);
        fail if $c eq '';
        $c;
    }

    multi method slurp() {
        $!PIO.readall();
    }

    multi method t() {
        $!PIO.isatty;
    }

    # file test operations
    multi method d() {
        self.e ?? $.stat.isdir !! Bool;
    }
    multi method e() {
        $.stat.exists;
    }
    multi method f() {
        self.e ?? (!$.stat.isdir and !$.stat.isdev) !! Bool;
    }

    multi method s() {
        self.e ?? $.stat.size !! Any;
    }

    multi method R() {
        ?pir::new__PS('OS').can_read($.path);
    }

    multi method W() {
        ?pir::new__PS('OS').can_write($.path);
    }

    multi method X() {
        ?pir::new__PS('OS').can_execute($.path);
    }

    multi method r() {
        return self.R if $*OSNAME -eq "MSWin32";
        my $path = $.path;
        return ?Q:PIR {
            .local pmc uid,euid,suid,undef,os

            uid = get_hll_global '$UID'
            euid = get_hll_global '$EUID'
            undef = new ['Undef']
            os = new ['OS']

            .local pmc setresuid,res,path
            path = find_lex '$path'
            setresuid = dlfunc undef, 'setresuid', 'iiii'

            setresuid(euid, uid, uid)
            res = os.'can_read'(path)
            setresuid(uid, euid, uid)

            %r = res
        };
    }

    multi method w() {
        return self.W if $*OSNAME -eq "MSWin32";
        my $path = $.path;
        return ?Q:PIR {
            .local pmc uid,euid,suid,undef,os

            uid = get_hll_global '$UID'
            euid = get_hll_global '$EUID'
            undef = new ['Undef']
            os = new ['OS']

            .local pmc setresuid,res,path
            path = find_lex '$path'
            setresuid = dlfunc undef, 'setresuid', 'iiii'

            setresuid(euid, uid, uid)
            res = os.'can_write'(path)
            setresuid(uid, euid, uid)

            %r = res
        };
    }

    multi method x() {
        return self.X if $*OSNAME -eq "MSWin32";
        my $path = $.path;
        return ?Q:PIR {
            .local pmc uid,euid,suid,undef,os

            uid = get_hll_global '$UID'
            euid = get_hll_global '$EUID'
            undef = new ['Undef']
            os = new ['OS']

            .local pmc setresuid,res,path
            path = find_lex '$path'
            setresuid = dlfunc undef, 'setresuid', 'iiii'

            setresuid(euid, uid, uid)
            res = os.'can_execute'(path)
            setresuid(uid, euid, uid)

            %r = res
        };
    }

    multi method l() {
        $.stat.islnk;
    }

    multi method O() {
        pir::new__PS('OS').get_user_id() ~~ $.stat.uid;
    }

    # Can't get effective uid in parrot
    multi method o() {
        pir::new__PS('OS').get_user_id() ~~ $.stat.uid;
    }

	multi method u() {
		?($.stat.permissions +& 0o4000);
	}

	multi method g() {
		?($.stat.permissions +& 0o2000);
	}

	multi method k() {
		?($.stat.permissions +& 0o1000);
	}

    multi method z() {
        $.e && $.s == 0;
    }

    multi method created() { ::Instant.from-posix($.stat.createtime) }
    multi method modified() { ::Instant.from-posix($.stat.modifytime) }
    multi method accessed() { ::Instant.from-posix($.stat.accesstime) }
    multi method changed() { ::Instant.from-posix($.stat.changetime) }

    multi method move($dest as Str) {
        try {
            pir::new__PS('OS').rename($.path, $dest);
        }
        $! ?? fail($!) !! True
    }

    multi method chmod($mode as Int) {
        try {
            pir::new__PS('OS').chmod($.path, $mode);
        }
        $! ?? fail($!) !! True
    }

    multi method copy($dest as Str) {
        try {
            pir::new__PS('File').copy($.path, $dest);
        }
        $! ?? fail($!) !! True
    }

    multi method link($dest as Str, Bool :$hard = False) {
        try {
            if $hard {
                pir::new__PS('OS').link($.path, $dest);
            }
            else {
                pir::new__PS('OS').symlink($.path, $dest);
            }
        }
        $! ?? fail($!) !! True
    }
}

multi sub get(IO $filehandle = $*ARGFILES) { $filehandle.get };

multi sub lines(IO $filehandle = $*ARGFILES,
                :$bin = False,
                :$enc = 'Unicode',
                :$nl = "\n",
                :$chomp = True) {

    fail 'Binary mode not supported yet'    if $bin;
    fail 'Encodings not supported yet'      if $enc ne 'Unicode';
    fail 'Fancy newlines not supported yet' if $nl ne "\n";
    fail 'Lack of chomp not supported yet'  if !$chomp;

    $filehandle.lines();
}

multi sub print(Mu *@items) { $*OUT.print(@items); }

multi sub prompt($msg) {
    print $msg;
    $*IN.get;
}

multi sub say(Mu *@items) { $*OUT.say(@items); }

sub getc($handle) { $handle.getc(); }

sub open($filename, :$r, :$w, :$a, :$bin) {
    my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r');
    my $PIO = pir::open__PSS($filename, $mode);
    unless pir::istrue__IP($PIO) {
        fail("Unable to open file '$filename'");
    }
    $PIO.encoding($bin ?? 'binary' !! 'utf8');
    IO.new(:$PIO)
}

sub close($handle) {
    $handle.close()
}

sub slurp($filename) {
    my $handle = open($filename, :r);
    my $contents = $handle.slurp();
    $handle.close();
    $contents
}

sub unlink($filename) {
    try {
        pir::new__PS('OS').rm($filename);
    }
    $! ?? fail($!) !! True
}

# CHEAT: This function is missing a bunch of arguments,
# and should be more robust.
multi lines (Str $filename, Any  $limit = *) {
    my $fh = open $filename or fail "Unable to open $filename";
    $fh.lines($limit);
}

multi sub printf($format, *@args) {
    $*OUT.printf($format, |@args);

}

multi sub note(*@args) {
    $*ERR.say(@args);
}

multi sub dir($path as Str = '.', Mu :$test = none('.', '..')) {
    Q:PIR {
        $P0 = find_lex '$path'
        $P1 = new ['OS']
        $P1 = $P1.'readdir'($P0)
        %r = '&infix:<,>'($P1 :flat)
    }.map({~$_}).grep($test)
}

multi sub chdir($path as Str) {
    try {
        pir::new__PS('OS').chdir($path)
    }
    $! ?? fail($!) !! True
}

multi sub mkdir($path as Str, $mode = 0o777) {
    try {
        pir::new__PS('OS').mkdir($path, $mode)
    }
    $! ?? fail($!) !! True
}

multi sub cwd() {
    my $pwd;
    try {
        $pwd = pir::new__Ps('OS').cwd();
    }
    $! ?? fail($!) !! $pwd;
}

multi sub move($src as Str, $dest as Str) {
    try {
        pir::new__PS('OS').rename($src, $dest);
    }
    $! ?? fail($!) !! True
}

multi sub chmod($path as Str, $mode as Int) {
    try {
        pir::new__PS('OS').chmod($path, $mode);
    }
    $! ?? fail($!) !! True
}

multi sub copy($src as Str, $dest as Str) {
    try {
        pir::new__PS('File').copy($src, $dest);
    }
    $! ?? fail($!) !! True
}

multi sub rm($path as Str) {
    try { 
        pir::new__PS('OS').rm($path);
    }
    $! ?? fail($!) !! True
}

multi sub link($src as Str, $dest as Str, Bool :$hard = False) {
    if $hard {
        try {
            pir::new__PS('OS').link($src, $dest);
        }
        $! ?? fail($!) !! return True;
    }

    try {
        pir::new__PS('OS').symlink($src, $dest);
    }
    $! ?? fail($!) !! True
}

# vim: ft=perl6
