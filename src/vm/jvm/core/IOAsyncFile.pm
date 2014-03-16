# Very basic asynchronous I/O support for files. Work in progress. Things that
# would nomally return something scalar-ish produce a Promise. Things that
# would normally return a (lazy) list produce a Channel.
my class IO::Async::File {
    has $!PIO;
    has $.chomp = Bool::True;
    has $.path;
    
    proto method open(|) { * }
    multi method open($path? is copy, :$r, :$w, :$a, :$bin, :$chomp = Bool::True,
            :enc(:$encoding) = 'utf8') {
        $path //= $!path;
        my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r' );
        nqp::bindattr(self, IO::Async::File, '$!PIO',
             nqp::openasync(nqp::unbox_s($path.Str), nqp::unbox_s($mode))
        );
        $!path = $path;
        $!chomp = $chomp;
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($encoding)) unless $bin;
        self;
    }

    method close() {
        nqp::closefh($!PIO);
        Bool::True;
    }
    
    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }
    
    method slurp(IO::Async::File:D: :$bin, :enc($encoding)) {
        self.open(:r, :$bin) unless self.opened;
        self.encoding($encoding) if $encoding.defined;

        if $bin {
            die "Asynchronous binary file reading NYI"
        }
        else {
            my $p = Promise.new;
            nqp::slurpasync($!PIO, Str,
                -> $str { $p.keep($str); self.close(); },
                -> $msg { $p.break($msg); try self.close(); });
            $p
        }
    }
    
    method spurt(IO::Async::File:D: $data, :$bin, :enc($encoding)) {
        self.open(:w, :$bin) unless self.opened;
        self.encoding($encoding) if $encoding.defined;

        if $bin {
            die "Asynchronous binary file writing NYI"
        }
        else {
            my $p = Promise.new;
            nqp::spurtasync($!PIO, Str, $data,
                -> { $p.keep(1); self.close(); },
                -> $msg { $p.break($msg); try self.close(); });
            $p
        }
    }

    method lines(:enc($encoding)) {
        self.open(:r) unless self.opened;
        self.encoding($encoding) if $encoding.defined;

        my $c := Channel.new;
        nqp::linesasync($!PIO, Str, $.chomp ?? 1 !! 0,
            nqp::getattr($c, Channel, '$!queue'),
            -> { $c.close(); self.close() },
            -> $msg { $c.fail($msg); try self.close(); });
        $c
    }
}
