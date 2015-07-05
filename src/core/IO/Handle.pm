<<<<<<< HEAD
class IO::Handle does IO does PIO does IO::Pathy {

    submethod BUILD(:$abspath,:$path,|c) {
        $!abspath = $abspath // $path.Str;
        self!set-PIO-attributes(|c);
=======
my class IO::Path { ... }
my class IO::Special { ... }
my class Proc { ... }

# Will be removed together with pipe() and open(:p).
my class Proc::Status {
    has $.exitcode = -1;  # distinguish uninitialized from 0 status
    has $.pid;
    has $.signal;

    method exit {
        DEPRECATED('Proc::Status.exitcode', |<2015.03 2015.09>);
        $!exitcode;
    }

    proto method status(|) { * }
    multi method status($new_status) {
        $!exitcode = $new_status +> 8;
        $!signal   = $new_status +& 0xFF;
    }
    multi method status(Proc::Status:D:)  { ($!exitcode +< 8) +| $!signal }
    multi method Numeric(Proc::Status:D:) { $!exitcode }
    multi method Bool(Proc::Status:D:)    { $!exitcode == 0 }
}

my class IO::Handle does IO {
    has $.path;
    has $!PIO;
    has int $.ins;
    has $.chomp is rw = Bool::True;
    has $.nl    = "\n";
    has int $!pipe;

    method pipe(IO::Handle:D: |c) {
        DEPRECATED('shell() or run() with :in, :out or :err', |<2015.06 2015.09>);
        self.open(:p, :nodepr, |c);
    }

    method open(IO::Handle:D:
      :$p, :$r, :$w, :$x, :$a, :$update,
      :$rw, :$rx, :$ra,
      :$mode is copy,
      :$create is copy,
      :$append is copy,
      :$truncate is copy,
      :$exclusive is copy,
      :$bin,
      :$chomp = True,
      :$enc   = 'utf8',
      :$nl    = "\n",
      :$nodepr,
    ) {

        $mode //= do {
            when so $p { 'pipe' }

            when so ($r && $w) || $rw { $create              = True; 'rw' }
            when so ($r && $x) || $rx { $create = $exclusive = True; 'rw' }
            when so ($r && $a) || $ra { $create = $append    = True; 'rw' }

            when so $r { 'ro' }
            when so $w { $create = $truncate  = True; 'wo' }
            when so $x { $create = $exclusive = True; 'wo' }
            when so $a { $create = $append    = True; 'wo' }

            when so $update { 'rw' }

            default { 'ro' }
        }

        if $!path eq '-' {
            $!path = IO::Special.new:
                what => do given $mode {
                    when 'ro' { '<STDIN>'  }
                    when 'wo' { '<STDOUT>' }
                    default {
                        die "Cannot open standard stream in mode '$_'";
                    }
                }
        }

        if nqp::istype($!path, IO::Special) {
            my $what := $!path.what;
            if $what eq '<STDIN>' {
                $!PIO := nqp::getstdin();
            }
            elsif $what eq '<STDOUT>' {
                $!PIO := nqp::getstdout();
            }
            elsif $what eq '<STDERR>' {
                $!PIO := nqp::getstderr();
            }
            else {
                die "Don't know how to open '$_' especially";
            }
            $!chomp = $chomp;
            nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc)) unless $bin;
            return self;
        }

        fail (X::IO::Directory.new(:$!path, :trying<open>))
          if $!path.e && $!path.d;

        if $mode eq 'pipe' {
            DEPRECATED('shell(...)/run(...) with :in, :out or :err', |<2015.06 2015.09>, :what(':p for pipe')) unless $nodepr;
            $!pipe = 1;

            $!PIO := nqp::syncpipe();
            nqp::shell(
              nqp::unbox_s($!path.Str),
              nqp::unbox_s($*CWD.Str),
              CLONE-HASH-DECONTAINERIZED(%*ENV),
              nqp::null(), $!PIO, nqp::null(),
              nqp::const::PIPE_INHERIT_IN + nqp::const::PIPE_CAPTURE_OUT + nqp::const::PIPE_INHERIT_ERR
            );
        }
        else {
            my $llmode = do given $mode {
                when 'ro' { 'r' }
                when 'wo' { '-' }
                when 'rw' { '+' }
                default { die "Unknown mode '$_'" }
            }

            $llmode = join '', $llmode,
                $create    ?? 'c' !! '',
                $append    ?? 'a' !! '',
                $truncate  ?? 't' !! '',
                $exclusive ?? 'x' !! '';

#?if !moar
            # don't use new modes on anything but MoarVM
            # TODO: check what else can be made to work on Parrot
            #       cf io/utilities.c, Parrot_io_parse_open_flags()
            #          platform/generic/io.c, convert_flags_to_unix()
            #          platform/win32/io.c, convert_flags_to_win32 ()
            $llmode = do given $llmode {
                when 'r'   { 'r' }
                when '-ct' { 'w' }
                when '-ca' { 'wa' }
                default {
                    die "Backend { $*VM.name
                        } does not support opening files in mode '$llmode'";
                }
            }
#?endif

            # TODO: catch error, and fail()
            $!PIO := nqp::open(
              nqp::unbox_s($!path.abspath),
              nqp::unbox_s($llmode),
            );
        }

        $!chomp = $chomp;
        nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
        nqp::setencoding($!PIO, NORMALIZE_ENCODING($enc)) unless $bin;
        self;
    }

    method input-line-separator {
        DEPRECATED("nl",|<2015.03 2015.09>);
        self.nl;
    }

    method nl is rw {
        Proxy.new(
          FETCH => {
              $!nl
          },
          STORE => -> $, $nl is copy {
            nqp::setinputlinesep($!PIO, nqp::unbox_s($!nl = $nl));
          }
        );
    }

    method close(IO::Handle:D:) {
        # TODO:b catch errors
        if $!pipe {
            my $ps = Proc::Status.new;
            $ps.status( nqp::closefh_i($!PIO) ) if nqp::defined($!PIO);
            $!PIO := Mu;
            $ps;
        }
        else {
            nqp::closefh($!PIO) if nqp::defined($!PIO);
            $!PIO := Mu;
            True;
        }
    }

    method eof(IO::Handle:D:) {
        nqp::p6bool(nqp::eoffh($!PIO));
    }

    method get(IO::Handle:D:) {
        return Str if self.eof;

        my Str $x = nqp::p6box_s(nqp::readlinefh($!PIO));
        # XXX don't fail() as long as it's fatal
        # fail('end of file') if self.eof && $x eq '';
        $x.=chomp if $.chomp;
        return Str if self.eof && $x eq '';

        $!ins = $!ins + 1;
        $x;
    }

    method getc(IO::Handle:D:) {
        my $c = nqp::p6box_s(nqp::getcfh($!PIO));
        fail if $c eq '';
        $c;
    }

    proto method words (|) { * }
    # can probably go after GLR
    multi method words(IO::Handle:D: :$eager!, :$close) {
        return self.words(:$close) if !$eager;

        my str $str;
        my int $chars;
        my int $pos;
        my int $left;
        my int $nextpos;
        my Mu $rpa := nqp::list();

        until nqp::eoffh($!PIO) {

#?if moar
            $str   = $str ~ nqp::readcharsfh($!PIO, 65536); # optimize for ASCII
#?endif
#?if !moar
            my Buf $buf := Buf.new;
            nqp::readfh($!PIO, $buf, 65536);
            $str   = $str ~ nqp::unbox_s($buf.decode);
#?endif
            $chars = nqp::chars($str);
            $pos   = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

            while ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
                last unless $left = $chars - $nextpos; # broken word

                nqp::push($rpa,
                  nqp::box_s(nqp::substr($str, $pos, $nextpos - $pos), Str) );

                $pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
            }

            $str = $pos < $chars ?? nqp::substr($str,$pos) !! '';
        }
        self.close if $close;
        nqp::p6parcel($rpa, Nil);
>>>>>>> nom
    }

    method open(IO::Handle:D: :$enc,:$p,|c) {
        DEPRECATED('.IO.open or open(...)',|<2014.12 2015.12>);
        DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
          if $enc;
        DEPRECATED('pipe($path,...)',|<2014.12 2015.12>,:what(':p for pipe'))
          if $p;

        open($!abspath,:$enc,:$p,:nodepr,|c);
    }

    method pipe(IO::Handle:D: :$enc,|c) {
        DEPRECATED('.IO.pipe or pipe(...)',|<2014.12 2015.12>);
        DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
          if $enc;
        pipe($!abspath,:$enc,:nodepr,|c);
    }

    method slurp(IO::Handle:D: |c) {
        DEPRECATED('$handle.slurp-rest', |<2014.10 2015.09>);
        self.slurp-rest(|c);
    }
    method spurt(IO::Handle:D: \what,:$nodepr) {
        DEPRECATED(".IO.spurt or spurt(...)", |<2014.10 2015.09>)
          unless $nodepr;
        what ~~ Blob
          ?? self.write(what)
          !! self.print(what);
    }

    multi method Str(IO::Handle:D:)  { "open('$.relative')" }
    multi method gist(IO::Handle:D:) { "open('$.relative')" }
    multi method perl(IO::Handle:D:) {
        "open('$.relative',...)";
    }

    method d(PIO:D:) { False }
    method f(PIO:D:) { True }
}

# vim: ft=perl6 expandtab sw=4
