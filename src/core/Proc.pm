my class Proc {
    has IO::Pipe $.in;
    has IO::Pipe $.out;
    has IO::Pipe $.err;
    has $.exitcode = -1;  # distinguish uninitialized from 0 status
    has $.pid;
    has $.signal;

    has $!in_fh;
    has $!out_fh;
    has $!err_fh;
    has int $!flags;

    submethod BUILD(:$in = '-', :$out = '-', :$err = '-', :$exitcode,
                    Bool :$bin, Bool :$chomp = True, Bool :$merge,
                    Str:D :$enc = 'utf8', Str:D :$nl = "\n", :$signal) {
        if nqp::istype($in, IO::Handle) && $in.DEFINITE {
            $!in_fh := nqp::getattr(nqp::decont($in), IO::Handle, '$!PIO');
            $!flags += nqp::const::PIPE_INHERIT_IN;
        }
        elsif $in === True {
            $!in     = IO::Pipe.new(:proc(self), :path(''), :$chomp, nl-out => $nl);
            $!in_fh := nqp::syncpipe();
            $!flags += nqp::const::PIPE_CAPTURE_IN;
            Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!in_fh, $nl);
            nqp::setencoding($!in_fh,Rakudo::Internals.NORMALIZE_ENCODING($enc))
              unless $bin;
            nqp::bindattr(nqp::decont($!in), IO::Handle, '$!PIO', $!in_fh);
        }
        elsif nqp::istype($in, Str) && $in eq '-' {
            $!in_fh := nqp::null();
            $!flags += nqp::const::PIPE_INHERIT_IN;
        }
        else {
            $!in_fh := nqp::null();
            $!flags += nqp::const::PIPE_IGNORE_IN;
        }

        if $out === True || $merge {
            $!out     = IO::Pipe.new(:proc(self), :path(''), :$chomp, nl-in => $nl);
            $!out_fh := nqp::syncpipe();
            $!flags  += nqp::const::PIPE_CAPTURE_OUT;
            Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!out_fh, $nl);
            nqp::setencoding($!out_fh,Rakudo::Internals.NORMALIZE_ENCODING($enc))
              unless $bin;
            nqp::bindattr(nqp::decont($!out), IO::Handle, '$!PIO', $!out_fh);
        }
        elsif nqp::istype($out, IO::Handle) && $out.DEFINITE {
            $!out_fh := nqp::getattr(nqp::decont($out), IO::Handle, '$!PIO');
            $!flags  += nqp::const::PIPE_INHERIT_OUT;
        }
        elsif nqp::istype($out, Str) && $out eq '-' {
            $!out_fh := nqp::null();
            $!flags  += nqp::const::PIPE_INHERIT_OUT;
        }
        else {
            $!out_fh := nqp::null();
            $!flags  += nqp::const::PIPE_IGNORE_OUT;
        }

        if $merge {
            $!err    := $!out;
            $!err_fh := $!out_fh;
            $!flags  += nqp::const::PIPE_INHERIT_ERR;
        }
        elsif nqp::istype($err, IO::Handle) && $err.DEFINITE {
            $!err_fh := nqp::getattr(nqp::decont($err), IO::Handle, '$!PIO');
            $!flags  += nqp::const::PIPE_INHERIT_ERR;
        }
        elsif nqp::istype($err, Str) && $err eq '-' {
            $!err_fh := nqp::null();
            $!flags  += nqp::const::PIPE_INHERIT_ERR;
        }
        elsif $err === True {
            $!err     = IO::Pipe.new(:proc(self), :path(''), :$chomp, nl-in =>  $nl);
            $!err_fh := nqp::syncpipe();
            $!flags  += nqp::const::PIPE_CAPTURE_ERR;
            Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!err_fh, $nl);
            nqp::setencoding($!err_fh,Rakudo::Internals.NORMALIZE_ENCODING($enc))
              unless $bin;
            nqp::bindattr(nqp::decont($!err), IO::Handle, '$!PIO', $!err_fh);
        }
        else {
            $!err_fh := nqp::null();
            $!flags  += nqp::const::PIPE_IGNORE_ERR;
        }

        if nqp::istype($exitcode, Int) && $exitcode.DEFINITE {
            $!exitcode = $exitcode;
        }
        if nqp::istype($signal, Int) && $signal.DEFINITE {
            $!signal = $signal;
        }
    }

    method spawn(*@args ($, *@), :$cwd = $*CWD, :$env) {
        my %env := $env ?? $env.hash !! %*ENV;
        self.status(nqp::p6box_i(nqp::spawn(
            CLONE-LIST-DECONTAINERIZED(@args),
            nqp::unbox_s($cwd.Str),
            CLONE-HASH-DECONTAINERIZED(%env),
            $!in_fh, $!out_fh, $!err_fh,
            $!flags
        )));
        self.Bool
    }

    method shell($cmd, :$cwd = $*CWD, :$env) {
        my %env := $env ?? $env.hash !! %*ENV;
        self.status(nqp::p6box_i(nqp::shell(
            nqp::unbox_s($cmd),
            nqp::unbox_s($cwd.Str),
            CLONE-HASH-DECONTAINERIZED(%env),
            $!in_fh, $!out_fh, $!err_fh,
            $!flags
        )));
        self.Bool
    }

    proto method status(|) { * }
    multi method status($new_status) {
        $!exitcode = $new_status +> 8;
        $!signal   = $new_status +& 0xFF;
    }
    multi method status(Proc:D:)  { ($!exitcode +< 8) +| $!signal }
    multi method Numeric(Proc:D:) { $!exitcode }
    multi method Bool(Proc:D:)    { $!exitcode == 0 }

    method sink(--> Nil) {
        X::Proc::Unsuccessful.new(:proc(self)).throw unless self;
    }
}

sub run(*@args ($, *@), :$in = '-', :$out = '-', :$err = '-',
        Bool :$bin, Bool :$chomp = True, Bool :$merge,
        Str:D :$enc = 'utf8', Str:D :$nl = "\n", :$cwd = $*CWD, :$env) {
    my $proc = Proc.new(:$in, :$out, :$err, :$bin, :$chomp, :$merge, :$enc, :$nl);
    $proc.spawn(@args, :$cwd, :$env);
    $proc
}

sub shell($cmd, :$in = '-', :$out = '-', :$err = '-',
        Bool :$bin, Bool :$chomp = True, Bool :$merge,
        Str:D :$enc = 'utf8', Str:D :$nl = "\n", :$cwd = $*CWD, :$env) {
    my $proc = Proc.new(:$in, :$out, :$err, :$bin, :$chomp, :$merge, :$enc, :$nl);
    $proc.shell($cmd, :$cwd, :$env);
    $proc
}

sub QX($cmd, :$cwd = $*CWD, :$env) {
    my %env := $env ?? $env.hash !! %*ENV;
    my Mu $pio := nqp::syncpipe();
    my $status := nqp::shell(
        nqp::unbox_s($cmd),
        nqp::unbox_s($cwd.Str),
        CLONE-HASH-DECONTAINERIZED(%env),
        nqp::null(), $pio, nqp::null(),
        nqp::const::PIPE_INHERIT_IN + nqp::const::PIPE_CAPTURE_OUT + nqp::const::PIPE_INHERIT_ERR
    );
    my $result;
    try {
        $result = nqp::p6box_s(nqp::readallfh($pio));
        $status := nqp::closefh_i($pio);
    }
    fail "Unable to read from '$cmd'" unless $result.DEFINITE;
    $result;
}

# vim: ft=perl6 expandtab sw=4
