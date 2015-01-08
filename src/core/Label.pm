my class Label {
    has Str $!name;
    has Str $!file;
    has Int $!line;
    has Str $!prematch;
    has Str $!postmatch;
    method new(:$name, :$line, :$prematch, :$postmatch) {
        # XXX Register in &?BLOCK.labels when we have &?BLOCK.
        my $file := nqp::getlexdyn('$?FILES');
        $file    := nqp::isnull($file) ?? '<unkown>' !! nqp::p6box_s($file);
        my $obj  := nqp::create(self);
        nqp::bindattr($obj, Label, '$!name',      $name);
        nqp::bindattr($obj, Label, '$!file',      $file);
        nqp::bindattr($obj, Label, '$!line',      $line);
        nqp::bindattr($obj, Label, '$!prematch',  nqp::p6box_s($prematch));
        nqp::bindattr($obj, Label, '$!postmatch', nqp::p6box_s($postmatch));
        $obj
    }
    method name() {
        $!name
    }

    # XXX method leave(@args)

    multi method gist(Label:D:) {
        my $color = %*ENV<RAKUDO_ERROR_COLOR> // !$*DISTRO.is-win;
        my ($red, $green, $yellow, $clear) = $color
            ?? ("\e[31m", "\e[32m", "\e[33m", "\e[0m")
            !! ("", "", "", "");
        my $eject = $*DISTRO.is-win ?? "<HERE>" !! "\x[23CF]";

        "Label<$!name>(at $!file:$!line, '$green$!prematch$yellow$eject$red$!name$green$!postmatch$clear')"
    }

    method Int() { nqp::where(nqp::decont(self)) }

    # XXX method goto
    method next() {
        my Mu $ex := nqp::newexception();
        nqp::setpayload($ex, nqp::decont(self));
#?if parrot
        nqp::setextype($ex, 512); # XXX create nqp::const::CONTROL_LOOP_NEXT_LABELED?
#?endif
#?if !parrot
        nqp::setextype($ex, nqp::const::CONTROL_NEXT + nqp::const::CONTROL_LABELED);
#?endif
        nqp::throw($ex);
    }
    method redo() {
        my Mu $ex := nqp::newexception();
        nqp::setpayload($ex, nqp::decont(self));
#?if parrot
        nqp::setextype($ex, 513); # XXX create nqp::const::CONTROL_LOOP_REDO_LABELED?
#?endif
#?if !parrot
        nqp::setextype($ex, nqp::const::CONTROL_REDO + nqp::const::CONTROL_LABELED);
#?endif
        nqp::throw($ex);
    }
    method last() {
        my Mu $ex := nqp::newexception();
        nqp::setpayload($ex, nqp::decont(self));
#?if parrot
        nqp::setextype($ex, 514); # XXX create nqp::const::CONTROL_LOOP_LAST_LABELED?
#?endif
#?if !parrot
        nqp::setextype($ex, nqp::const::CONTROL_LAST + nqp::const::CONTROL_LABELED);
#?endif
        nqp::throw($ex);
    }
}

# vim: ft=perl6 expandtab sw=4
