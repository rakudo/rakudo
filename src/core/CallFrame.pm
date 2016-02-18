my class CallFrame {
    has Int $.level;
    has %.annotations;
    has %.my;
    method new(Int :$level = 0) {
        my $l = $level + 1;
        my $self := nqp::create(CallFrame);
        my $i = $l;
        my Mu $ctx := nqp::ctx();
        while $i-- {
            $ctx := nqp::ctxcaller($ctx);
        }
        my $h := nqp::create(Stash);  # should probably be PseudoStash?
        nqp::bindattr($h, Map, '$!storage', $ctx);
        nqp::bindattr($self, CallFrame, '%!my', $h);
        nqp::bindattr($self, CallFrame, '$!level', $l);

        my $e  := nqp::handle(nqp::die(''), 'CATCH', nqp::exception());
        my $bt := nqp::backtrace($e);
        nqp::bindattr($self, CallFrame, '%!annotations',
            nqp::hllize(nqp::atkey(nqp::atpos($bt, $l), 'annotations')));

        $self;
    }

    method line() {
        %.annotations<line>;
    }
    method file() {
        %.annotations<file>;
    }
    multi method gist(CallFrame:D:) {
        my %annotations := %.annotations;
        "%annotations<file> at line %annotations<line>";
    }
    method code() {
        my $ctx := nqp::getattr(%!my, Map, '$!storage');
        nqp::getcodeobj(nqp::ctxcode($ctx));
    }

    method callframe(Int $level = 0) {
        X::NYI.new(feature => 'Callframe.callframe').throw;
    }
}

sub callframe(Int $level = 0) {
    CallFrame.new(level => ($level + 1));
}

# vim: ft=perl6 expandtab sw=4
