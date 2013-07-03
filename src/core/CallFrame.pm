my class CallFrame {
    has Int $.level;
    has %.annotations;
    has %.my;
    method new(Int :$level = 0) {
        my $l = $level + 1;
        my $self := nqp::create(CallFrame);
#?if parrot
        my Mu $interp := pir::getinterp__P;
        nqp::bindattr($self, CallFrame, '%!annotations',
            Q:PIR {
                .local pmc interp, annon
                .local int level

                interp = find_lex '$interp'
                $P0    = find_lex '$l'
                level  = repr_unbox_int $P0

                annon  = interp["annotations"; level]
                %r     = nqp_hllize annon
            }
        );

        my Mu $lexpad := Q:PIR {
            .local pmc interp
            .local int level

            interp = find_lex '$interp'
            $P0    = find_lex '$l'
            level  = $P0

            # no idea why we need this:
            %r = interp["lexpad"; level]
        };
        my $h := nqp::create(EnumMap);
        nqp::bindattr($h, EnumMap, '$!storage', $lexpad);
        nqp::bindattr($self, CallFrame, '%!my', $h);
#?endif
#?if !parrot
        my $i = $l;
        my Mu $ctx := nqp::ctx();
        while $i-- {
            $ctx := nqp::ctxcaller($ctx);
        }
        my $h := nqp::create(EnumMap);
        nqp::bindattr($h, EnumMap, '$!storage', $ctx);
        nqp::bindattr($self, CallFrame, '%!my', $h);
        
        my $e  := nqp::handle(nqp::die(''), 'CATCH', nqp::exception());
        my $bt := nqp::backtrace($e);
        nqp::bindattr($self, CallFrame, '%!annotations',
            nqp::hllize(nqp::atkey(nqp::atpos($bt, $l), 'annotations')));
#?endif

        $self;
    }

    method line() {
        %.annotations<line>;
    }
    method file() {
        %.annotations<file>;
    }

    method callframe(Int $level = 0) {
        X::NYI.new(feature => 'Callframe.callframe').throw;
    }
}

sub callframe(Int $level = 0) {
    CallFrame.new(level => ($level + 1));
}

# vim: ft=perl6
