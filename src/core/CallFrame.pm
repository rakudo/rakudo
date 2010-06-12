class CallFrame {
    has $!interp;
    has $!level = 2;
    method !annotations {
        my $i = $!interp;
        my $l = $!level;
        Q:PIR {
            .local pmc interp
            .local int level

            interp = find_lex '$i'
            $P0    = find_lex '$l'
            level  = $P0

            %r     = interp["annotations"; level]
        }
    }

    method line() {
        my $ann = self!annotations();
        $ann<line>;
    }
    method file() {
        my $ann = self!annotations();
        $ann<file>;
    }

    method callframe(Int $level) {
        CallFrame.new(:interp($!interp), :level($!level + $level));
    }

    method my() {
        my $i = $!interp;
        my $l = $!level;
        my $pad = Q:PIR {
            .local pmc interp
            .local int level

            interp = find_lex '$i'
            $P0    = find_lex '$l'
            level  = $P0

            # no idea why we need this:
            dec level

            %r     = interp["lexpad"; level]
        };
        ::LexPad.new(parrot_lexpad => $pad);
    }
}

our multi sub callframe(Int $level = 0) {
    my $interp = pir::getinterp__p();
    CallFrame.new(:$interp, level => ($level + 2));
}

# vim: ft=perl6
