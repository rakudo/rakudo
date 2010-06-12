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
}

our multi sub callframe($level = 0) {
    my $interp = pir::getinterp__p();
    CallFrame.new(:$interp, level => $level + 2);
}
