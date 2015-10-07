my class Grammar is Cursor {
    method parse($target, :$rule = 'TOP',  Capture() :$args = (), Mu :$actions = Mu, *%opt) {
        my $*ACTIONS = $actions;
        my $result =
            self."!cursor_init"($target, |%opt)."$rule"(|$args).MATCH;
        $result = Nil unless $result.to == $target.chars;
        nqp::getlexcaller('$/') = $result;
    }
    method subparse($target, :$rule = 'TOP', Capture() :$args = (),  Mu :$actions = Mu, *%opt) {
        my $*ACTIONS = $actions;
        nqp::getlexcaller('$/') =
            self."!cursor_init"($target, |%opt)."$rule"(|$args).MATCH;
    }
    method parsefile(Str(Cool) $filename, :$enc, *%opts) {
        my $match := self.parse($filename.IO.slurp(:$enc), |%opts);
        nqp::getlexcaller('$/') = $match;
    }
}

# vim: ft=perl6 expandtab sw=4
