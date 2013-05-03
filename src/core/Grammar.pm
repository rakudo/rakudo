my class Grammar is Cursor { 
    method parse($target, :$rule = 'TOP', Mu :$actions = Mu, *%opt) {
        my $*ACTIONS = $actions;
        nqp::getlexcaller('$/') =
            self."!cursor_init"($target, |%opt)."$rule"().MATCH;
    }
    method parsefile(Cool $filename as Str, *%opts) {
        my $fh    := open($filename);
        my $match := self.parse($fh.slurp, |%opts);
        $fh.close;
        nqp::getlexcaller('$/') = $match;
    }
}
