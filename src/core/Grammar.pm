my class Grammar is Cursor { 
    method parse($target, :$rule = 'TOP', :$actions, *%opt) {
        my $*ACTIONS = $actions;
        pir::find_caller_lex__Ps('$/') =
            self."!cursor_init"($target, |%opt)."$rule"().MATCH;
    }
    method parsefile(Cool $filename as Str, *%opts) {
        my $fh    := open($filename);
        my $match := self.parse($fh.slurp, |%opts);
        $fh.close;
        pir::find_caller_lex__Ps('$/') = $match;
    }
}
