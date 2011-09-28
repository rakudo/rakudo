my class Grammar is Cursor { 
    method parse($target, :$rule = 'TOP', :$actions, *%opt) {
        my $*ACTIONS = $actions;
        self."!cursor_init"($target, |%opt)."$rule"().MATCH;
    }
}
