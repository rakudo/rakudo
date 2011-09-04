my class Grammar is Cursor { 
    method parse($target, :$rule = 'TOP', *%opt) {
        self."!cursor_init"($target, |%opt)."$rule"().MATCH;
    }
}
