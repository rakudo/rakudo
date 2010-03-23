class Grammar is Regex::Cursor {
    method parsefile($file, *%options) {
        my $fh = open($file, :r)
            || die "$file: $!";
        my $str = $fh.slurp;
        self.parse($str, %options);
    }
    method parse($str, :$actions, :$action) {
        if $action {
            die(":action has been renamed to :actions in the Grammar.parse method")
        }
        nextwith($str, :$actions);
    }
}

our sub make($ast) {
    Q:PIR {
        $P0 = find_dynamic_lex '$/'
        $P1 = find_lex '$ast'
        $P0.'!make'($P1)
    }
}
