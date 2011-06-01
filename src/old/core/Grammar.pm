class Grammar is Cursor {
    method parsefile($file, *%options) {
        my $fh = open($file, :r)
            || die "$file: $!";
        my $str = $fh.slurp;
        self.parse($str, |%options);
    }
    method parse($str, :$actions, :$action, *%options) {
        if $action {
            die(":action has been renamed to :actions in the Grammar.parse method")
        }
        my $match = callwith($str, :$actions, |%options);
        pir::store_dynamic_lex__vSP('$/', $match);
        $match;
    }
}

our sub make (*@ast) {
    my $ast = (sub { return |@ast })();

    Q:PIR {
        $P0 = find_dynamic_lex '$/'
        $P1 = find_lex '$ast'
        $P0.'!make'($P1)
    }
}
