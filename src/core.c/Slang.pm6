class Slang {
    has $.grammar;
    has $.actions;
    multi method gist(Slang:D:) {
        # Handle NQP objects like Perl6::Grammar
        'Slang.new('
            ~ (':grammar(' ~ $!grammar.^name ~ ')',
               ':actions(' ~ $!actions.^name ~ ')').join(', ')
            ~ ')'
    }
    method parse (|c) {
        $!grammar.parse(:$!actions, |c);
    }
}

# vim: expandtab shiftwidth=4
