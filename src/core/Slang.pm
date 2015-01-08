class Slang {
    has $.grammar;
    has $.actions;
    multi method gist(Slang:D:) {
        # Handle NQP objects like Perl6::Grammar
        'Slang.new('
            ~ (':grammar('~$!grammar.HOW.name($!grammar)~')',
               ':actions('~$!actions.HOW.name($!actions)~')').join(', ')
            ~ ')'
    }
    method parse (|c) {
        $!grammar.parse(:$!actions, |c);
    }
}
