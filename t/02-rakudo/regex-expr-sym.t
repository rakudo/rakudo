use Test;

plan 7;

# A proto regex candidate may name its sym with an expression rather than a
# literal: `token foo:sym(EXPR) {...}`. The implicit `<sym>` token must match
# the evaluated value of EXPR, not an empty string.

my enum Opt (MARK => "Q");

grammar G {
    proto token tok {*}
    token tok:sym(MARK)     { <option=.sym> <body> }
    token tok:sym<fallback> { $<option>=[.] <body> }
    token body { . }
}
class GActions {
    method tok:sym(MARK)($/)     { make 'expr' }
    method tok:sym<fallback>($/) { make 'fallback' }
    method body($/) { }
}

is G.parse("QZ", :rule<tok>, :actions(GActions)).ast, 'expr',
    'a :sym(EXPR) candidate matches the evaluated sym value';
is G.parse("QZ", :rule<tok>, :actions(GActions))<option>, 'Q',
    'the <sym> token captures the evaluated sym value';
is G.parse("RZ", :rule<tok>, :actions(GActions)).ast, 'fallback',
    'input not matching the evaluated sym falls through to the other candidate';

# A literal :sym<...> candidate is unaffected.
grammar S {
    proto token tok {*}
    token tok:sym<foo> { <sym> }
    token tok:sym<x>   { . }
}
ok S.parse("foo", :rule<tok>),
    'a literal :sym<...> candidate still matches its sym';
ok S.parse("z", :rule<tok>),
    'a literal :sym<...> proto still falls through to another candidate';

# A sym given by a constant-foldable expression matches its value.
grammar Folded {
    proto token tok {*}
    token tok:sym("a" ~ "b") { <sym> }
    token tok:sym<x>         { . }
}
ok Folded.parse("ab", :rule<tok>),
    'a constant-foldable expression sym evaluates and matches';

# A sym whose value is only known by running code is evaluated at BEGIN time,
# the same as the legacy frontend does.
sub sym-value { "go" }
grammar BeginTime {
    proto token tok {*}
    token tok:sym(sym-value()) { <sym> }
    token tok:sym<x>           { . }
}
ok BeginTime.parse("go", :rule<tok>),
    'a sym computed by a BEGIN-time call evaluates and matches';

# vim: expandtab shiftwidth=4
