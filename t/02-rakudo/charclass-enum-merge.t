use Test;

plan 6;

# A character class that mixes an enumerable class (\v, \h) with literal
# characters merges into one enumeration, so it orders correctly against another
# class under longest-token matching in an alternation.

grammar Link {
    token TOP { [ <-[\s>|]>+ | <-[\v>|]>+ '|' ] '>' }
}
ok Link.parse('ab|>'), 'LTM picks the longer branch of two negated char classes';
ok Link.parse('Some text|>'), 'the same with a space in the text';

# The merged class still matches the same characters.
ok "a\tb" ~~ / <[\h]> /, '\h in a class still matches horizontal space';
ok "x\ny" ~~ / <[\v]> /, '\v in a class still matches vertical space';
is ("foo,bar" ~~ / <-[\v,]>+ /).Str, 'foo',
    '\v merged with a literal in a negated class stops at the literal';
is ("one two" ~~ / <[\h a..z]>+ /).Str, 'one two',
    '\h merged with a range in a positive class matches both';
