use Test;

plan 10;

# In a `rule` (ratchet + sigspace) the ratchet of a quantified atom belongs on
# the atom together with the whitespace the rule inserts after it, not on the
# quantifier itself. So a default quantifier can still give back its match
# across that whitespace when what follows needs the same input. Ratcheting the
# quantifier directly stopped `<foo>? <bar>` from backtracking, so a rule where
# `<foo>` could also match `<bar>`'s text failed.

grammar G {
    token word { \w+ }
    token val  { 'FALSE' }
    rule  optword { <word>? <val> }
    token tokword { <word>? <val> }
}

ok G.parse('FALSE', :rule<optword>),
    'a default subrule quantifier in a rule backtracks across inserted whitespace';

ok G.parse('id FALSE', :rule<optword>),
    'the same rule still matches when the optional subrule is present';

# A token is ratchet without sigspace, so its quantifier does not backtrack.
nok G.parse('FALSE', :rule<tokword>),
    'the same shape in a token does not backtrack (ratchet, no whitespace)';

# The reduced ASN.1 case that surfaced this: a SEQUENCE field with a DEFAULT.
grammar Seq {
    rule  TOP   { 'SEQUENCE' '{' <field>+ % ',' '}' }
    rule  field { <name> <type> <default>? }
    token name  { \w+ }
    token type  { 'BOOLEAN' | 'INTEGER' }
    rule  default { 'DEFAULT' <name>? <bool> }
    token bool  { 'TRUE' | 'FALSE' }
}
ok Seq.parse('SEQUENCE { a BOOLEAN DEFAULT FALSE }'),
    'DEFAULT FALSE parses: the optional name gives FALSE back to the value';
ok Seq.parse('SEQUENCE { a BOOLEAN DEFAULT def TRUE }'),
    'DEFAULT with an explicit name still parses';

# An explicit ratchet modifier on the quantifier is honoured, not backtracked.
grammar E {
    token word { \w+ }
    token val  { 'FALSE' }
    rule  r { <word>?: <val> }
}
nok E.parse('FALSE', :rule<r>),
    'an explicit ratchet quantifier (?:) in a rule does not backtrack';

# The same give-back applies to the other quantifier shapes in a rule.
grammar Q {
    token word { \w+ }
    token val  { 'FALSE' }
    rule  sep { <word>+ %% ' ' <val> }
    rule  dyn { <word> ** {0..1} <val> }
}
ok Q.parse('abc FALSE', :rule<sep>),
    'a %%-separated quantifier in a rule gives an iteration back';
ok Q.parse('FALSE', :rule<dyn>),
    'a ** {..} block-range quantifier in a rule backtracks';

# The ratchet still applies inside positions that own their own body.
{
    my token cap { (\w+) . }
    nok 'abc' ~~ /^ <cap> $/,
        'a quantifier inside a capture group in a token stays ratcheted';
}
{
    my token alt { 'ab' | 'a' }
    ok 'ax' ~~ / <alt> 'x' /,
        'a token alternation can still be re-entered for its other branch';
}

# vim: expandtab shiftwidth=4
