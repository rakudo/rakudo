use Test;

plan 38;

# In a ratchet regex with sigspace the whitespace after an atom is attached
# outside the atom, so the atom itself carries the ratchet. A quantifier,
# an alternation, a subrule call or a capture group followed by whitespace
# does not give its match back to what follows, just as it does not without
# the whitespace.

# The `.*` in these matches the whole string, so a further `'$'` can only
# match if the quantifier gives a character back.
{
    my $n = 0;
    nok '!@$' ~~ rx:r:s/^ .* {$n++} '$'/,
        'a ratchet sigspace regex does not backtrack into a quantifier';
    is $n, 1, 'the code block after the quantifier runs once';
}
ok '!@$' ~~ rx:s/.* '$'/,
    'a sigspace regex without ratchet backtracks into the quantifier';
nok '!@$' ~~ rx:r:s/.*<.ws>'$'/,
    'the ratchet holds with the whitespace call spelled out';
nok '!@$' ~~ rx:r/.* '$'/,
    'the ratchet holds without sigspace';
nok '!@$' ~~ rx:r:s/.*: '$'/,
    'an explicit ratchet modifier still does not backtrack';
ok '!@$' ~~ rx:r:s/.*! '$'/,
    'an explicit greedy modifier still backtracks in a ratchet regex';
ok '!@$' ~~ rx:r:s/.*? '$'/,
    'an explicit frugal modifier still extends in a ratchet regex';
ok '!@$' ~~ rx:r:s/ :!r .* '$'/,
    'turning ratchet off again restores backtracking';

# The other quantifier shapes followed by whitespace.
nok '!!@' ~~ rx:r:s/ '!'+ '!@' /,
    'a greedy quantifier followed by whitespace does not give back';
nok '!@' ~~ rx:r:s/ '!'? '!@' /,
    'an optional atom followed by whitespace does not give its match back';
ok '!!@' ~~ rx:r:s/ '!'? '!@' /,
    'the optional atom still matches when its input is there';
nok '!!@' ~~ rx:r:s/ '!' ** 1..2 '!@' /,
    'a range quantifier followed by whitespace does not give back';
nok '!!@' ~~ rx:r:s/ '!' ** {1..2} '!@' /,
    'a block range quantifier followed by whitespace does not give back';
nok '!,!,!@' ~~ rx:r:s/ '!'+ % ',' ',!@' /,
    'a separated quantifier followed by whitespace does not give back';
nok '!!@' ~~ rx:r:s/ '!' + '!@' /,
    'whitespace on both sides of the quantifier ratchets it';

# Alternations, subrule calls and capture groups followed by whitespace are
# not re-entered for another match either.
nok '!@$' ~~ rx:r:s/ [ '!@' || '!' ] '@$' /,
    'a sequential alternation followed by whitespace is not re-entered';
nok '!@$' ~~ rx:r:s/ [ '!@' | '!' ] '@$' /,
    'an alternation followed by whitespace is not re-entered';
nok '!@$' ~~ rx:r:s/ ( '!@' || '!' ) '@$' /,
    'a capture group followed by whitespace is not re-entered';
{
    my regex bangs { '!'+ }
    nok '!!@' ~~ rx:r:s/ <bangs> '!@' /,
        'a subrule call followed by whitespace is not re-entered';
    ok '!!! !@' ~~ rx:r:s/ <bangs> '!@' /,
        'the subrule call still matches when its input is there';
    nok '!!@' ~~ rx:s/ <bangs>: '!@' /,
        'an explicit ratchet on a subrule call followed by whitespace holds without :r';
    nok '!!@' ~~ rx:r:s/ <bangs>: '!@' /,
        'an explicit ratchet on a subrule call followed by whitespace holds with :r';
}

# A rule is a ratchet sigspace regex, so a subrule that also matches the
# next atom's input does not give it back.
grammar G {
    token word { \w+ }
    token val  { 'FALSE' }
    rule  optword { <word>? <val> }
    token tokword { <word>? <val> }
}
nok G.parse('FALSE', :rule<optword>),
    'an optional subrule in a rule does not give its match back';
ok G.parse('id FALSE', :rule<optword>),
    'the rule still matches when the optional subrule has its own input';
nok G.parse('FALSE', :rule<tokword>),
    'the same shape in a token does not give its match back either';

grammar E {
    token word { \w+ }
    token val  { 'FALSE' }
    rule  r { <word>?: <val> }
}
nok E.parse('FALSE', :rule<r>),
    'an explicit ratchet quantifier in a rule does not give back';

grammar Q {
    token word { \w+ }
    token val  { 'FALSE' }
    rule  sep { <word>+ %% ' ' <val> }
    rule  dyn { <word> ** {0..1} <val> }
}
nok Q.parse('abc FALSE', :rule<sep>),
    'a quantifier with a %% separator in a rule does not give an iteration back';
nok Q.parse('FALSE', :rule<dyn>),
    'a ** {..} block range quantifier in a rule does not give back';

# A %% trailing separator wraps its quantifier in a concat the same way
# whitespace does, so the quantifier is ratcheted inside it, also without
# sigspace, and the trailing separator follows the quantifier's backtracking.
nok 'a a FALSE' ~~ token { \w+ %% ' ' FALSE },
    'a quantifier with a %% separator in a token does not give an iteration back';
ok 'a a  FALSE' ~~ token { \w+ %% ' ' ' FALSE' },
    'the quantifier with a %% separator still matches with its trailing separator';
nok '!,' ~~ token { '!'+ %% ',' ',' },
    'the trailing separator of a ratcheted quantifier does not give back';
ok '!,' ~~ token { '!'+! %% ',' ',' },
    'the trailing separator of an explicitly greedy quantifier gives back';
ok '!,' ~~ token { '!'+? %% ',' ',' },
    'the trailing separator of an explicitly frugal quantifier gives back';
nok '!,' ~~ token { '!'+: %% ',' ',' },
    'the trailing separator of an explicitly ratcheted quantifier does not give back';
nok '!,' ~~ rx/ '!'+: %% ',' ',' /,
    'the trailing separator follows an explicit ratchet without :r as well';
ok 'a a FALSE' ~~ token { \w+! %% ' ' FALSE },
    'an explicitly greedy quantifier with a %% separator still gives an iteration back';

# The ratchet also applies inside a capture group's body.
{
    my token cap { (\w+) . }
    nok 'abc' ~~ /^ <cap> $/,
        'a quantifier inside a capture group in a token stays ratcheted';
}

# vim: expandtab shiftwidth=4
