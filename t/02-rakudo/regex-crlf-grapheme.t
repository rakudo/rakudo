use Test;

plan 9;

# In NFG strings a CR followed by an LF is a single `\r\n` grapheme. A regex
# `\r\n` must match that grapheme even though neither `\r` nor `\n` alone does.

ok "a\r\nb" ~~ /\r\n/,
    'a regex \r\n matches a combined CR LF grapheme';

is ("a\r\nb\r\nc".split(/\r\n/)).join('|'), 'a|b|c',
    'splitting on /\r\n/ breaks at each CR LF grapheme';

"a\r\nb" ~~ /\r\n/;
is "$/.from()-$/.to()", '1-2',
    'a /\r\n/ match consumes exactly the one combined grapheme';

ok "head\r\n\r\nbody" ~~ /\r\n\r\n/,
    'a regex \r\n\r\n matches a blank-line header terminator';

# A negated half must not fuse into the CR LF literal.
ok "a\r\nb" ~~ /\R\n/,
    'a negated \R followed by \n does not fuse and still matches';
nok "a\r\nb" ~~ /\r\N/,
    'a \r followed by a negated \N does not fuse';

# A lone \n still matches a combined grapheme through the newline class.
ok "a\r\nb" ~~ /\n/,
    'a lone \n still matches a CR LF grapheme via the newline class';

# Fusion keys off the \r and \n metachars, not any CR/LF source form. A CR
# written as a codepoint escape is a different node and must not fuse with \n.
nok "a\r\nb" ~~ /\x0D\n/,
    'a \x0D codepoint escape does not fuse with \n';

# A quantified \n+ is its own atom, so \r does not fuse with it and is left as
# a bare CR matcher, which finds no bare CR (the CR is in the \r\n grapheme).
nok "x\r\ny" ~~ /\r\n+/,
    'a \r followed by a quantified \n+ does not fuse into a \r\n literal';

# vim: expandtab shiftwidth=4
