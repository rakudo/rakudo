use Test;

plan 4;

# A constant that is not a string interpolates into a regex at runtime instead
# of being treated as a literal.
ok EVAL('my constant @w = <foo bar baz>; "bar" ~~ / @w /'),
    'a constant array interpolates as an alternation';

ok EVAL('my constant $n = 42; "x42x" ~~ / $n /'),
    'a constant integer interpolates by its string value';

ok EVAL('my constant $s = "foo"; "xfoox" ~~ / $s /'),
    'a constant string is still matched as a literal';

# The grammar pattern from Contact::Name: a token interpolating a constant
# array under :i.
ok EVAL('my constant @w = <Mr Dr>; grammar G { token p { :i @w } }; G.parse("dr", :rule<p>)'),
    'a constant array interpolates in a grammar token under :i';

# vim: expandtab shiftwidth=4
