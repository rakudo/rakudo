use Test;

plan 12;

# https://github.com/rakudo/rakudo/issues/4387
# Whitespace inside a regex must not apply unspace: a backslash there
# always starts an escape like \# or \s, so `regex { \# }` matches a
# literal hash instead of commenting out the rest of the line.

is ("#" ~~ regex { \# }), "#",
    'regex {} with a leading escaped hash matches a literal hash';
is ("#" ~~ token { \# }), "#",
    'token {} with a leading escaped hash matches a literal hash';
is ("#" ~~ rule { \# }), "#",
    'rule {} with a leading escaped hash matches a literal hash';

my regex hash { \# }
is ("#" ~~ &hash), "#",
    'a named regex with a leading escaped hash matches a literal hash';

is ("a#b" ~~ rx/ a \# b /), "a#b",
    'an escaped hash after whitespace mid-regex stays an escape';
is ("a #" ~~ rx:s/ a \# /), "a #",
    'an escaped hash after sigspace stays an escape';

my regex spread { \#
    x }
is ("#x" ~~ &spread), "#x",
    'an escaped hash does not comment out the rest of its line';

my regex commented { # a comment
    a }
is ("a" ~~ &commented), "a",
    'a line comment at the start of a regex body still works';

my regex embedded { #`(one
two) ab }
is ("ab" ~~ &embedded), "ab",
    'a multi-line embedded comment at the start of a regex body still works';

is ("a b" ~~ rule { a b }), "a b",
    'rule {} sigspace still applies between atoms';

throws-like q| rx/ a \ b / |, X::Syntax::Regex::Unspace,
    'a backslash before whitespace mid-regex reports the no-unspace error';
throws-like q| my regex r { \ } |, X::Syntax::Regex::Unspace,
    'a backslash before whitespace in a regex body reports the no-unspace error';

# vim: expandtab shiftwidth=4
