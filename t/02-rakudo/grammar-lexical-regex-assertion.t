use Test;

plan 3;

# A capturing <foo> assertion resolves to a lexically visible regex when
# the grammar has no method of that name, and compiles to a call to that
# lexical. The grammar sanity check must treat such an assertion as
# resolved rather than reporting an unresolved reference; grammars like
# Cro::HTTP::Cookie::CookieString rely on this.

{
    my token domain { \w+ }
    my grammar G {
        token TOP { <domain> }
    }
    my $m = G.parse("abc");
    ok $m, 'grammar using a lexical token via <name> compiles and parses';
    is ~$m<domain>, 'abc', 'and the assertion captures under its name';
}

{
    my token wins { \d+ }
    my grammar Both {
        token wins { \w+ }
        token TOP { <wins> }
    }
    nok Both.parse("abc"),
        'a lexical regex takes precedence over a method of the same name';
}

# vim: expandtab shiftwidth=4
