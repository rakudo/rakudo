use Test;

plan 25;

# Interpolating the same string object under different modifiers must not
# reuse the regex compiled for the first set of modifiers.
{
    my $pattern = "ab" ~ "c";
    nok "ABC" ~~ /<$pattern>/,
        'same string object matches case sensitively without :i';
    ok "ABC" ~~ /:i <$pattern>/,
        'same string object matches case insensitively under :i';
    ok "abc" ~~ /<$pattern>/,
        'same string object still matches case sensitively after :i use';
}
{
    my $accent = "a" ~ "";
    nok "á" ~~ /<$accent>/,
        'same string object matches marks sensitively without :m';
    ok "á" ~~ /:m <$accent>/,
        'same string object ignores marks under :m';
    nok "á" ~~ /<$accent>/,
        'same string object still matches marks sensitively after :m use';
    nok "Á" ~~ /:i <$accent>/,
        ':i alone still respects marks on the same string object';
    ok "Á" ~~ /:i:m <$accent>/,
        ':i:m combines case and mark insensitivity on the same string object';
}

# Equal pattern text carried by distinct string objects yields a working
# regex every time.
{
    my int $matched;
    for ^20 {
        ++$matched if "foo123" ~~ /<{ "f" ~ Q/oo\d+/ }>/;
    }
    is $matched, 20,
        'code interpolation producing a fresh string object matches every time';
}

# Equal pattern text under different modifiers stays distinct even when
# every interpolation produces a fresh string object.
{
    ok "ABC" ~~ /:i <{ "a" ~ "bc" }>/,
        ':i applies to freshly interpolated pattern text';
    nok "ABC" ~~ /<{ "a" ~ "bc" }>/,
        'equal pattern text without :i stays case sensitive';
}

# Elements of an interpolated array are compiled like single strings.
{
    my @alts = "abc", "def";
    ok "xdefy" ~~ /<@alts>/,
        'array alternation matches an element';
    nok "DEF" ~~ /<@alts>/,
        'array alternation is case sensitive by default';
    ok "DEF" ~~ /:i <@alts>/,
        'the same array matches case insensitively under :i';
    ok "xabcy" ~~ /<@alts>/,
        'the same array still matches case sensitively after :i use';
}

# Pattern text referencing the lexical scope of the interpolation site
# must be compiled against the scope of each use, not reused from an
# earlier use of equal text elsewhere.
{
    my @pat = Q/$/, "x";
    my sub match-lexical($x, $target) {
        so $target ~~ /^<{ @pat.join }>$/
    }
    ok match-lexical("AAA", "AAA"),
        'interpolated pattern referencing a caller lexical matches on the first call';
    ok match-lexical("BBB", "BBB"),
        'equal pattern text binds the current caller lexical on later calls';

    my regex vowel { <[aeiou]> }
    my sub narrower() {
        my regex vowel { <[xyz]> }
        so "x" ~~ /<{ "<&vo" ~ "wel>" }>/
    }
    ok narrower(),
        'interpolated subrule call resolves the local lexical regex';
    ok "e" ~~ /<{ "<&vo" ~ "wel>" }>/,
        'equal pattern text resolves the outer lexical regex afterward';
}

# A regex interpolated where code interpolation was allowed must not be
# reused where it is prohibited, whether the pattern text arrives in the
# same string object or a fresh one.  Test exports a MONKEY-SEE-NO-EVAL
# that allows code interpolation everywhere, so the prohibited scopes
# must opt out explicitly.
{
    use MONKEY-SEE-NO-EVAL;
    my $pattern = Q/a <?{ True }> b/;
    ok "ab" ~~ /<$pattern>/,
        'code assertion interpolates where code interpolation is allowed';
    {
        no MONKEY-SEE-NO-EVAL;
        throws-like { "ab" ~~ /<$pattern>/ }, X::SecurityPolicy::Eval,
            'the same string object still dies where code interpolation is prohibited';
        my $copy = $pattern.substr(0);
        throws-like { "ab" ~~ /<$copy>/ }, X::SecurityPolicy::Eval,
            'a fresh object with equal pattern text also dies there';
    }
}

# A prohibited interpolation that throws must not poison a later allowed
# interpolation of the same pattern text.
{
    {
        no MONKEY-SEE-NO-EVAL;
        my $pattern = Q/x <?{ True }> y/;
        throws-like { "xy" ~~ /<$pattern>/ }, X::SecurityPolicy::Eval,
            'code assertion dies where code interpolation is prohibited';
    }
    {
        use MONKEY-SEE-NO-EVAL;
        my $pattern = Q/x <?{ True }> y/;
        ok "xy" ~~ /<$pattern>/,
            'the same pattern text compiles where code interpolation is allowed afterward';
    }
}

# Filling the source cache past its capacity must not break matching of
# evicted or later patterns.
{
    my int $matched;
    for ^1100 -> $i {
        ++$matched if "n$i" ~~ /<{ "n" ~ $i }>/;
    }
    ++$matched if "n7" ~~ /<{ "n" ~ 7 }>/;
    is $matched, 1101,
        'distinct pattern texts past the cache capacity all still match';
}

# vim: expandtab shiftwidth=4
