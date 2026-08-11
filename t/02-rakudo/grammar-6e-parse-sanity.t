use v6.e.PREVIEW;
use Test;

# A rule that does not return a Match must be reported by name instead
# of failing on the first Match operation applied to its return value

plan 3;

grammar NotAMatch {
    method TOP($?) { 42 }
}

throws-like { NotAMatch.parse("x") }, Exception,
    message => /"'TOP' returned a Int object (42) rather than a Match object"/,
    'a TOP returning a non-Match is reported by name';

grammar NotAMatchRule {
    method custom($?) { "oops" }
}

throws-like { NotAMatchRule.parse("x", :rule<custom>) }, Exception,
    message => /"'custom' returned a Str object (oops) rather than a Match object"/,
    'a named rule returning a non-Match is reported by name';

grammar Fine {
    token TOP { \w+ }
}

is Fine.parse("word").Str, 'word',
    'a rule returning a Match still parses';

# vim: expandtab shiftwidth=4
