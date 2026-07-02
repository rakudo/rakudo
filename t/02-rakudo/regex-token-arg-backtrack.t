use Test;

plan 6;

# A token (or rule) is ratchet, so when a call to it is backtracked over the
# cursor must not be re-entered to look for another match. A token that takes
# an argument would otherwise be re-invoked without it.

{
    my token t ($x) { $x }
    ok 'xaby' ~~ / 'x' [ <&t: 'a'> | <&t: 'ab'> ] 'y' /,
        'backtracking an alternation of argument tokens tries the next branch';
}

{
    my token t ($x) { $x }
    nok 'xaz' ~~ / 'x' [ <&t: 'a'> | <&t: 'ab'> ] 'y' /,
        'exhausting both branches fails cleanly instead of erroring';
}

{
    my token t ($x) { $x }
    ok 'xaby' ~~ / 'x' [ <&t: 'a'> || <&t: 'ab'> ] 'y' /,
        'sequential alternation of argument tokens backtracks too';
}

{
    my token t ($x) { $x }
    ok 'ab' ~~ / <&t: 'ab'> /, 'a single argument token call still matches';
}

{
    my token t { 'a' }
    my token u { 'ab' }
    ok 'xaby' ~~ / 'x' [ <t> | <u> ] 'y' /,
        'backtracking argumentless tokens is unaffected';
}

{
    my regex r ($x) { $x+ }
    ok 'xaaaz' ~~ / 'x' <&r: 'a'> 'z' /,
        'a regex (non-ratchet) with an argument can still be backtracked into';
}
