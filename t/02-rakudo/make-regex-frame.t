use Test;

plan 8;

# https://github.com/rakudo/rakudo/issues/1235

# First, before anything in this file has produced a match: make with
# nothing to attach to throws.
{
    throws-like { make 5 }, X::Make::MatchRequired,
      'make with no match anywhere in reach throws';
}

# The make routine attaches to the match of the nearest enclosing regex
# frame, so a match run inside a regex code block cannot redirect it.
{
    grammar ClobberGrammar {
        token TOP { <letters> { if $<letters> ~~ /^a/ { make 42 } } }
        token letters { \w+ }
    }
    is ClobberGrammar.parse("abcd").made, 42,
      'make targets the token match after a smartmatch inside its code block';

    grammar ActionGrammar { token TOP { \d+ } }
    class ActionClass { method TOP($/) { make 3 * 3 } }
    is ActionGrammar.parse("42", :actions(ActionClass)).made, 9,
      'make in an action method attaches to the $/ parameter';

    grammar InlineGrammar { token TOP { \w+ { make "hi" } } }
    is InlineGrammar.parse("x").made, "hi",
      'make in a plain regex code block attaches to the token match';

    grammar NestedGrammar {
        token TOP { <in> { make "O" } }
        token in { \w { make "I" } }
    }
    my $nested = NestedGrammar.parse("x");
    is $nested<in>.made, "I",
      'make in a nested token attaches to that token, not the outer one';
    is $nested.made, "O",
      'make in the outer token is untouched by the inner make';
}

# Outside any regex frame, make attaches to the caller's $/.
{
    my $/ = Match.new;
    make "plain";
    is $/.made, "plain",
      'make outside a regex frame attaches to the Match in the caller $/';
}

# The topic plays no part here: even with a Match as the topic, the
# caller's $/ receives the value.
{
    my $/ = Match.new;
    given Match.new { make "T" }
    is $/.made, "T",
      'make ignores a Match topic and attaches to the caller $/';
}

# vim: expandtab shiftwidth=4
