use Test;

plan 15;

# https://github.com/rakudo/rakudo/issues/1235

# First, before anything in this file has produced a match: make with
# nothing to attach to throws.
{
    throws-like { make 5 }, X::Make::MatchRequired,
      'make with no match anywhere in reach throws';
}

# Inside a regex code block, make attaches to the match of the parse in
# progress, so a match run inside the block cannot redirect it.
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

# In a capture group's code block, $/ is the capture's own match, and
# that is where make attaches, whichever regex frame's cursor the block
# can see.
{
    is-deeply ('ab' ~~ / (\w { make ~$/ }) (\w { make ~$/ }) /)[0, 1].map(*.made).List,
      ('a', 'b'),
      'make in a positional capture attaches to that capture';
    is ('ab' ~~ / $<x>=(\w { make ~$/ }) /)<x>.made, 'a',
      'make in a named capture attaches to that capture';
    is-deeply ('abc' ~~ / ^ [ (\w { make ~$/ }) ]* $ /)[0].map(*.made).List,
      ('a', 'b', 'c'),
      'make in a quantified capture attaches to each match of it';
    is-deeply ('abc' ~~ / ^ [ $<x>=(\w { make ~$/ }) ]* $ /)<x>.map(*.made).List,
      ('a', 'b', 'c'),
      'make in a quantified named capture attaches to each match of it';
    is (BEGIN / ^ [ (\w { make ~$/ }) ]* $ { make $0>>.made.join('-') } /)
         .ACCEPTS('abc').made, 'a-b-c',
      'a later code block of a regex closure created at BEGIN time reads the made of each capture';

    grammar CaptureGrammar { token TOP { (\w) { make 'token' } (\w { make 'capture' }) } }
    class CaptureActions { method TOP($/) { make 'action:' ~ $1.made } }
    my $parsed = CaptureGrammar.parse('ab', :actions(CaptureActions));
    is $parsed[1].made, 'capture',
      'make in a token capture attaches to the capture under an action class';
    is $parsed.made, 'action:capture',
      'make in the action method attaches to the token match after the capture made';
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
