use Test;

plan 24;

# A lookbehind is compiled by reversing its pattern and matching against the
# reversed target string. That is only possible for pattern nodes the
# compiler knows how to reverse: subrule calls and embedded code have
# compiled bodies that always match left-to-right, and zero-width checks
# other than anchors inspect the character on the wrong side of a reversed
# position. Lookbehinds containing any of those instead scan for a match of
# the unreversed pattern that ends at the current position.

my regex ab { ab }

is ('ab ba' ~~ / <?after ab > /).pos, 2,
    'lookbehind on a plain literal matches where the literal ends';
is ('ab ba' ~~ / <?after <ab> > /).pos, 2,
    'lookbehind calling a named regex matches where that regex ends';
is ('ab ba' ~~ / <?after <{'ab'}> > /).pos, 2,
    'lookbehind with an interpolated pattern matches where it ends';
is ('ab ba' ~~ / <?after <{'ba'}> > /).pos, 5,
    'lookbehind with a different interpolated pattern matches where it ends';

my token tab { ab }
is ('ab ba' ~~ / <?after <tab> > /).pos, 2,
    'lookbehind calling a ratcheted token matches where the token ends';

my regex aab { a || ab }
is ('zab' ~~ / <?after <aab>> /).pos, 2,
    'lookbehind subrule with sequential alternation matches its first ending here';
ok ?('zb ab' ~~ / <?after <aab>> $ /),
    'lookbehind backtracks into a subrule whose preferred match ends elsewhere';
ok ?('xab' ~~ / <?after <aab>> $ /),
    'lookbehind finds the longer alternative ending at the current position';

ok ?('(x)' ~~ / <?after [ '(' ~ ')' x ]> /),
    'goal matching inside a lookbehind matches the text behind';

ok ?('abc' ~~ / abc <?after a <?before b> bc> /),
    'lookahead nested inside a lookbehind checks the text ahead of its position';

ok ?('ab' ~~ / ab <?after a <?[b]> b> /),
    'zero-width character peek inside a lookbehind checks the character ahead';
nok ?('ab' ~~ / ab <?after a <?[a]> b> /),
    'zero-width character peek inside a lookbehind does not check the character behind';

ok ?('aq' ~~ / aq <?after <[a..z] - [f]>> /),
    'character class subtraction inside a lookbehind matches an included character';
nok ?('af' ~~ / af <?after <[a..z] - [f]>> /),
    'character class subtraction inside a lookbehind rejects a subtracted character';

ok ?('ab' ~~ / ab <?after ^ <ab>> /),
    'start anchor inside a scanning lookbehind holds at the start of the string';
nok ?('zab' ~~ / zab <?after ^ <ab>> /),
    'start anchor inside a scanning lookbehind fails away from the start';

is ('ab ba' ~~ / <!after <ab>> b /).pos, 2,
    'negated lookbehind with a subrule matches where the subrule cannot end';

'ab ba' ~~ / <after ab> /;
is $<after>.from, 2,
    'capturing lookbehind on the reversing path starts its capture at the current position';
is $<after>.pos, 2,
    'capturing lookbehind on the reversing path ends its capture at the current position';

'ab ba' ~~ / <after <ab>> /;
is $<after>.from, 2,
    'capturing lookbehind on the scanning path starts its capture at the current position';
is $<after>.pos, 2,
    'capturing lookbehind on the scanning path ends its capture at the current position';

my @origs;
'aab' ~~ / <?after a { @origs.push(~$/.orig) } ab> /;
is @origs.tail, 'aab',
    'a code block inside a scanning lookbehind sees the unreversed target';

ok ?(('x' x 1000 ~ 'ab') ~~ / <?after <ab>> $ /),
    'scanning lookbehind succeeds at the end of a long string';
nok ?(('x' x 1000) ~~ / <?after <ab>> $ /),
    'scanning lookbehind fails at the end of a long string with no match';
