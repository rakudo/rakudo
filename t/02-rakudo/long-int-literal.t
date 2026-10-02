use nqp;
use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 33;

is EVAL('1' x 37), (10**37 - 1) div 9,
  'a decimal literal longer than a native int parses exactly';
is EVAL('9' x 19), 10**19 - 1,
  'the shortest run of digits that overflows a native int parses exactly';
is EVAL('١٢٣٤٥٦٧٨٩٠' x 4), 1234567890 * (10**30 + 10**20 + 10**10 + 1),
  'a long literal of non-ASCII decimal digits parses exactly';

is EVAL('1234567890_1234567890_1234567890_1234567890'),
  1234567890 * (10**30 + 10**20 + 10**10 + 1),
  'underscores between digit groups are skipped in a long literal';

is EVAL('123456789_0123456789'), 1234567890123456789,
  'an underscore halfway through a long literal is skipped';

is EVAL('1_2345678901234567890_1'), 123456789012345678901,
  'underscores near both ends of a long literal are skipped';

is EVAL('1_2_3_4_5_6_7_8_9_0_1_2_3_4_5_6_7_8_9_0'), 12345678901234567890,
  'a long literal with an underscore between every digit parses';

is EVAL('-1_2_3_4_5_6_7_8_9_0_1_2_3_4_5_6_7_8_9_0'), -12345678901234567890,
  'negating a long literal with an underscore between every digit works';

is EVAL('-Ⅼ'), -50,
  'negating a unicode numeral works';
is EVAL('sub (-Ⅼ) { "matched" }(-50)'), 'matched',
  'a negative unicode numeral type constraint dispatches';
is EVAL('sub (-1234567890_1234567890_1234567890) { "matched" }(-123456789012345678901234567890)'),
  'matched',
  'a negative literal type constraint with underscores dispatches';

ok EVAL(q{'a' x 12 ~~ / ^ a ** 1_0..1_2 $ /}),
  'a regex quantifier range with underscores in its bounds matches';
ok EVAL(q{'aaa' ~~ / ^ a ** 0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_3 $ /}),
  'a regex quantifier bound with an underscore between every digit matches';

'abcdefghij' ~~ /(.)(.)(.)(.)(.)(.)(.)(.)(.)/;
is EVAL('$007'), 'h',
  'a capture index with leading zeros is the same capture';
is EVAL('$' ~ '0' x 25 ~ '8'), 'i',
  'a capture index longer than a native int is still exact';

my $builder = RakuAST::LiteralBuilder.new;
is $builder.intern-Int('1' ~ '_2' x 20), 1 ~ '2' x 20,
  'the literal builder skips an underscore between every digit';
is $builder.intern-Int('-4_2'), -42,
  'the literal builder negates a signed number with an underscore';
is $builder.intern-Int('+' ~ '9' x 40), 10**40 - 1,
  'the literal builder accepts a leading plus on a long number';
is $builder.intern-Int("\x[2212]" ~ '9' x 40), 1 - 10**40,
  'the literal builder accepts a leading minus sign character';
throws-like { $builder.intern-Int('1' x 19 ~ '__' ~ '1' x 19) }, Exception,
  message => /"'" "1" ** 19 "__" "1" ** 19 "'" .* "is not a valid number"/,
  'the literal builder reports the whole number when one half is malformed';
dies-ok { $builder.intern-Int('1' x 20 ~ '__' ~ '2' x 20) },
  'the literal builder rejects a double underscore in a long number';
dies-ok { $builder.intern-Int('1' x 18 ~ '__' ~ '2' x 20) },
  'the literal builder rejects a double underscore just before the split';
dies-ok { $builder.intern-Int('1' x 20 ~ '__' ~ '2' x 18) },
  'the literal builder rejects a double underscore just after the split';
dies-ok { $builder.intern-Int('_' ~ '1' x 30) },
  'the literal builder rejects a leading underscore';
dies-ok { $builder.intern-Int('1' x 30 ~ '_') },
  'the literal builder rejects a trailing underscore';
dies-ok { $builder.intern-Int('1' x 20 ~ 'x' ~ '1' x 20) },
  'the literal builder rejects a letter inside a long number';
dies-ok { $builder.intern-Int('-') },
  'the literal builder rejects a bare sign';

my $reported = 0;
is $builder.intern-Int-by-base('1' x 20 ~ '__' ~ '2' x 20, 10, { $reported++; 'reported' }),
  'reported',
  'the literal builder returns what the base 10 error reporter returns';
is $reported, 1,
  'the literal builder calls the base 10 error reporter once';
dies-ok { $builder.intern-Int-by-base('1' x 20 ~ '__' ~ '2' x 20, 10) },
  'the literal builder dies on a malformed base 10 number without a reporter';

{
    my $start = now;
    EVAL('sub { $' ~ '1' x 1000000 ~ ' }');
    ok now - $start < 20,
      'compiling a capture index with a million digits finishes in bounded time';
}

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    my $start = now;
    my $value = EVAL('1' x 1000000);
    ok now - $start < 20,
      'compiling a decimal literal with a million digits finishes in bounded time';
    ok $value == (10**1000000 - 1) div 9,
      'a decimal literal with a million digits parses exactly';
}
else {
    skip 'the legacy frontend parses long decimal literals with the VM bigint parser', 2;
}

# vim: expandtab shiftwidth=4
