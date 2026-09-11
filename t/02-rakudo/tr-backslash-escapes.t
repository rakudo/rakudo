use Test;
use nqp;

plan 24;

# A transliteration takes literal characters and ranges, not the character
# classes a regex takes.  A class names no single character, so it is rejected
# the same way an unknown backslash sequence is rejected in a string.

use MONKEY-SEE-NO-EVAL;

is EVAL(Q{my $s = 'abc'; $s ~~ tr/a..c/x..z/; $s}), 'xyz',
  'a transliteration range maps each character in its order';

is EVAL(Q{my $s = "a\tb"; $s ~~ tr/\t/./; $s}), 'a.b',
  'a tab escape transliterates the tab it names';

is EVAL(Q{my $s = "a\tb\nc"; $s ~~ tr/\t..\n/XY/; $s}), "aXbYc",
  'a range between two escapes maps every character between them';

is EVAL(Q{my $s = 'ABCD'; $s ~~ tr/\x41..\x43/x..z/; $s}), 'xyzD',
  'a range between two hex escapes maps every character between them';

throws-like Q{my $s = 'a'; $s ~~ tr/a-c/x-z/}, X::Obsolete,
  message => /'Unsupported use of - as character range'/,
  'a hyphen range in a transliteration names the Raku spelling instead';

throws-like Q{my $s = 'a'; $s ~~ tr/../x/}, X::Comp::AdHoc,
  message => /'Range missing start character on the left'/,
  'a transliteration range with no left character is rejected';

throws-like Q{my $s = 'a'; $s ~~ tr/\t../x/}, X::Comp::AdHoc,
  message => /'Range missing stop character on the right'/,
  'a transliteration range with no right character is rejected';

# Legacy parses these escapes to nothing, so they vanish from the character
# list and every character after one pairs with the wrong replacement.  An
# unrecognized letter panics there without naming itself.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    for <d s w h v> -> $class {
        throws-like Q{my $s = 'a'; $s ~~ tr/\} ~ $class ~ Q{/./},
          X::Backslash::UnrecognizedSequence, sequence => $class,
          "the \\$class character class in a transliteration is rejected and named";
    }

    for <D S W H V> -> $class {
        throws-like Q{my $s = 'a'; $s ~~ tr/\} ~ $class ~ Q{/./},
          X::Backslash::UnrecognizedSequence, sequence => $class,
          "the negated \\$class character class in a transliteration is rejected and named";
    }

    throws-like Q{my $s = 'a'; $s ~~ tr/\z/./},
      X::Backslash::UnrecognizedSequence, sequence => 'z',
      'a backslash before any other letter is rejected and named';

    throws-like Q{my $s = 'a'; $s ~~ tr/\9/./},
      X::Backslash::UnrecognizedSequence, message => /'$8'/,
      'a backslash before a digit still suggests the capture variable';

    throws-like Q{my $s = 'ab'; $s ~~ tr/ab/x\d/},
      X::Backslash::UnrecognizedSequence, sequence => 'd',
      'a character class in the replacement list is rejected and named';

    throws-like Q{my $s = 'xd'; $s ~~ tr/x\d/ab/},
      X::Backslash::UnrecognizedSequence, sequence => 'd',
      'a character class after a literal character is rejected and named';

    throws-like Q{my $s = 'a'; my $r = TR/\d/./ given $s},
      X::Backslash::UnrecognizedSequence, sequence => 'd',
      'a character class in the returning transliteration form is rejected';

    throws-like Q{my $s = 'a'; $s ~~ tr:d/\d//},
      X::Backslash::UnrecognizedSequence, sequence => 'd',
      'a character class under the delete adverb is rejected';

    # An escaped non-word character has to keep its value.  Losing it drops
    # the character from the list, which shifts every pair after it.
    is EVAL(Q{my $s = 'a-b.c d'; $s ~~ tr/\-\.\ /XYZ/; $s}), 'aXbYcZd',
      'an escaped hyphen, dot and space each transliterate as themselves';
}
else {
    skip 'the legacy frontend parses these escapes differently', 17;
}

# vim: expandtab shiftwidth=4
