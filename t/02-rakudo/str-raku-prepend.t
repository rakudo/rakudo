use Test;
use MONKEY-SEE-NO-EVAL;

plan 9;

# A character that joins the grapheme next to it would take the quote on
# that side of a string literal into its own grapheme, so .raku escapes it.

is "\x[600]".raku, '"\x[600]"',
  'a prepend character on its own is escaped';

is "\x[600]a".raku, '"\x[600,61]"',
  'a prepend character with its base is escaped as one grapheme';

is "\x[300]".raku, '"\x[300]"',
  'a combining character on its own is escaped';

is "\x[1103F]".raku, '"\x[1103F]"',
  'an extending character without a combining class is escaped';

is "\x[903]".raku, '"\x[903]"',
  'a spacing mark on its own is escaped';

is "\x[200D]".raku, '"\x[200D]"',
  'a zero width joiner on its own is escaped';

is "a\x[D4E]".raku, '"a\x[D4E]"',
  'a prepend letter at the end of a word is escaped';

is "a\x[300]".raku, "\"a\x[300]\"",
  'a combining character on a base stays in its grapheme';

is EVAL("\x[600]\x[1103F]".raku), "\x[600]\x[1103F]",
  'the escaped form evaluates back to the string';

# vim: expandtab shiftwidth=4
