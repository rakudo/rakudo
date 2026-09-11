use Test;

plan 31;

# The quote parser splits a quoted construct into literal pieces, nested
# quotes and escapes.  The cases below pin the boundaries between those
# pieces.  They go through EVAL so a parse failure is reported at the case
# that hit it rather than as a file that does not compile.

use MONKEY-SEE-NO-EVAL;

is EVAL(Q{"\ta"}), "\ta",
  'an escape at the start of a string keeps the text that follows it';

is EVAL(Q{"a\t"}), "a\t",
  'an escape at the end of a string keeps the text before it';

is EVAL(Q{"a\tb"}), "a\tb",
  'an escape between two pieces of text keeps both';

is EVAL(Q{"\t"}), "\t",
  'a string that is nothing but an escape yields just that escape';

is EVAL(Q{"\t\n"}), "\t\n",
  'two adjacent escapes yield no empty piece between them';

is EVAL(Q{""}), '',
  'an empty string yields the empty string';

is EVAL(Q{q{a{b}c}}), 'a{b}c',
  'a nested delimiter pair inside a single quoted string stays literal';

is EVAL(Q{Q«a«b»c»}), 'a«b»c',
  'a nested angle delimiter pair inside Q stays literal';

is EVAL(Q{q{a{}b}}), 'a{}b',
  'an empty nested delimiter pair yields both of its delimiters';

is EVAL(Q{q{a{b}}}), 'a{b}',
  'a nested pair at the end of a quote closes before the quote does';

is EVAL(Q{q{{a}{b}}}), 'a}{b',
  'a doubled opening brace is the delimiter, not a nested pair';

is EVAL(Q{Q«a«b»«c»d»}), 'a«b»«c»d',
  'two adjacent nested pairs keep the text on both sides of each';

is EVAL(Q{q{a\{b\}c}}), 'a{b}c',
  'a backslash before a nesting delimiter keeps it from nesting';

throws-like Q[q{a{b}], Exception,
  message => /"Couldn't find terminator }"/,
  'a nested pair with no outer terminator names the missing terminator';

is EVAL(Q{"a{ "b{ "c\td" }e" }f"}), "abc\tdef",
  'three levels of interpolated string nest without losing a piece';

is EVAL(Q{q{x\qq[{1+1}]y}}), 'x2y',
  'a qq escape re-enters the quote parser inside a single quoted string';

is-deeply EVAL(Q{qqw{a {1+1} c}}), ('a', '{1+1}', 'c'),
  'a brace delimiter makes an inner brace a nesting rather than a closure';

is EVAL(Q{"\x41\o101\c[LATIN SMALL LETTER A]"}), 'AAa',
  'three numeric and named escapes in a row each yield one character';

is EVAL(Q{qq:to/END/
a\tb
END}), "a\tb\n",
  'an escape inside a heredoc body is honoured';

is EVAL(Q{my $s = 'abc'; $s ~~ tr/a..c/x..z/; $s}), 'xyz',
  'a transliteration range maps each character in its order';

throws-like Q{my $s = 'a'; $s ~~ tr/a-c/x-z/}, X::Obsolete,
  message => /'Unsupported use of - as character range'/,
  'a hyphen range in a transliteration names the Raku spelling';

throws-like Q{my $s = 'a'; $s ~~ tr/../x/}, Exception,
  message => /'Range missing start character on the left'/,
  'a transliteration range with no left character is rejected';

is EVAL(Q{"{1+1}"}), '2',
  'a string that is nothing but an interpolation yields just that interpolation';

is EVAL(Q{"{1}{2}"}), '12',
  'two adjacent interpolations yield no empty piece between them';

is EVAL(Q{"{1+1}\t"}), "2\t",
  'an escape directly after an interpolation leaves no piece between them';

is EVAL(Q{"\t{1+1}"}), "\t2",
  'an interpolation directly after an escape leaves no piece between them';

is EVAL(Q{my $a = 'X'; my $b = 'Y'; "p$a\tq$b r"}), "pX\tqY r",
  'a variable interpolation between literal pieces keeps both of them';

is EVAL(Q{my @a = 1, 2; "<@a[1]>"}), '<2>',
  'an indexed array interpolation is one escape, not the text around it';

is EVAL(Q{my $a = 'xy'; "$a.uc()"}), 'XY',
  'a method call interpolation consumes the call and nothing after it';

# The quote records where it started so a runaway one can name that line.
throws-like qq{say "a\nb" 1;}, X::Syntax::Confused,
  message => /'runaway multi-line "" quote starting at line 1'/,
  'a confused parse after a multi line quote names the line the quote opened on';

throws-like qq{say <<a\nb>> 1;}, X::Syntax::Confused,
  message => /'runaway multi-line <<>> quote starting at line 1'/,
  'the runaway hint spells a double angle quote with both of its brackets';

# vim: expandtab shiftwidth=4
