use nqp;
use Test;

plan 26;

# A regex takes the implicit slurpy hash every method takes. A body
# that never names %_ builds no hash for it, while the parameter stays
# in the signature, so a subrule call may still pass a named argument
# the regex declares no parameter for.

grammar Plain {
    token TOP { <word>+ % ',' }
    token word { \w+ }
}

my $plain-word := Plain.^lookup('word');
ok $plain-word.signature.params.first(*.name eq '%_'),
  'a token without a signature still declares the slurpy hash';
is $plain-word.arity, 1, 'a token without a signature still takes only the invocant';
is $plain-word.count, 1, 'a token without a signature still counts one argument';

ok Plain.parse('a,bb,ccc'), 'a grammar of tokens without signatures parses';
is Plain.parse('a,bb,ccc')<word>>>.Str, <a bb ccc>,
  'the captures of such a grammar come out whole';

grammar NamedArgument {
    token TOP { <word(:ignored)> }
    token word { \w+ }
}

ok NamedArgument.parse('abc'),
  'a named argument to a subrule that declares no parameter for it is accepted';
is ~NamedArgument.parse('abc')<word>, 'abc',
  'the subrule taking that named argument still captures';

grammar Counted {
    token TOP { <repeated(2)> }
    token repeated(Int $n) { \w ** { $n } }
}

is ~Counted.parse('ab')<repeated>, 'ab',
  'a regex with a signature binds its positional parameter';
ok Counted.^lookup('repeated').signature.params.first(*.name eq '%_'),
  'a regex with a signature also declares the slurpy hash';

grammar Named {
    token TOP { <letter(:upper)> }
    token lowercase { <letter(:!upper)> }
    token letter(:$upper) {
        [ <?{ $upper }> <[A..Z]> ] | [ <!{ $upper }> <[a..z]> ]
    }
}

ok Named.parse('Q'), 'a regex with a named parameter binds it from a subrule call';
nok Named.parse('q'), 'and the bound value is the one the subrule call passed';
ok Named.parse('q', :rule<lowercase>),
  'a subrule call passing the negated named argument binds that value instead';

sub interpolating() {
    my $pattern = '\d+';
    'x4711' ~~ / \w <( <$pattern> /
}

is ~interpolating(), '4711',
  'a regex interpolates a lexical of the sub around it that nothing outside the regex names';

# A body that reaches its own frame by name can find %_ without naming
# it outright, and a caller can pass a named argument no parameter of
# the regex takes. The hash has to be there for both.

my $reached;
grammar Reaching {
    token TOP { <named(:passed)> }
    token named { <?{ $reached = OUTER::{Q[%_]}; 1 }> \w+ }
}

ok Reaching.parse('abc'),
  'a regex whose body reaches its own frame by name parses';
is-deeply $reached, {:passed}.Hash,
  'and its slurpy hash holds what the subrule call passed';

grammar Stray {
    token TOP { <letter(:upper, :bogus)> }
    token letter(:$upper) { <?{ $upper }> <[A..Z]> }
}

ok Stray.parse('Q'),
  'a named argument the regex declares no parameter for is accepted beside one it does';

my $word := Plain.^lookup('word');
is $word.cando(\(Plain, :stray)).elems, 1,
  'a regex takes a capture carrying a named argument it does not declare';
ok $word.signature.ACCEPTS(\(Plain, :stray)),
  'and its signature accepts that capture';

class Replacing {
    method substitute($text) { my $copy = $text; $copy ~~ s/a/b/; ($copy, %_) }
    method transliterate($text) { my $copy = $text; $copy ~~ tr/a/z/; ($copy, %_) }
}

my ($substituted, $subst-slurpy) = Replacing.substitute('aaa', :adverb);
is $substituted, 'baa', 'a method holding a substitution still substitutes';
is-deeply $subst-slurpy, {:adverb}.Hash,
  'and the method around it still reaches its own slurpy hash';

my ($transliterated, $tr-slurpy) = Replacing.transliterate('abc', :adverb);
is $transliterated, 'zbc', 'a method holding a transliteration still transliterates';
is-deeply $tr-slurpy, {:adverb}.Hash,
  'and the method around a transliteration reaches its own too';

# Naming %_ in a body is what makes the hash exist, so a body that
# never names one cannot look at its own. That one reads its frame back
# through callframe, from a sub it calls.

sub slurpy-of(Str $frame-name) {
    my int $n = 0;
    loop {
        my $frame = (try callframe($n++)) // return Nil;
        return $frame.my{Q[%_]} if (try $frame.code.name) eq $frame-name;
    }
}

my ($unnamed, $named);
grammar Unnaming {
    token TOP { <plain(:passed)> <naming(:passed)> }
    token plain { <?{ $unnamed = slurpy-of('plain'); 1 }> \w }
    token naming { <?{ $named = %_; 1 }> \w }
}

ok Unnaming.parse('ab'), 'a regex whose body names no slurpy hash parses';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is $unnamed.^name, 'Mu',
      'and its frame holds no hash for the named argument to have landed in';
}
else {
    skip 'the legacy frontend sets up every regex slurpy hash', 1;
}

is-deeply $named, {:passed}.Hash,
  'a regex whose body does name one gets it set up and filled';
ok Unnaming.^lookup('plain').signature.params.first(*.name eq '%_'),
  'and the regex that went without still declares the parameter';
