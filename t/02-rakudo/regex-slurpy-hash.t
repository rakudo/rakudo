use Test;

plan 15;

# A regex declaration is a method, and a method's body reaches the
# implicit slurpy hash whether or not the method declares a signature of
# its own. A sub is the other case: there %_ is a placeholder that
# builds the signature, so a sub that already has one refuses it.

my $seen;

grammar Tokens {
    token TOP { <part(:adverb)> }
    token part { <?{ $seen = %_; 1 }> \w+ }
}

ok Tokens.parse('abc'), 'a token whose body names the slurpy hash parses';
is-deeply $seen, {:adverb}.Hash, 'and it holds what the subrule call passed';

grammar Rules {
    rule TOP { <part(:adverb)> }
    rule part { <?{ $seen = %_; 1 }> \w+ }
}

$seen = Nil;
ok Rules.parse('abc'), 'a rule whose body names the slurpy hash parses';
is-deeply $seen, {:adverb}.Hash, 'and it holds what that call passed';

grammar Regexes {
    regex TOP { <part(:adverb)> }
    regex part { <?{ $seen = %_; 1 }> \w+ }
}

$seen = Nil;
ok Regexes.parse('abc'), 'a regex whose body names the slurpy hash parses';
is-deeply $seen, {:adverb}.Hash, 'and it holds what that call passed';

grammar Declaring {
    token TOP { <part(:declared, :stray)> }
    token part(:$declared) { <?{ $seen = %_; $declared }> \w+ }
}

$seen = Nil;
ok Declaring.parse('abc'),
  'a regex that declares a signature reaches the slurpy hash as well';
is-deeply $seen, {:stray}.Hash,
  'and the hash holds the named argument no parameter of its own took';

# The first %_ in a body takes the placeholder name, so a body that
# spells %_ out somewhere it does not read, a string among the places,
# reaches the hash through an ordinary lookup instead.

grammar Spelled {
    token TOP { <part(:adverb)> }
    token part { <?{ my $doc = "names %_"; $seen = %_; 1 }> \w+ }
}

$seen = Nil;
ok Spelled.parse('abc'),
  'a regex body reaching the hash by lookup rather than by placeholder parses';
is-deeply $seen, {:adverb}.Hash, 'and the lookup finds the hash filled';

class Spelling {
    method taking { my $doc = "names %_"; %_ }
}

is-deeply Spelling.taking(:adverb), {:adverb}.Hash,
  'a method body reaches its hash by lookup the same way';

class Declared {
    method taking(:$declared) { %_ }
}

is-deeply Declared.taking(:declared, :stray), {:stray}.Hash,
  'a method declaring a signature reaches its slurpy hash the same way';

throws-like 'sub taking(:$declared) { %_ }', X::Signature::Placeholder,
  'a sub that declares a signature still refuses the placeholder';

throws-like 'sub outside { "a" ~~ / <?{ %_ }> \w / }', X::Placeholder::Block,
  'a regex code block with no method around it to hold a slurpy hash refuses one';

throws-like 'grammar Refusing { token TOP { <?{ @_ }> \w } }',
  X::Placeholder::Block,
  'a regex body still refuses a placeholder that is not the slurpy hash';
