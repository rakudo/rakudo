use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 26;

# A block takes its topic as a parameter, so a `my $_` in the body names that
# parameter rather than a slot of its own.
is EVAL(q|no worries; my $c = { my $_; $_ }; $c(42)|), 42,
    'a `my $_` in a block names the argument the block was called with';

# Left unbound, the topic parameter still defaults from the outer topic.
is EVAL(q|no worries; $_ = 5; my $c = { my $_; $_ }; $c()|), 5,
    'a `my $_` in a block called without an argument names the outer topic';

# The declaration binds no slot of its own, so an initializer assigns to the
# topic, which a topic bound to an immutable argument refuses.
is EVAL(q|no worries; my $c = { my $_ = 7; $_ }; $c()|), 7,
    'an initializer on a `my $_` in a block stores into the topic';

is-run 'my $c = { my $_ = 7; $_ }; $c(42)',
    :err{.contains: 'Cannot assign to an immutable value'},
    :exitcode(1),
    'an initializer on a `my $_` in a block refuses an immutable topic';

# A routine declares its topic, match and error variables the same way.
is EVAL(q|no worries; sub f { my $_ = 3; $_ }; f()|), 3,
    'a `my $_` in a sub stores into the topic the sub declares';

is EVAL(q|no worries; sub f { my $/; "abc" ~~ /b/; ~$/ }; f()|), 'b',
    'a `my $/` in a sub names the match variable a match writes';

is EVAL(q|no worries; sub f { my $!; try die "boom"; ~$! }; f()|), 'boom',
    'a `my $!` in a sub names the error variable a failed try writes';

is EVAL(q|no worries; class C { method m { my $_ = 4; $_ } }; C.m|), 4,
    'a `my $_` in a method stores into the topic the method declares';

# An `our $_` binds the block's topic to the package symbol it installs, so
# the block reads the package symbol rather than its argument.
is EVAL(q|no worries; my $c = { our $_ = 9; "$_/$GLOBAL::_" }; $c(3)|), '9/9',
    'an `our $_` in a block names the package symbol it installs';

# The topic the declaration names is the caller's own container.
is EVAL(q|no worries; $_ = 1; { my $_ = 2 }; $_|), 2,
    'an initializer on a `my $_` in a block stores into the outer topic';

# A block inside a routine takes its own topic, which defaults from the
# routine's.
is EVAL(q|no worries; sub f { my $_ = 3; my $i = { my $_; $_ }; $i(9) }; f()|), 9,
    'a `my $_` in a block inside a sub names the topic the block was given';

is EVAL(q|no worries; multi m(Int) { my $_ = 3; $_ }; m(1)|), 3,
    'a `my $_` in a multi candidate stores into the topic it declares';

is EVAL(q|no worries; my $c = { my $_ := 4; $_ }; $c(1)|), 4,
    'a binding `my $_ :=` in a block names the topic it binds';

# A `given` body takes its topic the same way.
is EVAL(q|no worries; given 3 { my $_; $_ }|), 3,
    'a `my $_` in a given body names the topic given';

# So does each run of a `for` body.
is EVAL(q|no worries; my @seen; for 1,2 { my $_; @seen.push($_) }; @seen.join(",")|), '1,2',
    'a `my $_` in a for body names the value of each iteration';

# A CATCH block takes the exception as its topic.
is EVAL(q|no worries; my $m; { CATCH { my $_; $m = .message; .resume }; die "boom" }; $m|), 'boom',
    'a `my $_` in a CATCH block names the exception';

is-run 'print BEGIN { my $_ = 7; $_ }',
    :out('7'),
    :err{.contains: q{Redeclaration of symbol '$_'}},
    'a `my $_` in a BEGIN block stores into the topic, and warns';

is-run '{ my $_ }; print "ok"',
    :out('ok'),
    :err{.contains: q{Redeclaration of symbol '$_'}},
    'a `my $_` in a block warns of the redeclaration';

is-run 'no worries; { my $_ }; print "ok"',
    :out('ok'),
    :err(''),
    'a `no worries` silences the redeclaration worry';

is-run '{ no worries; my $_ }; print "ok"',
    :out('ok'),
    :err(''),
    'a `no worries` inside the block silences the redeclaration worry';

is-run 'use fatal; { my $_ }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$_'}},
    :exitcode(1),
    'a `use fatal` makes the redeclaration an error';

# A parameter list declares the lexicals of a signature it binds, which
# cannot be ones the block already has.
is-run 'my $c = { my ($_, $x) = 1,2 }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$_'}},
    :exitcode(1),
    'a `my ($_, ...)` in a block is a compile error';

# A constant asks for no container, so it cannot name the topic either.
is-run 'my $c = { constant $_ = 3; $_ }; print $c(1)',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$_'}},
    :exitcode(1),
    'a `constant $_` in a block is a compile error';

# A placeholder asks for a parameter of a name the declaration has taken.
is-run 'my $c = { my $_; $^_ }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$^_' as a placeholder parameter}},
    :exitcode(1),
    'a `$^_` after a `my $_` in a block is a compile error';

# The parser has the first declaration in scope by the time it reads the
# second, so each of the two is reported once.
is-run '{ my $_; my $_ }; print "ok"',
    :out('ok'),
    :err{ 2 == .comb(q{Redeclaration of symbol '$_'}).elems },
    'a second `my $_` in a block is reported once more';

is-run '{ my $x }; print "ok"',
    :out('ok'),
    :err(''),
    'a block declaring a name of its own warns nothing';

# vim: expandtab shiftwidth=4
