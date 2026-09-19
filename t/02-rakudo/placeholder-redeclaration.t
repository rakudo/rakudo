use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 16;

# A `%_` in a routine body gives the routine a slurpy hash, so a `my %_`
# there names that parameter rather than a slot of its own.
is EVAL(q|no worries; sub f { my %_; %_<a> }; f(a => 5)|), 5,
    'a `my %_` in a sub names the named arguments the sub was called with';

# The declaration binds no slot of its own, but its initializer still runs.
is EVAL(q|no worries; sub f { my %_ = a => 1; %_<a> }; f()|), 1,
    'an initializer on a `my %_` in a sub stores into the slurpy hash';

# A `@_` gives the routine a slurpy array the same way.
is EVAL(q|no worries; sub f { my @_; @_[0] }; f(7)|), 7,
    'a `my @_` in a sub names the positional arguments the sub was called with';

is EVAL(q|no worries; sub f { my @_ = 1,2; @_[1] }; f()|), 2,
    'an initializer on a `my @_` in a sub stores into the slurpy array';

# A named placeholder pairs with a declaration of its name the same way.
is EVAL(q|no worries; sub f { $:a; my $a; $a }; f(:a(7))|), 7,
    'a `my $a` after a `$:a` names the placeholder parameter';

# A method always has a slurpy hash, whether or not the body mentions one.
is EVAL(q|no worries; class C { method m { my %_; %_<a> } }; C.m(a => 5)|), 5,
    'a `my %_` in a method names the named arguments the method was called with';

# A declaration that follows a placeholder names the parameter the
# placeholder asked for.
is EVAL(q|no worries; sub f { $^a; my $a; $a }; f(7)|), 7,
    'a `my $a` after a `$^a` names the placeholder parameter';

is-run 'sub f { my %_ }; print "ok"',
    :out('ok'),
    :err{ 1 == .comb(q{Redeclaration of symbol '%_'}).elems },
    'a `my %_` in a sub warns of the redeclaration, once';

is-run 'sub f { $^a; my $a }; print "ok"',
    :out('ok'),
    :err{ 1 == .comb(q{Redeclaration of symbol '$a'}).elems },
    'a `my $a` after a `$^a` warns of the redeclaration, once';

# A `my @_` in a method asks for a slurpy array placeholder, and a method
# takes no placeholder parameter other than the `%_` it always has. The two
# frontends word the refusal differently.
is-run 'class C { method m { my @_ } }; print "ok"',
    :out(''),
    :err{.contains: '@_'},
    :exitcode(1),
    'a `my @_` in a method is a compile error';

is-run 'use fatal; sub f { my %_ }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '%_'}},
    :exitcode(1),
    'a `use fatal` makes the slurpy redeclaration an error';

is-run 'no worries; sub f { my %_ }; print "ok"',
    :out('ok'),
    :err(''),
    'a `no worries` silences the slurpy redeclaration worry';

is-run 'no worries; sub f { $^a; my $a }; print "ok"',
    :out('ok'),
    :err(''),
    'a `no worries` silences the placeholder redeclaration worry';

# A `no worries` silences worries, and the report a placeholder after a
# declaration of its name earns is not one.
is-run 'no worries; sub f { my $a; $^a }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$^a' as a placeholder parameter}},
    :exitcode(1),
    'a `no worries` leaves the placeholder compile error standing';

# A placeholder cannot take over a name the body has already declared: it
# asks for a parameter of a name that is spoken for. The error names the
# placeholder as it is written.
is-run 'sub f { my $a; $^a }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$^a' as a placeholder parameter}},
    :exitcode(1),
    'a `$^a` after a `my $a` is a compile error naming the placeholder';

is-run 'sub f { my $a; $:a }; print "ok"',
    :out(''),
    :err{.contains: q{Redeclaration of symbol '$:a' as a placeholder parameter}},
    :exitcode(1),
    'a `$:a` after a `my $a` is a compile error naming the placeholder';

# vim: expandtab shiftwidth=4
