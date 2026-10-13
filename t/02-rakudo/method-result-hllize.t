use Test;
use nqp;

plan 15;

my $c = q[use nqp; my class C { method m { nqp::list(1, 2) }; method n { 'str' } }; ];

is EVAL($c ~ q[(C . m).^name]), 'List',
    'the dotty infix maps a VM array result to a List';

is EVAL($c ~ q[my $x = C; $x .= m; $x.^name]), 'List',
    '.= on a variable maps a VM array result to a List';

is EVAL($c ~ q[my @a = C; @a[0] .= m; @a[0].^name]), 'List',
    '.= on an element maps a VM array result to a List';

is EVAL($c ~ q[my $x = C; given $x { .=m }; $x.^name]), 'List',
    '.= on the topic maps a VM array result to a List';

is EVAL($c ~ q[my $x is default(C) .= m; $x.^name]), 'List',
    'a .= initializer of a variable maps a VM array result to a List';

is EVAL(q[use nqp; use MONKEY-TYPING; augment class List { method from-vm-list { nqp::list(1, 2) } }; my List constant x .= from-vm-list; x.^name]),
    'List',
    'a .= initializer of a constant maps a VM array result to a List';

throws-like $c ~ q[my C constant x .= n], X::TypeCheck,
    'a .= initializer of a constant is type checked';

is EVAL(q[my Int constant x .= new(do { 5 }); x]), 5,
    'a .= initializer of a constant may take arguments that are compiled';

is EVAL(q[my sub f($) { 41 }; my Int constant x .= &f; x]), 41,
    'a .= initializer of a constant may call a sub as a method';

is EVAL(q[my Array[Numeric] constant a .= new(1, 2); a.raku]), 'Array[Numeric].new(1, 2)',
    'a .= initializer of a constant with a parameterized type passes its type check';

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[use lib <t/02-rakudo/test-packages>; use ConstantCallAssign; ConstantCallAssign::closure-argument.v.(1)]), 2,
        'a .= initializer of a constant in a precompiled module keeps a closure its compiled arguments make';
}
else {
    skip 'a closure made by the arguments of a .= initializer of a precompiled constant dies on the legacy frontend';
}

is EVAL(q[my class F { has $.a .= new }; F.new.a.raku]), 'Any.new',
    'a .= initializer of an attribute without a type calls the method on Any';

is EVAL(q[my class F { has $!a .= new; method a { $!a } }; F.new.a.raku]), 'Any.new',
    'a .= initializer of a private attribute without a type calls the method on Any';

is EVAL(q[my role R { has $.a .= new }; my class F does R { }; F.new.a.raku]), 'Any.new',
    'a .= initializer of a role attribute without a type calls the method on Any';

is EVAL(q[my class F { has Int:D $.a .= new(5) }; F.new.a]), 5,
    'a .= initializer of an attribute with a definite type calls the method on the base type';

# vim: expandtab shiftwidth=4
