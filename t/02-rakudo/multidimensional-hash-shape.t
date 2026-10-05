use Test;

plan 85;

use MONKEY-SEE-NO-EVAL;
use nqp;

my class NestedHash is Hash { }

is EVAL(q[my %h{Str;Int}; %h<a>{1} = 42; %h<a>{1}]), 42,
    'a hash of two dimensions stores a value under both keys';
is EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; %h<a>{1}]), 42,
    'a multidimensional subscript stores a value in a hash of two dimensions';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'a';2} = 2; %h{'b';1} = 3; (%h.elems, %h.keys.sort.List)]),
    (2, <a b>),
    'a hash of two dimensions counts and lists the keys of its first dimension';
ok EVAL(q[my %h{Str;Int}; %h.keyof]) === Str,
    'the keys of a hash of two dimensions are of the first type';
ok EVAL(q[my %h{Str;Int}; %h<a>{1} = 42; %h<a>.keyof]) === Int,
    'the keys of a value of a hash of two dimensions are of the second type';
ok EVAL(q[my %h{Str;Int}; %h<a>.keyof]) === Int,
    'a missing value of a hash of two dimensions is a hash keyed by the second type';
is EVAL(q[my %h{Str;Int}; %h<a>; %h<a>{1}:exists; %h.elems]), 0,
    'reading and testing a hash of two dimensions do not vivify it';

throws-like { EVAL q[my %h{Str;Int}; %h{1;1} = 42] }, X::TypeCheck::Binding::Parameter,
    expected => Str,
    'a hash of two dimensions checks the type of the first key';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';'b'} = 42] }, X::TypeCheck::Binding::Parameter,
    expected => Int,
    'a hash of two dimensions checks the type of the second key';
throws-like { EVAL q[my Int %h{Str;Int}; %h<a>{1} = 'x'] }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of two dimensions checks the value type';
ok EVAL(q[my %h{Str;Int}; %h<a>.WHAT]) =:= Hash[Any,Int],
    'a missing value of a hash of two dimensions is the nested hash type';

is EVAL(q[my Int %h{Str;Str} = a => { b => 42 }; %h<a><b>]), 42,
    'a hash of two dimensions initializes from nested hashes';
ok EVAL(q[my Int %h{Str;Str} = a => { b => 42 }; %h<a>.keyof]) === Str,
    'a nested hash a hash of two dimensions initializes from is keyed by the second type';
throws-like { EVAL q[my Int %h{Str;Str} = a => { b => 'x' }] }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of two dimensions checks the values of the nested hashes it initializes from';
throws-like { EVAL q[my %h{Str;Int}; %h<a> = { 1 => 42 }] }, X::TypeCheck::Binding::Parameter,
    expected => Int,
    'a hash of two dimensions checks the keys of a nested hash stored in it';
is EVAL(q[my Int %h{Str;Int} = a => :{ 1 => 42 }; %h<a>{1}]), 42,
    'a hash of two dimensions initializes from nested hashes keyed by the second type';
is EVAL(q[my %h{Str;Str}; %h<a> = (b => 1, c => 2); %h<a>.elems]), 2,
    'a value of a hash of two dimensions takes a list of pairs';
throws-like { EVAL q[my %h{Str;Str}; %h<a> = 42] }, X::Hash::Store::OddNumber,
    'a value of a hash of two dimensions refuses what a hash cannot store';

is-deeply EVAL(q[my %h{Str;Str}; (%h<a>.keys, %h<a>.values, %h<a>.kv, %h{'a';*}).map(*.elems)]),
    (0, 0, 0, 0),
    'a missing value of a hash of two dimensions has no keys or values';
ok EVAL(q[my %h{Str;Int;}; %h<a>.keyof]) === Int,
    'a trailing semicolon does not add a dimension';
ok EVAL(q[my %h{Str;Int}; %h<a>{1} = 42; %h<a>.of]) === Any,
    'the innermost values of an untyped hash of two dimensions are Any before 6.e';
nok EVAL(q[my Int %h{Str;Str}; try %h<a><b> = 'x'; %h<a>:exists]),
    'a value refused by an assignment does not vivify a nested hash';
ok EVAL(q[my %h{Str;Int;Rat}; %h<a>{1}.WHAT]) =:= Hash[Any,Rat],
    'a missing value of the second dimension of a hash of three dimensions is the nested hash type';
ok EVAL(q[my Int() %h{Str;Str}; %h<a><b>]) === Int,
    'a missing value of a hash of two dimensions of a coercive type is the target type';
ok EVAL(q[my Int:D %h{Str;Str}; %h<a><b>]) === Int,
    'a missing value of a hash of two dimensions of a definite type is the base type';
ok EVAL(q[my Int(Str) %h{Str;Str}; %h{'a';'b'} = 'abc'; my $v := %h<a><b>; my $failed = $v ~~ Failure; $v.so; $failed]),
    'a hash of two dimensions stores a value that fails to coerce as a Failure';
ok EVAL(q[my %h{Str;Mu}; %h<a>{Mu} = 1; %h<a>.keys.head]) === Mu,
    'a dimension keyed by Mu takes a Mu key';

ok EVAL(q[my %h{Str;Int;Rat}; %h<a>.keyof]) === Int,
    'the second dimension of a hash of three dimensions is keyed by the second type';
ok EVAL(q[my %h{Str;Int;Rat}; %h<a>{1}.keyof]) === Rat,
    'the third dimension of a hash of three dimensions is keyed by the third type';
throws-like { EVAL q[my %h{Str;Int;Rat}; %h{'a';1;'x'} = 42] }, X::TypeCheck::Binding::Parameter,
    expected => Rat,
    'a hash of three dimensions checks the type of the third key';
throws-like { EVAL q[my Int %h{Str;Str;Str} = a => { b => { c => 'x' } }] }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of three dimensions checks the values of the nested hashes it initializes from';
is-deeply EVAL(q[my Int(Str) %h{Str;Str}; %h<a><b> = '42'; %h<a><b>]), 42,
    'a hash of two dimensions coerces a value of a coercive type';
is EVAL(q[my Int %h{Str;Str()}; %h<a><b> = 42; %h<a>.^name]), 'Hash[Int]',
    'a dimension keyed by Str() is a hash of the value type';
throws-like { EVAL q[my Int %h{Str;Str()}; %h<a><b> = 'x'] }, X::TypeCheck::Assignment,
    expected => Int,
    'a dimension keyed by Str() checks the value type';

is EVAL(q[my %h{Str:D;Str:D;Str:D}; %h{'a';'b';'c'} = 42; %h<a><b><c>]), 42,
    'a hash of three dimensions stores a value under all three keys';
ok EVAL(q[my %h{Str:D;Str:D;Str:D}; %h{'a';'b';'c'} = 42; %h{'a';'b'}:exists]),
    'a hash of three dimensions has a key path shorter than its dimensions';
is-deeply EVAL(q[my %h{Str:D;Str:D;Str:D}; %h{'a';'b';'c'} = 42; %h<a><b>.keys]), ('c',).Seq,
    'a key path shorter than its dimensions gives a nested hash';
is EVAL(q[my %h{Str;Int;Str;Int}; %h{'a';1;'b';2} = 42; %h<a>{1}<b>{2}]), 42,
    'a hash of four dimensions stores a value under all four keys';

is EVAL(q[class { has %!h{Str:D;Str:D;Str:D}; method m { %!h{'a';'b';'c'} = 42; %!h<a><b><c> } }.new.m]), 42,
    'an attribute of three dimensions stores a value under all three keys';
is EVAL(q[class { has Int %.h{Str;Str} }.new(h => { a => { b => 42 } }).h<a><b>]), 42,
    'an attribute of two dimensions initializes from nested hashes';
ok EVAL(q[class { has Int %.h{Str;Str} }.new(h => { a => { b => 42 } }).h<a>.keyof]) === Str,
    'a nested hash an attribute of two dimensions initializes from is keyed by the second type';
is EVAL(q[class { has Int %.h{Str;Str} = a => { b => 42 } }.new.h<a><b>]), 42,
    'an attribute of two dimensions initializes from its default';
throws-like { EVAL q[class { has Int %.h{Str;Str} }.new(h => { a => { b => 'x' } })] },
    X::TypeCheck::Assignment,
    'an attribute of two dimensions checks the values of the nested hashes it initializes from';

ok EVAL(q[state %h{Str;Int}; %h<a>{1} = 42; %h<a>.keyof]) === Int,
    'a state hash of two dimensions nests a hash keyed by the second type';
ok EVAL(q[(my %{Str;Int}).keyof]) === Str,
    'an anonymous hash of two dimensions is keyed by the first type';
ok EVAL(q[(my %{Str;Int})<a>.keyof]) === Int,
    'an anonymous hash of two dimensions nests a hash keyed by the second type';

is EVAL(q[my Int %h{Str;Int}; %h<a>{1} = 42; my %g := EVAL %h.raku; %g<a>{1}]), 42,
    'the .raku of a hash of two dimensions evaluates to its contents';
ok EVAL(q[my Int %h{Str;Int}; %h<a>{1} = 42; my %g := EVAL %h.raku; %g.WHAT =:= %h.WHAT]),
    'the .raku of a hash of two dimensions evaluates to its type';

is EVAL(q[my Int %a{Str;Str} = a => { b => 42 }; my Int %b{Str;Str} := %a; %b<a><b>]), 42,
    'a hash of two dimensions binds to a declaration of the same shape';
throws-like { EVAL q[my Str %a{Str;Str}; my Int %b{Str;Str} := %a] }, X::TypeCheck::Binding,
    'a hash of two dimensions does not bind to a declaration of another value type';
throws-like { EVAL q[my %a{Str;Int}; my %b{Str;Str} := %a] }, X::TypeCheck::Binding,
    'a hash of two dimensions does not bind to a declaration of another inner key type';
throws-like { EVAL q[my %a{Str}; my %b{Str;Str} := %a] }, X::TypeCheck::Binding,
    'a hash of one dimension does not bind to a declaration of two';
ok EVAL(q[my %h{Str;Str}; my %i := Hash[Any,Str].new; %h<a> := %i; %h<a> =:= %i]),
    'a hash of the type of a dimension binds into that dimension as itself';
is EVAL(q[my Int %h{Str;Str}; my $v = 42; %h<a><b> := $v; $v = 43; %h<a><b>]), 43,
    'a value bound into a hash of two dimensions stays bound';
throws-like { EVAL q[my Int %h{Str;Str}; %h<a><b> := 'x'] }, X::TypeCheck::Binding::Parameter,
    expected => Int,
    'a hash of two dimensions checks the type of a value bound into it';
nok EVAL(q[my Int %h{Str;Str}; try %h<a><b> := 'x'; %h<a>:exists]),
    'a value refused by a bind does not vivify a nested hash';

is EVAL(q[my role ValueStore[::T] { has T %.h{Str;Str} }; my $r = ValueStore[Int].new; $r.h<a><b> = 42; $r.h<a><b>]), 42,
    'a hash of two dimensions stores a value of a type that a role takes';
throws-like { EVAL q[my role ValueTyped[::T] { has T %.h{Str;Str} }; ValueTyped[Int].new.h<a><b> = 'x'] },
    X::TypeCheck::Assignment, expected => Int,
    'a hash of two dimensions checks a value type that a role takes';
ok EVAL(q[my role DefiniteTyped[::T] { has T:D %.h{Str;Str} }; DefiniteTyped[Int].new.h<a><b>]) === Int,
    'a missing value of a hash of two dimensions of a definite type that a role takes is the base type';
ok EVAL(q[my role DefiniteCoerciveKeyTyped[::T] { has T:D %.h{Str;Str()} }; DefiniteCoerciveKeyTyped[Int].new.h<a><b>]) === Int,
    'a missing value of a dimension keyed by Str() of a definite type that a role takes is the base type';
ok EVAL(q[my role BindsDeclared[::T] { has T:D %.h{Str;Str()} }; my Int:D %x{Str;Str()} := BindsDeclared[Int].new.h; %x<a><b> = 1; %x<a>.WHAT =:= (my Int:D %{Str;Str()})<a>.WHAT]),
    'a hash of two dimensions of a type that a role takes binds to a declaration of the instantiated type';
ok EVAL(q[my role KeyTyped[::T] { has %.h{Str;T} }; KeyTyped[Int].new.h<a>.keyof]) === Int,
    'a hash of two dimensions is keyed by a type that a role takes';
throws-like { EVAL q[my role DeepValueTyped[::T] { has T %.h{Str;Str;Str} }; DeepValueTyped[Int].new.h<a><b><c> = 'x'] },
    X::TypeCheck::Assignment, expected => Int,
    'a hash of three dimensions checks a value type that a role takes';
ok EVAL(q[my role MiddleKeyTyped[::T] { has %.h{Str;T;Str} }; MiddleKeyTyped[Int].new.h<a>.keyof]) === Int,
    'a hash of three dimensions is keyed in its second dimension by a type that a role takes';

is EVAL(q[my Int %h{Str;Str} where * > 0; %h<a><b> = 42; %h<a><b>]), 42,
    'a hash of two dimensions stores a value that meets its where constraint';
throws-like { EVAL q[my Int %h{Str;Str} where * > 0; %h<a><b> = -1] }, X::TypeCheck::Assignment,
    'a hash of two dimensions checks the where constraint of its values';
ok EVAL(q[my %h{Str;Str} of Int; %h<a><b> = 42; %h<a>.of]) === Int,
    'the innermost values of a hash of two dimensions are of the type of an of trait';
throws-like { EVAL q[my %h{Str;Str} of Int; %h<a><b> = 'x'] }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of two dimensions checks the value type of an of trait';

ok EVAL(q[my %h{Str;Int} is NestedHash; %h]) ~~ NestedHash,
    'a hash of two dimensions is of the type of an is trait';
ok EVAL(q[my %h{Str;Int} is NestedHash; %h<a>{1} = 42; %h<a>.keyof]) === Int,
    'a hash of two dimensions with an is trait nests a hash keyed by the second type';
ok EVAL(q[my %h{Str;Int} is NestedHash; %h<a>{1} = 42; %h<a>]) ~~ NestedHash,
    'a hash of two dimensions with an is trait nests a hash of that type';
is EVAL(q[my %h{Str;Str()} is NestedHash; %h<a><b> = 42; %h<a><b>]), 42,
    'a hash of two dimensions with an is trait stores a value in a dimension keyed by Str()';
throws-like { EVAL q[my Int %h{Str;Str} is NestedHash; %h<a><b> = 'x'] }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of two dimensions with an is trait checks the value type';
ok EVAL(q[my %h{Str;Str} is NestedHash; %h<a> = { b => 1 }; %h<a> ~~ NestedHash && %h<a>.keyof === Str]),
    'a hash of two dimensions with an is trait coerces an assigned hash to that type';

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[sub f(::T $x) { my T %h{Str;Str}; %h<a><b> = $x; %h<a><b> }; f(42)]), 42,
        'a hash of two dimensions in a routine stores a value of a type capture';
    throws-like { EVAL q[sub f(::T $x) { my T %h{Str;Str}; %h<a><b> = 'x' }; f(42)] },
        X::TypeCheck::Assignment, expected => Int,
        'a hash of two dimensions in a routine checks a value type capture';
    ok EVAL(q[sub f(::T $x) { my %h{Str;T}; %h<a>{$x} = 1; %h<a>.keyof }; f(42)]) === Int,
        'a hash of two dimensions in a routine is keyed by a type capture';
}
else {
    skip 'the legacy frontend leaves a generic hash in a routine uninstantiated', 3;
}

is EVAL(q[my Int %h{Str;Str;Str}; my $v = 1; %h<a><b><c> := $v; $v = 2; %h<a><b><c>]), 2,
    'a value bound into a hash of three dimensions stays bound';
throws-like { EVAL q[my Int %h{Str;Str}; sub f { fail 'lookup failed' }; %h<a> = f()] }, Exception,
    message => 'lookup failed',
    'a Failure assigned to a dimension of a hash of two dimensions throws itself';
throws-like { EVAL q[my Int %h{Str;Str}; my %inner := %h<a>; %inner<b> = 1] }, X::Assignment::RO,
    'assigning into a missing dimension bound outside a container refuses to modify it';
ok EVAL(q[my Int %h{Int(Str);Str}; %h{'1'}<a> = 42; %h.keys.head]) === 1,
    'a hash of two dimensions coerces its first key';
is-deeply EVAL(q[my Int %h{Int(Str);Str}; try %h<x><a> = 42; ($!.^name, %h.elems)]),
    ('X::Str::Numeric', 0),
    'a first key that fails to coerce throws and stores nothing';

throws-like { EVAL q[my Int %h{Str;Str} is default(42)] }, X::Comp::NYI,
    feature => 'is default on a multidimensional shaped hash',
    'is default on a hash of two dimensions is not yet implemented';
throws-like { EVAL q[class { has Int %.h{Str;Str} is default(42) }] }, X::Comp::NYI,
    feature => 'is default on a multidimensional shaped hash',
    'is default on an attribute of two dimensions is not yet implemented';
throws-like { EVAL q[my int %h{Str;Str}] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash of two dimensions refuses a native value type';

# vim: expandtab shiftwidth=4
