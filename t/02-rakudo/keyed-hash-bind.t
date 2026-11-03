use Test;

plan 17;

use MONKEY-SEE-NO-EVAL;

ok Hash[Int,Str] ~~ Associative[Int,Str],
    'an object hash is Associative of its value and key types';
nok Hash[Int,Str] ~~ Associative[Int,Int],
    'an object hash is not Associative of another key type';
ok Hash[Int,Str] ~~ Associative[Int],
    'an object hash is Associative of its value type';

{
    my Int %a{Str} = a => 1;
    my Int %b{Str} := %a;
    ok %b =:= %a,
        'a keyed hash binds to a declaration of the same value and key types';
}

throws-like { EVAL 'my Int %a{Str}; my Int %b{Int} := %a' }, X::TypeCheck::Binding,
    'a keyed hash does not bind to a declaration of another key type';
throws-like { EVAL 'my Int %a{Str}; my Str %b{Str} := %a' }, X::TypeCheck::Binding,
    'a keyed hash does not bind to a declaration of another value type';
throws-like { EVAL 'my Int %a; my Int %b{Int} := %a' }, X::TypeCheck::Binding,
    'a hash keyed by Str does not bind to a declaration of another key type';
throws-like { EVAL 'my Int %a; my Int %b{Str} := %a' }, X::TypeCheck::Binding,
    'a hash keyed by Str(Any) does not bind to a declaration keyed by Str';

{
    my %a{Str()} = a => 1;
    my %b{Str()} := %a;
    ok %b =:= %a,
        'a hash keyed by Str() binds to a declaration of the same key type';
}
{
    my %a{Int()} = 1 => 1;
    my %b{Int()} := %a;
    ok %b =:= %a,
        'a hash keyed by Int() binds to a declaration of the same key type';
    my Int() %c{Str} = a => 1;
    my Int() %d{Str} := %c;
    ok %d =:= %c,
        'a keyed hash of Int() values binds to a declaration of the same value and key types';
}
throws-like { EVAL 'my %a{Int}; my %b{Str()} := %a' }, X::TypeCheck::Binding,
    'a hash keyed by Int does not bind to a declaration keyed by Str()';
throws-like { EVAL 'my Int %a{Str}; my Int %b{Int()} := %a' }, X::TypeCheck::Binding,
    'a hash keyed by Str does not bind to a declaration keyed by Int()';
throws-like { EVAL 'my Str %a{Str}; my Int() %b{Str} := %a' }, X::TypeCheck::Binding,
    'a hash of Str values does not bind to a declaration of Int() values';

{
    my class KeyedAssociative does Associative[Int,Str] { method AT-KEY($) { 42 } }
    my Int %h{Str} := KeyedAssociative.new;
    is %h<a>, 42,
        'a keyed hash declaration binds a class Associative of its value and key types';
}

{
    sub value-typed(Int %h) { %h.elems }
    my %h := Hash[Int,Str].new;
    %h<a> = 1;
    is value-typed(%h), 1,
        'an object hash binds to a parameter that only names its value type';
}

is Hash[Int,Str].new.of, Int,
    'an object hash has the value type it is parameterized with';

# vim: expandtab shiftwidth=4
