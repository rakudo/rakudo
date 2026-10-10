use Test;

plan 138;

use MONKEY-SEE-NO-EVAL;
use nqp;

my class NestedHash is Hash { }

# the type and its parts
is EVAL(q[my %h{Str;Int}; %h.^name]), 'Hash[Any,(Str,Int)]',
    'a hash of two dimensions is of a hash type keyed by a list of its key types';
is EVAL(q[my Int %h{Str;Int}; %h.^name]), 'Hash[Int,(Str,Int)]',
    'a typed hash of two dimensions is of a hash type of its value type';
ok EVAL(q[my Int %h{Str;Int}; %h.WHAT =:= Hash[Int,(Str,Int)]]),
    'the type of a hash of two dimensions is the one its type name gives';
ok EVAL(q[my Int %h{Str;Int}; %h.of]) === Int,
    'a hash of two dimensions has the value type it is declared with';
ok EVAL(q[my %h{Str;Int}; %h.keyof]) === List,
    'a hash of two dimensions is keyed by a list';
is-deeply EVAL(q[my %h{Str;Int;Str}; %h.shape]), (Str, Int, Str),
    'a hash of three dimensions has the key types of its shape';
ok EVAL(q[my %h{Str;Int}; %h ~~ Associative[Any,List]]),
    'a hash of two dimensions is Associative of its value type and List';
is EVAL(q[my $types = (Str, Int); Hash.^parameterize(Int, $types).^name]), 'Hash[Int,(Str,Int)]',
    'a hash type takes an itemized list of key types';
is-deeply EVAL(q[my role ListTaking[$x] { method x { $x } }; my ListTaking[(1, 2)] $v; $v.x.List]), (1, 2),
    'a role takes a list of values as its argument';
is-deeply EVAL(q[my role PairTaking[$x] { method x { $x } }; my PairTaking[(:a(1), 2)] $v; $v.x.List]), (:a(1), 2),
    'a role takes a list holding a pair as its argument';

# one key for each dimension
is EVAL(q[my Int %h{Str;Int}; %h{'a';1} = 42; %h{'a';1}]), 42,
    'a hash of two dimensions stores a value under a key for each dimension';
isa-ok EVAL(q[my Int %h{Str;Int}; %h{'a';1} = 42; %h{'a';1}]), Int,
    'a key for each dimension gives the element itself rather than a list';
is EVAL(q[my Int %h{Str;Int}; %h{'a';1} = 42; %h{'a';1} += 1; %h{'a';1}]), 43,
    'an element of a hash of two dimensions takes an assignment operator';
is EVAL(q[my Int %h{Str;Int}; %h{'a';1}++; %h{'a';1}++; %h{'a';1}]), 2,
    'an element of a hash of two dimensions takes an increment';
ok EVAL(q[my Int %h{Str;Int}; %h{'a';1}]) === Int,
    'a missing element of a typed hash of two dimensions is its value type';
is EVAL(q[my %h{Str;Int}; %h{'a';1}; %h{'a';1}:exists; %h.elems]), 0,
    'reading and testing a missing element do not store it';
is EVAL(q[my Int %h{Str;Int;Str}; %h{'a';1;'b'} = 3; %h{'a';1;'b'}]), 3,
    'a hash of three dimensions stores a value under a key for each dimension';
is EVAL(q[my %h{Str;Int}; my $v = 1; %h{'a';1} := $v; $v = 2; %h{'a';1}]), 2,
    'binding an element of a hash of two dimensions binds the container';
is EVAL(q[my %h{Str;Int}; %h{'a';1} = 3; my $key = ('a', 1).Seq; %h{$key}]), 3,
    'a Seq of a key for each dimension is a key of a hash of two dimensions';
is EVAL(q[my %h{Str;Str}; %h{'a';'bc'} = 1; %h{'ab';'c'} = 2; %h.elems]), 2,
    'keys that join to the same text are different keys';
is EVAL(q[my %h{Mu;Mu}; %h{1;'1'} = 1; %h{'1';1} = 2; %h{1;'1'}]), 1,
    'keys of different types with the same text are different keys';
is-deeply EVAL(q[my @key is default(42); @key[1] = 1; my %h{Int;Int}; %h{$@key} = 1; %h.keys.List]), ((42, 1),),
    'a missing element of an array of keys is its default';

# adverbs
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; (%h{'a';1}:exists, %h{'a';2}:exists, %h{'a';2}:!exists)]),
    (True, False, True),
    'an element of a hash of two dimensions takes :exists';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; (%h{'a';1}:delete, %h.elems)]), (42, 0),
    'an element of a hash of two dimensions takes :delete';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; %h{'a';1}:p]), ('a', 1) => 42,
    'an element of a hash of two dimensions takes :p';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; %h{'a';1}:kv]), (('a', 1), 42),
    'an element of a hash of two dimensions takes :kv';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; %h{'a';1}:k]), ('a', 1),
    'an element of a hash of two dimensions takes :k';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; my $name; my @keys; for <a b> { $name = $_; @keys.push: %h{$name;1}:k }; @keys.List]),
    (('a', 1), ('b', 1)),
    'the keys :k gives do not change with the variables they came from';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; my $name = 'a'; my @keys = %h{$name,'b';1}:k; $name = 'z'; @keys.List]),
    (('a', 1), ('b', 1)),
    'the keys a slice with :k gives do not change with the variables they came from';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';2}:p]), (),
    'a missing element of a hash of two dimensions gives nothing for :p';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 42; (%h{'a';1}:delete:p, %h.elems)]), (('a', 1) => 42, 0),
    'an element of a hash of two dimensions takes :delete with :p';

# listing
is EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'a';2} = 2; %h{'b';1} = 3; %h.elems]), 3,
    'a hash of two dimensions counts its elements';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; %h.keys.sort.List]),
    (('a', 1), ('b', 2)),
    'a hash of two dimensions lists a key for each dimension of each element';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; %h.values.sort.List]), (1, 2),
    'a hash of two dimensions lists the values of its elements';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h.pairs.List]), (('a', 1) => 1,),
    'a hash of two dimensions pairs each value with its list of keys';
is EVAL(q[my %h{Str;Int}; %h{'a';1} = 7; my $key = %h.keys.head; %h{$key}]), 7,
    'a list of keys from a hash of two dimensions is a key of that hash';

# dimensions left out
my $wild = q[my %h{Str;Int}; %h{'a';1} = 1; %h{'a';2} = 2; %h{'b';1} = 3;];
is-deeply EVAL($wild ~ q[%h{'a'}.sort.List]), (1, 2),
    'a key for the first dimension alone takes any key for the second';
is-deeply EVAL($wild ~ q[%h{'a','b'}.sort.List]), (1, 2, 3),
    'a slice of the first dimension alone takes any key for the second';
is-deeply EVAL($wild ~ q[%h{'z'}.List]), (),
    'a key for the first dimension alone that no element has takes nothing';
is-deeply EVAL($wild ~ q[%h{**}.sort.List]), (1, 2, 3),
    'a hyper whatever star alone takes any key for each dimension';
is-deeply EVAL($wild ~ q[%h{'a';**}.sort.List]), (1, 2),
    'a trailing hyper whatever star takes any key for the dimensions left';
is-deeply EVAL($wild ~ q[(%h{'a'}:exists).List]), (True, True),
    'a key for the first dimension alone tests each element it takes';
is-deeply EVAL($wild ~ q[%h{'a'}:k.sort.List]), (('a', 1), ('a', 2)),
    'a key for the first dimension alone gives the keys of each element it takes';
is-deeply EVAL($wild ~ q[(%h{'a'}:delete).sort.List, %h.keys.List]), ((1, 2), (('b', 1),)),
    'a key for the first dimension alone deletes each element it takes';
is-deeply EVAL($wild ~ q[my $key = ('a',); %h{$key}.sort.List]), (1, 2),
    'a list of keys for fewer dimensions takes any key for those left out';
is-deeply EVAL($wild ~ q[%h{%h.keys}.sort.List]), (1, 2, 3),
    'a slice of lists of keys takes the element of each';
is-deeply EVAL($wild ~ q[%h{()}.List]), (),
    'an empty slice takes nothing';
is-deeply EVAL($wild ~ q[(%h{'a','b'}:exists).List]), (True, True, True),
    'a slice of the first dimension alone tests each element it takes';
is EVAL($wild ~ q[(%h{*}:k).elems]), 3,
    'a whatever star alone gives the keys of each element';
ok EVAL($wild ~ q[%h{} =:= %h]),
    'a zen slice of a hash of two dimensions without an adverb gives the hash itself';
is-deeply EVAL($wild ~ q[%h{}:p.sort(*.value).List]), (('a', 1) => 1, ('a', 2) => 2, ('b', 1) => 3),
    'a zen slice gives a pair of the list of keys and the value of each element for :p';
is-deeply EVAL($wild ~ q[%h{}:k.sort.List]), (('a', 1), ('a', 2), ('b', 1)),
    'a zen slice gives the list of keys of each element for :k';
is-deeply EVAL($wild ~ q[%h{}:kv.batch(2).sort(*.[1]).List]), ((('a', 1), 1), (('a', 2), 2), (('b', 1), 3)),
    'a zen slice gives the list of keys and the value of each element for :kv';
is-deeply EVAL($wild ~ q[%h{}:!k.sort.List]), (('a', 1), ('a', 2), ('b', 1)),
    'a zen slice gives the list of keys of each element for :!k';
is-deeply EVAL($wild ~ q[(%h{}:delete:k).sort.List, %h.elems]), ((('a', 1), ('a', 2), ('b', 1)), 0),
    'a zen slice deletes each element and gives its list of keys for :delete:k';
is-deeply EVAL(q[my %h{Str;Int;Str}; %h{'a';1;'x'} = 1; %h{}:k]), (('a', 1, 'x'),),
    'a zen slice of a hash of three dimensions gives the list of keys of each element for :k';
throws-like { EVAL $wild ~ q[my $v; %h{} := $v] }, X::Bind::ZenSlice,
    'binding a zen slice of a hash of two dimensions is refused';
is EVAL($wild ~ q[my $a = ('a', 1); my $b = ('b', 1); %h{$a|$b}.raku]), 'any(1, 3)',
    'a junction of lists of keys takes the element of each';
is-deeply EVAL(q[my %h{Mu;Int}; my $j = 'a'|'b'; %h{$j;1} = 5; %h{'a';1} = 6; %h{$j}.List]), (5,),
    'a junction is a key for a first dimension that takes it';
throws-like { EVAL $wild ~ q[my $v; %h{*} := $v] }, X::Bind::Slice,
    'binding a whatever star alone is refused as for any slice';
throws-like { EVAL $wild ~ q[%h{'a'} += 10] }, X::Assignment::RO,
    'the elements a dimension left out takes cannot be assigned';
throws-like { EVAL $wild ~ q[%h{*;1} += 10] }, X::Assignment::RO,
    'the elements a whatever star takes cannot be assigned';
is EVAL($wild ~ q[%h{'a';1;**} = 7; %h{'a';1}]), 7,
    'a trailing hyper whatever star for no dimension left takes an assignment';
throws-like { EVAL $wild ~ q[%h{**;1}] }, Exception,
    message => /'**'/,
    'a hyper whatever star before the last dimension is refused';
is-deeply EVAL(q[my %h{Any;Int}; %h{List;1} = 5; %h{List}.List]), (5,),
    'a type object for the first dimension alone is a key rather than a slice';
is-deeply EVAL(q[my %h{Int(Str);Str}; %h{1;'x'} = 9; %h{'1'}.List]), (9,),
    'a key for the first dimension alone is coerced to its type';
is-deeply EVAL(q[my %h{Str;Int;Str}; %h{'a';1;'x'} = 1; %h{'a';2;'y'} = 2; %h{'b';1;'x'} = 3; (%h{'a';1}.List, %h{'a'}.sort.List)]),
    ((1,), (1, 2)),
    'a hash of three dimensions takes any key for each dimension left out';

# set operators
ok EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; %h.keys.head ∈ %h]),
    'a list of keys from a hash of two dimensions is an element of it';
nok EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; ('a', 1) ∈ %h]),
    'another list of the same keys is not an element of a hash of two dimensions';
is EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; (%h ⊖ %h).elems]), 0,
    'the symmetric difference of a hash of two dimensions with itself is empty';
ok EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; %h.keys (==) %h]),
    'a hash of two dimensions is equal to the set of its keys';
ok EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; (%h.keys.head,) (<=) %h]),
    'a list holding a list of keys from a hash of two dimensions is a subset of it';

# slices
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; %h{'a','b';1}.List]), (1, 2),
    'a hash of two dimensions takes a slice of the keys of a dimension';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a','b';1} = 1, 2; %h.sort.List]), (('a', 1) => 1, ('b', 1) => 2),
    'a hash of two dimensions assigns a slice of the keys of a dimension';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; %h{'b';2} = 3; %h{*;1}.sort.List]), (1, 2),
    'a whatever star in a dimension matches the elements with any key there';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; %h{'a';*}.List]), (1,),
    'a whatever star matches only the elements whose other keys are given';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; %h{*;1}:delete; %h.elems]), 0,
    'a whatever star in a dimension takes :delete';
is-deeply EVAL(q[my %h{Int(Str);Str}; %h{1;'a'} = 5; %h{'1';*}.List]), (5,),
    'a whatever star slice coerces the keys given for the other dimensions';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';1} = 1; %h{1;*}] }, X::TypeCheck::Binding,
    expected => Str,
    'a whatever star slice checks the keys given for the other dimensions';
is EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; %h{'a'|'b';*}.raku]), 'any((1,), (2,))',
    'a junction key in a whatever star slice gives a junction of slices';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';1} = 1; %h{*;1} = 2] }, Exception,
    message => /'non-deterministic'/,
    'a whatever star slice cannot be assigned';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';1} = 1; my $v; %h{*;1} := $v] }, X::Bind::Slice,
    'a whatever star slice cannot be bound';

# what is refused
throws-like { EVAL q[my %h{Str;Int}; %h{'a';'b'} = 1] }, X::TypeCheck::Binding,
    expected => Int,
    'a hash of two dimensions checks the key type of its second dimension';
throws-like { EVAL q[my %h{Str;Int}; %h{1;1} = 1] }, X::TypeCheck::Binding,
    expected => Str,
    'a hash of two dimensions checks the key type of its first dimension';
throws-like { EVAL q[my Int %h{Str;Int}; %h{'a';1} = 'x'] }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of two dimensions checks its value type';
throws-like { EVAL q[my %h{Str;Int}; %h{'a'} = 1] }, X::NotEnoughDimensions,
    message => /hash/,
    'a hash of two dimensions refuses assigning a key for fewer dimensions';
throws-like { EVAL q[my %h{Str;Int;Str}; %h{'a';1} = 1] }, X::NotEnoughDimensions,
    'a hash of three dimensions refuses assigning a key for two';
throws-like { EVAL q[my %h{Str;Int}; my $v; %h{'a'} := $v] }, X::NotEnoughDimensions,
    'a hash of two dimensions refuses binding a key for fewer dimensions';
throws-like { EVAL q[my %h{Str;Int;Str}; %h{'a';1;'x'} = 1; %h{'a';*} = 1] }, X::NotEnoughDimensions,
    'a hash of three dimensions refuses assigning a slice for two';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';1} = 1; %h<a><b>] }, Exception,
    message => /associative/,
    'a cascaded subscript of a hash of two dimensions fails rather than taking other keys';
throws-like { EVAL q[my %h{Str;Int}; %h.AT-KEY('a')] }, X::NotEnoughDimensions,
    'an element of a hash of two dimensions needs a key for each dimension';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';1;2} = 1] }, X::TooManyDimensions,
    message => /hash/,
    'a hash of two dimensions refuses a key for more dimensions';
throws-like { EVAL q[my %h{Str;Int}; %h{*;1;2}] }, X::TooManyDimensions,
    'a whatever star slice refuses keys for more dimensions';
throws-like { EVAL q[my %h{Str;Int}; %h{'a';1} = 1; %h{('a'|'b', 'c');*}] }, X::TypeCheck::Binding,
    'a whatever star slice checks a junction among the keys of a dimension';
throws-like { EVAL q[my %h{Int;Int}; my $key = (1...*); %h{$key}] }, X::Cannot::Lazy,
    'a hash of two dimensions refuses a lazy list as a key';
throws-like { EVAL q[my %h{Str;int}] }, Exception,
    message => /native/,
    'a hash of two dimensions refuses a native key type';
throws-like { EVAL q[Hash[Int,(Str,int)]] }, Exception,
    message => /native/,
    'a hash type refuses a native key type in a list of key types';
is EVAL(q[my %h{Str;Int}; try %h{'a';'b'} = 1; %h.elems]), 0,
    'a refused key stores nothing';

# key types
is-deeply EVAL(q[my %h{Int(Str);Str}; %h{'1';'a'} = 1; %h.keys.List]), ((1, 'a'),),
    'a hash of two dimensions coerces a key of a coercive type';
is-deeply EVAL(q[my %h{Int(Str);Str}; try %h{'x';'a'} = 1; ($!.^name, %h.elems)]), ('X::Str::Numeric', 0),
    'a key that fails to coerce throws and stores nothing';
throws-like { EVAL q[my subset Pos of Int where * > 0; my %h{Pos;Str}; %h{-1;'a'} = 1] }, X::TypeCheck::Binding,
    'a hash of two dimensions checks the where constraint of a key';
is EVAL(q[my %h{Str:D;Int:D}; %h{'a';1} = 1; %h{'a';1}]), 1,
    'a hash of two dimensions takes keys of definite types';
throws-like { EVAL q[my %h{Str:D;Int}; %h{Str;1} = 1] }, X::TypeCheck::Binding,
    'a hash of two dimensions refuses a type object for a definite key type';
throws-like { EVAL q[my %h{Str;Int}; %h{Failure.new('bad');1} = 1] }, Exception,
    message => 'bad',
    'a Failure as a key throws';
is EVAL(q[my %h{Mu;Mu}; %h{Failure;Int} = 1; %h{Failure;Int}]), 1,
    'a Failure type object is a key like any other type object';
is-deeply EVAL(q[my %h{Str;Int}; %h{'a'|'b';1} = 7; %h.sort.List]), (('a', 1) => 7, ('b', 1) => 7),
    'a junction key of a hash of two dimensions assigns each of its keys';
ok EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; %h{'a'|'b';1} == 1|2]),
    'a junction key of a hash of two dimensions reads each of its keys';
ok EVAL(q[my %h{Str;Int}; %h{'a';1} = 1; %h{'b';1} = 2; my $key = ('a'|'b', 1).Seq; %h{$key} == 1|2]),
    'a junction in a Seq of keys reads each of its keys';
is EVAL(q[my %h{Junction;Int}; %h{1|2;1} = 3; %h.keys.head[0].raku]), 'any(1, 2)',
    'a dimension of type Junction takes a junction as its key';

# declarations
is EVAL(q[my Int %h{Str;Int} is default(42); %h{'a';1}]), 42,
    'a hash of two dimensions takes is default';
is EVAL(q[my Int %h{Str;Int} is default(42); %h{'a';1} = 1; %h{'a';1} = Nil; %h{'a';1}]), 42,
    'assigning Nil to an element of a hash of two dimensions gives its default';
ok EVAL(q[my %h{Str;Int} of Int; %h.of]) === Int,
    'a hash of two dimensions takes an of trait';
throws-like { EVAL q[my Int %h{Str;Int} where * > 0; %h{'a';1} = -1] }, X::TypeCheck::Assignment,
    'a hash of two dimensions checks the where constraint of its values';
ok EVAL(q[my %h{Str;Int} is NestedHash; %h.WHAT ~~ NestedHash]),
    'a hash of two dimensions is of the type of an is trait';
is EVAL(q[my %h{Str;Int} is NestedHash; %h{'a';1} = 2; %h{'a';1}]), 2,
    'a hash of two dimensions of the type of an is trait stores a value';
is-deeply EVAL(q[my %h{Str;Int} = ('a', 1) => 1, ('b', 2) => 2; %h.sort.List]), (('a', 1) => 1, ('b', 2) => 2),
    'a hash of two dimensions is initialized from pairs keyed by lists';
throws-like { EVAL q[my %h{Str;Int} = a => 1] }, X::NotEnoughDimensions,
    'a hash of two dimensions refuses an initializer keyed by a single key';
is EVAL(q[class { has Int %.h{Str;Int} }.new.h{'a';1}.^name]), 'Int',
    'an attribute takes a shape of two dimensions';
is EVAL(q[sub counted { state %h{Str;Int}; %h{'a';1}++ }; counted; counted; counted]), 2,
    'a state variable takes a shape of two dimensions';
is EVAL(q[my class KeyedNode { has Hash[Int,(::?CLASS,Int)] $.h }; KeyedNode.new.h.^name]), 'Hash[Int,(KeyedNode,Int)]',
    'a hash type takes the class being declared among its key types';
is EVAL(q[my Hash[Int,(Str,Int)] $h; $h{'a';1} = 5; $h{'a';1}]), 5,
    'a type object of a hash of two dimensions vivifies through a key for each dimension';
isa-ok EVAL(q[my Hash[Int,(Str,Int)] $h; $h{'a';1} = 5; $h]), Hash[Int,(Str,Int)],
    'a type object of a hash of two dimensions vivifies its own type';

# binding
ok EVAL(q[my Int %a{Str;Int}; my Int %b{Str;Int} := %a; %b =:= %a]),
    'a hash of two dimensions binds to a declaration of the same shape';
throws-like { EVAL q[my Int %a{Str;Int}; my Int %b{Str;Str} := %a] }, X::TypeCheck::Binding,
    'a hash of two dimensions does not bind to a declaration of another shape';
throws-like { EVAL q[my Int %a{Str}; my Int %b{Str;Int} := %a] }, X::TypeCheck::Binding,
    'a hash of one dimension does not bind to a declaration of two';

# a hash of hashes
is EVAL(q[my %h; my $v = 1; %h{'a';'b'} := $v; $v = 2; %h<a><b>]), 2,
    'binding through a multidimensional subscript of a hash of hashes binds the container';
throws-like { EVAL q[my %h; my $v; %h{'a','b';'c'} := $v] }, X::Bind::Slice,
    'binding through a multidimensional slice of a hash of hashes is refused';

# raku and gist
is EVAL(q[my %h{Mu;Int}; %h{1|2;1} = 3; %h{5;2} = 4; %h.gist]), '{(5 2) => 4, (any(1, 2) 1) => 3}',
    'the gist of a hash of two dimensions shows a junction among its keys';
is EVAL(q[my %h{Mu;Int}; %h{Int;1} = 3; %h{'!';2} = 4; %h.gist]), '{(! 2) => 4, ((Int) 1) => 3}',
    'the gist of a hash of two dimensions shows a type object among its keys';
is EVAL(q[my Int %h{Str;Int}; %h{'a';1} = 1; %h.raku]), '(my Int %{Str;Int} = ("a", 1) => 1)',
    'a hash of two dimensions gives its declaration and pairs as its raku';
ok EVAL(q[my Int %h{Str;Int}; %h{'a';1} = 1; %h{'b';2} = 2; EVAL(%h.raku) eqv %h]),
    'the raku of a hash of two dimensions evaluates to an equivalent hash';

# generics
is EVAL(q[my role ValueStore[::T] { has T %.h{Str;Str} }; my $r = ValueStore[Int].new; $r.h{'a';'b'} = 42; $r.h{'a';'b'}]), 42,
    'a hash of two dimensions stores a value of a type that a role takes';
throws-like { EVAL q[my role ValueTyped[::T] { has T %.h{Str;Str} }; ValueTyped[Int].new.h{'a';'b'} = 'x'] },
    X::TypeCheck::Assignment, expected => Int,
    'a hash of two dimensions checks a value type that a role takes';
is-deeply EVAL(q[my role KeyTyped[::T] { has %.h{Str;T} }; KeyTyped[Int].new.h.shape]), (Str, Int),
    'a hash of two dimensions is keyed by a type that a role takes';
throws-like { EVAL q[my role KeyChecked[::T] { has %.h{Str;T} }; KeyChecked[Int].new.h{'a';'b'} = 1] },
    X::TypeCheck::Binding, expected => Int,
    'a hash of two dimensions checks a key type that a role takes';
ok EVAL(q[my role DefiniteTyped[::T] { has T:D %.h{Str;Str} }; DefiniteTyped[Int].new.h{'a';'b'}]) === Int,
    'a missing element of a definite type that a role takes is the base type';

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[sub f(::T $x) { my T %h{Str;T}; %h{'a';$x} = $x; %h{'a';$x} }; f(42)]), 42,
        'a hash of two dimensions in a routine stores a value of a type capture';
}
else {
    skip 'the legacy frontend cannot instantiate a generic hash type in a routine';
}

throws-like { EVAL q[my int %h{Str;Str}] }, X::Comp::NYI,
    feature => 'native value types for hashes',
    'a hash of two dimensions refuses a native value type';

# vim: expandtab shiftwidth=4
