use v6.e.PREVIEW;
use Test;

plan 21;

my @none;

my @a = [1,2],[3,4];
is-deeply @a[||@none], [[1,2],[3,4]],
    'interpolating no indices into an array subscript takes the whole array';
is-deeply @a[||().Seq], [[1,2],[3,4]],
    'interpolating an empty Seq into an array subscript takes the whole array';
is-deeply (@a[||@none]:k, @a[||@none]:exists), ((0, 1), (True, True)),
    'an adverb on an array subscript of no indices applies to each element';
throws-like { @a[||@none]:foo }, X::Adverb,
    'an array subscript of no indices refuses an unknown adverb';
@a[||@none] = [5,6],[7,8];
is-deeply @a, [[5,6],[7,8]],
    'assigning to an array subscript of no indices assigns the whole array';
throws-like { @a[||@none] := 42 }, X::Bind::ZenSlice,
    'binding to an array subscript of no indices is refused';
is-deeply ((@a[||@none]:delete).List, @a.elems), (([5,6],[7,8]), 0),
    'deleting an array subscript of no indices deletes each element';

my int @n[2;2] = (1,2),(3,4);
is-deeply (@n[||@none].shape, @n[||@none][1;0]), ((2, 2), 3),
    'reading a native shaped array subscript of no indices takes the whole array';
@n[||@none] = (5,6),(7,8);
is-deeply (@n[0;1], @n[1;0]), (6, 7),
    'assigning to a native shaped array subscript of no indices assigns the whole array';
my $x = [[1,2],[3,4]];
is-deeply $x[||@none], [[1,2],[3,4]],
    'interpolating no indices into a subscript of an array in a scalar takes the whole array';

my %h = a => { b => 1 };
is-deeply %h{||@none}, {a => {b => 1}},
    'interpolating no keys into a hash subscript takes the whole hash';
is-deeply %h{|| <x y>.grep(/z/)}, {a => {b => 1}},
    'interpolating a Seq of no keys into a hash subscript takes the whole hash';
is-deeply (%h{||@none}:k, %h{||@none}:exists), (('a',), (True,)),
    'an adverb on a hash subscript of no keys applies to each element';
%h{||@none} = c => 3;
is-deeply %h, {c => 3},
    'assigning to a hash subscript of no keys assigns the whole hash';
is-deeply (%h{||@none}:p, %h{||@none}:kv, %h{||@none}:v), ((c => 3,), ('c', 3), (3,)),
    'a hash subscript of no keys takes :p, :kv and :v';
is-deeply (%h{||@none}:!exists, %h{||@none}:!k), ((False,), ('c',)),
    'a hash subscript of no keys takes a negated adverb as the zen slice does';
is-deeply ((%h{||@none}:delete).List, %h.elems), ((3,), 0),
    'deleting a hash subscript of no keys deletes each element';

my %s{Str;Int};
%s{'a';1} = 1;
is-deeply %s{||@none}.pairs.List, (('a', 1) => 1,),
    'interpolating no keys into a shaped hash subscript takes the whole hash';
is-deeply %s{||@none}:k, (('a', 1),),
    'an adverb on a shaped hash subscript of no keys applies to each element';
throws-like { %s{||@none} := 42 }, X::Bind::ZenSlice,
    'binding to a shaped hash subscript of no keys is refused';
%s{||@none} = ('b', 2) => 5;
is-deeply %s.pairs.List, (('b', 2) => 5,),
    'assigning to a shaped hash subscript of no keys assigns the whole hash';

# vim: expandtab shiftwidth=4
