use v6.e.PREVIEW;
use Test;
use nqp;

plan 48;

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
throws-like { @a[||(lazy 1,0)] }, X::Cannot::Lazy,
    'an array subscript refuses a lazy list of indices';
throws-like { @a[1; ||(lazy 0,)] }, X::Cannot::Lazy,
    'an array subscript refuses a lazy list of indices after another dimension';
throws-like { @a[||(lazy 1,0)] = 9 }, X::Cannot::Lazy,
    'assigning to an array subscript refuses a lazy list of indices';
throws-like { @a».[||(lazy 0,)] }, X::Cannot::Lazy,
    'a hyper array subscript refuses a lazy list of indices';
throws-like { @a[||(lazy 1,0)] := 42 }, X::Cannot::Lazy,
    'binding through an array subscript refuses a lazy list of indices';
throws-like { @a[|| ((lazy 1,0) | (0,1))] }, X::Cannot::Lazy,
    'an array subscript refuses a junction with a lazy list of indices';

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
%h = a => { b => 1 };
throws-like { %h{||(lazy <a b>)}:delete }, X::Cannot::Lazy,
    'a hash subscript refuses a lazy list of keys';
throws-like { %h».{||(lazy ('b',))} }, X::Cannot::Lazy,
    'a hyper hash subscript refuses a lazy list of keys';
throws-like { %h{||(lazy <a b>)} := 42 }, X::Cannot::Lazy,
    'binding through a hash subscript refuses a lazy list of keys';

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
throws-like { %s{||(lazy ('b', 2))}:delete }, X::Cannot::Lazy,
    'a shaped hash subscript refuses a lazy list of keys';
my %r{Int;Int};
%r{1;2} = 5;
is %r{||(1..2)}, 5,
    'a shaped hash subscript takes the keys of a Range interpolated with ||';
is-deeply %r{||(^0)}.pairs.List, ((1, 2) => 5,),
    'interpolating an empty Range into a shaped hash subscript takes the whole hash';

my @b = [5,6],[7,8];
is @b[|| (1, 0).map({ $_ })], 7,
    'a Seq of indices interpolated with || is not used up by the check for laziness';
is @b[|| ((1, 0) | (0, 1))].raku, 'any(7, 6)',
    'an array subscript takes a junction of lists of indices that are not lazy';
is-deeply @b».[||@none], ([5,6],[7,8]),
    'a hyper array subscript of no indices takes each element whole';
my %x = x => { a => 1 };
is-deeply %x».{||@none}, {x => {a => 1}},
    'a hyper hash subscript of no keys takes each element whole';
my @c = [[1,2],[3,4]], [[5,6],[7,8]];
is-deeply @c».[||(1,), 0], ((3,), (7,)),
    'a hyper array subscript interpolates a list that leads a comma list as a subscript does';
sub failing { fail 'boom' }
throws-like { @none».[|| failing()] }, X::AdHoc, message => 'boom',
    'a hyper subscript throws the Failure that || interpolates';
my int @ni = 1, 0;
is @b[||@ni], 7,
    'an array subscript takes the indices of a native array interpolated with ||';
my str @nk = <a b>;
my %n = a => { b => 1 };
is %n{||@nk}, 1,
    'a hash subscript takes the keys of a native array interpolated with ||';
is @b[||(0..1)], 6,
    'an array subscript takes the indices of a Range interpolated with ||';
my %p = 1 => { 2 => 'x' };
is %p{||(1..2)}, 'x',
    'a hash subscript takes the keys of a Range interpolated with ||';
my $v = 1;
@b[||(0..1)] := $v;
$v = 8;
is @b[0;1], 8,
    'binding through an array subscript of a Range interpolated with || binds the element';
my $w = 1;
%p{||(1..2)} := $w;
$w = 'y';
is %p{1}{2}, 'y',
    'binding through a hash subscript of a Range interpolated with || binds the element';
is @b[||Blob.new(1, 0)], 7,
    'an array subscript takes the indices of a Blob interpolated with ||';
my @u = [1,2],[3,4];
my Positional $up;
try { @u[||$up] = 9 }
is-deeply @u, [[1,2],[3,4]],
    'assigning through a subscript of an undefined Positional interpolated with || changes nothing';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    throws-like { @b[0, ||(lazy 1,0)] }, X::Cannot::Lazy,
        'an array subscript refuses a lazy list that || interpolates after another item';
}
else {
    skip 'the legacy frontend only takes a || that leads a comma list', 1;
}

# vim: expandtab shiftwidth=4
