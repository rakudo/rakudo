use Test;

plan 106;

# a view of the dimensions left
{
    my @a[2;3] = (1,2,3),(4,5,6);
    is-deeply @a[1].List, (4, 5, 6),
        'an index for the first dimension alone gives the elements of the dimension left';
    is @a[1].elems, 3,
        'a view of the dimension left has its length as its elements';
    is-deeply @a[1].shape, (3,),
        'a view of the dimension left has its shape';
    is @a[1].gist, '[4 5 6]',
        'a view gives the gist of an array of its elements';
    is @a[1].sum, 15,
        'a view iterates its elements';
    is-deeply @a[1].kv.List, (0, 4, 1, 5, 2, 6),
        'a view of one dimension left is keyed by Int';
    ok (4, 5, 6) ~~ @a[1],
        'a view smartmatches a list of its elements';
    throws-like { @a[1].push(1) }, X::IllegalOnFixedDimensionArray,
        'a view refuses a push as a shaped array does';
    is @a[0][1], 2,
        'a cascaded subscript takes the element the multidimensional one takes';
    ok @a[0][1]:exists,
        'a cascaded subscript tests the element the multidimensional one tests';
    @a[0][1] = 20;
    is @a[0;1], 20,
        'assigning through a cascaded subscript assigns the element';
    throws-like { @a[1] = 7, 8, 9 }, X::NotEnoughDimensions,
        'assigning to an index for the first dimension alone is refused';
    throws-like { @a[5] }, Exception, message => /'Index 5 for dimension 1'/,
        'an index out of range for the first dimension alone is refused';
}

{
    my @b[2;2] = (1,2),(3,4);
    throws-like { @b[1]:delete }, X::NotEnoughDimensions,
        'deleting an index for the first dimension alone is refused';
    is-deeply @b[1;*]:exists, (True, True),
        'refusing to delete an index for the first dimension alone deletes nothing';
}

{
    my @c[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    is @c[1].gist, "[[5 6]\n [7 8]]",
        'a view of two dimensions left gives the gist of a shaped array';
    is-deeply @c[1][0].List, (5, 6),
        'a view of a view takes an index for its first dimension';
    is @c[1][0][1], 6,
        'a view of a view of a view gives the element';
    is-deeply @c[1;0].List, (5, 6),
        'indices for fewer dimensions than three give a view of the dimension left';
    @c[1][0][1] = 9;
    is @c[1;0;1], 9,
        'assigning through a view of a view assigns the element';
}

{
    my int @n[2;2] = (1,2),(3,4);
    is-deeply @n.AT-POS(0).List, (1, 2),
        'an index for the first dimension alone of a native array gives a view';
    is-deeply @n[*;1], (2, 4),
        'a whatever star in a dimension of a native array takes each of its indices';
    @n.AT-POS(0)[1] = 5;
    is @n[0;1], 5,
        'assigning through a view of a native array assigns the element';
}

{
    my @f[2;2;2;2];
    @f[1;1;1;1] = 9;
    is-deeply @f[1;1;1].List, (Any, 9),
        'indices for three of four dimensions give a view of the dimension left';
    is-deeply @f[*;1;1;1], (Any, 9),
        'a whatever star in a dimension of four takes each of its indices';
}

# slices
{
    my @a[2;3] = (1,2,3),(4,5,6);
    is-deeply @a[*;0], (1, 4),
        'a whatever star in the first dimension takes each of its indices';
    is-deeply @a[0;*], (1, 2, 3),
        'a whatever star in the last dimension takes each of its indices';
    is-deeply @a[1;0..1], (4, 5),
        'a range in a dimension takes each of its indices';
    is-deeply @a[0,1;2], (3, 6),
        'a list in a dimension takes each of its indices';
    is-deeply @a[*-1;*-1], (6,),
        'a whatever code in a dimension takes the length of that dimension';
    is-deeply @a[0..1].map(*.List).List, ((1, 2, 3), (4, 5, 6)),
        'a slice of the first dimension alone gives a view for each index';
    is-deeply @a[|(1,0)].map(*.List).List, ((4, 5, 6), (1, 2, 3)),
        'a slip of indices is a slice of the first dimension';
    is-deeply @a[0..1;*], (1, 2, 3, 4, 5, 6),
        'a whatever star for the dimension left takes each of its indices';
    is-deeply @a[0..*;0], (1, 4),
        'a lazy range in a dimension takes the indices within it';
    is-deeply @a[1;**], (4, 5, 6),
        'a trailing hyper whatever star takes any index of the dimensions left';
    throws-like { @a[**;1] }, Exception, message => /'**'/,
        'a hyper whatever star before the last dimension is refused';
    throws-like { @a[*;5] }, Exception, message => /'Index 5 for dimension 2'/,
        'an index out of range in a slice is refused';
    @a[*;0] = 10, 40;
    is-deeply @a[*;0], (10, 40),
        'a slice of a shaped array takes an assignment';
    my @b[2;3];
    @b[0..1;*] = 1..6;
    is-deeply @b[*;*], (1, 2, 3, 4, 5, 6),
        'a slice of each dimension takes an assignment of each element';
    my $v;
    throws-like { @a[*;1] := $v }, X::Bind::Slice,
        'binding a slice of a shaped array is refused';
}

# adverbs on slices
{
    my @a[2;3] = (1,2,3),(4,5,6);
    is-deeply @a[*;0]:exists, (True, True),
        'a slice of a shaped array takes :exists';
    is-deeply @a[*;0]:k, ((0, 0), (1, 0)),
        'a slice of a shaped array takes :k';
    is-deeply @a[1;*]:v, (4, 5, 6),
        'a slice of a shaped array takes :v';
    is-deeply @a[*;0]:kv, ((0, 0), 1, (1, 0), 4),
        'a slice of a shaped array takes :kv';
    is-deeply @a[1;*]:p, ((1, 0) => 4, (1, 1) => 5, (1, 2) => 6),
        'a slice of a shaped array takes :p';
    my @b[2;2] = (1,2),(3,4);
    is-deeply @b[0;*]:delete, (1, 2),
        'a slice of a shaped array takes :delete';
    is-deeply (@b[0;0]:exists, @b[1;0]:exists), (False, True),
        'deleting a slice of a shaped array deletes its elements';
    is-deeply @a[0..2;0]:exists, (True, True, False),
        'a slice of a shaped array tests an index past its dimension';
    is-deeply @a[0;1..4]:exists, (True, True, False, False),
        'a slice of a shaped array tests an index past its last dimension';
    is-deeply @a[0,2;0]:k, ((0, 0),),
        'a slice of a shaped array takes :k with an index past its dimension';
    is-deeply @a[1;1,5]:delete, (5, Nil),
        'a slice of a shaped array takes :delete with an index past its dimension';
    is-deeply (@a[0]:!exists, @a[5]:!exists), (False, True),
        'an index for the first dimension alone takes :!exists';
}

{
    my @p = [1,2],[3,4];
    is-deeply @p[*;0]:exists, (True, True),
        'a slice of an array of arrays takes :exists';
    is-deeply @p[0..2;0]:exists, (True, True, False),
        'a slice of an array of arrays tests an element it does not have';
    is-deeply @p[*;0]:k, ((0, 0), (1, 0)),
        'a slice of an array of arrays takes :k';
    is-deeply @p[*;0]:v, (1, 3),
        'a slice of an array of arrays takes :v';
    is-deeply @p[*;0]:kv, ((0, 0), 1, (1, 0), 3),
        'a slice of an array of arrays takes :kv';
    is-deeply @p[*;0]:p, ((0, 0) => 1, (1, 0) => 3),
        'a slice of an array of arrays takes :p';
    my @q = [1,2],[3,4];
    is-deeply @q[*;1]:delete, (2, 4),
        'a slice of an array of arrays takes :delete';
    is-deeply @q, [[1], [3]],
        'deleting a slice of an array of arrays deletes its elements';
    is-deeply @p[0..*;0]:k, ((0, 0), (1, 0)),
        'a lazy index with an adverb takes the positions an array of arrays has';
}

# a Seq iterates when sunk
{
    my @a[2;2] = (1,2),(3,4);
    is-deeply @a[(0,1).map(*+0);1], (2, 4),
        'a Seq of indices is a slice of its dimension';
    @a[0;0] = (5,6).map(*+0);
    is-deeply (@a[0;*]:v).head.List, (5, 6),
        'a slice of a shaped array takes :v of an element holding a Seq';

    my class Seqs does Positional {
        method elems()          { 2 }
        method AT-POS(\pos)     { (pos, pos).map(*+0) }
        method EXISTS-POS(\pos) { 0 <= pos < 2 }
        method DELETE-POS(\pos) { (pos, pos).map(*+0) }
    }
    my @s = Seqs.new,;
    is-deeply (@s[0;*]:kv).[1].List, (0, 0),
        'a slice takes :kv of an element given as a Seq';
    is-deeply (@s[0;*]:delete).head.List, (0, 0),
        'a slice takes :delete of an element given as a Seq';
}

# binding
{
    my @p = [1,2],[3,4];
    my $v = 3;
    @p[0;1] := $v;
    $v = 4;
    is @p[0;1], 4,
        'binding through a multidimensional subscript binds the container';
    my @b[2;2];
    my $w = 3;
    @b[0][1] := $w;
    $w = 4;
    is @b[0;1], 4,
        'binding through a view binds the container';
    my $x;
    throws-like { @b[1] := $x }, X::NotEnoughDimensions,
        'binding an index for the first dimension alone is refused';
}

{
    my @s[3] = 1, 2, 3;
    is @s[1], 2,
        'an index of a shaped array of one dimension gives the element';
    is-deeply @s[0..*], (1, 2, 3),
        'a lazy range of a shaped array of one dimension takes the indices within it';
}

{
    my @u[2;2] = (1,2),(3,4);
    @u = @u[1], @u[0];
    is-deeply (@u[0].List, @u[1].List), ((3, 4), (1, 2)),
        'assigning views of a shaped array to it assigns the values they had';
}

{
    my int @m[2;3] = (1,2,3),(4,5,6);
    my $i = 1;
    is-deeply (@m[$i].List, @m[*-1].List, @m[1..1].map(*.List).List, @m[0,1].map(*.List).List),
      ((4, 5, 6), (4, 5, 6), ((4, 5, 6),), ((1, 2, 3), (4, 5, 6))),
        'a subscript of the first dimension alone of a native array gives views';
    is-deeply ((@m[$i]:v).List, (@m[$i]:kv)[1].List, (@m[$i]:p).value.List),
      ((4, 5, 6), (4, 5, 6), (4, 5, 6)),
        'an adverb on a subscript of the first dimension alone of a native array takes a view';
    throws-like { @m[$i] = 5 }, X::NotEnoughDimensions,
        'assigning to a subscript of the first dimension alone of a native array is refused';
    my num @c[2;2;2];
    @c[$i][0][1] = 2e0;
    is @c[1;0;1], 2e0,
        'assigning through views of a native array of three dimensions assigns the element';
    my int @e[2;3];
    @e = @m[1], @m[0];
    is-deeply @e[0].List, (4, 5, 6),
        'a native array takes its values from views';
    my int @f[3];
    @f = @m[1];
    is-deeply @f.List, (4, 5, 6),
        'a native array takes its values from a view of its shape';
}

{
    my @a[2;3] = (1,2,3),(4,5,6);
    my ($p, $q, $r) := @a[0];
    is-deeply ($p, $q, $r), (1, 2, 3),
        'a view destructures as the list of its elements';
    is-deeply (@a[0] < 5, @a[0] <=> @a[1]), (True, Order::Same),
        'a view compares as the number of its elements';
    is-deeply (@a[0].reverse.List, @a[0].rotate.List), ((3, 2, 1), (2, 3, 1)),
        'a view of one dimension takes reverse and rotate';
    my @c[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    throws-like { @c[0].reverse }, X::IllegalOnFixedDimensionArray,
        'a view of several dimensions refuses reverse';
    @c = @c[1], @c[0];
    is-deeply (@c[0;1;0], @c[1;0;1]), (7, 2),
        'assigning views of several dimensions of an array to it assigns the values they had';
    my @e[2;2] = @c[0];
    is-deeply @e[1].List, (7, 8),
        'an array takes its values from a view of several dimensions';
    @e = @e;
    is-deeply @e[1].List, (7, 8),
        'assigning a shaped array to itself keeps its values';
}

{
    my @t[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    my @u[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    is-deeply (@t[1] ~~ @t[1], ((5,6),(7,8)) ~~ @t[1], ((5,6),(7,9)) ~~ @t[1]),
      (True, True, False),
        'a view of several dimensions smartmatches as the list of its rows';
    my @w[2;2;2] = ((1,2),(3,4)),((5,6),(7,9));
    is-deeply (@t ~~ @u, @t ~~ @w), (True, False),
        'shaped arrays of several dimensions smartmatch as the lists of their rows';
    throws-like { my @a[2;2] = @t }, X::Assignment::ArrayShapeMismatch,
        'a shaped array of more dimensions is refused';
    my @o[2;3] = (1,2,3),(4,5,6);
    throws-like { my Int @b[3;3] = @o }, X::Assignment::ArrayShapeMismatch,
        'a shaped array of another type and shape is refused';
    throws-like { my int @c[3;3] = @t[0] }, X::Assignment::ArrayShapeMismatch,
        'a view of another shape is refused by a native array';
    my int @n[2;3];
    is-deeply (@n[1]:!delete).List, (0, 0, 0),
        'a delete adverb that is false on an index of the first dimension alone of a native array takes a view';
}

{
    my int @m[2;2] = (1,2),(3,4);
    @m = @m[1], @m[0];
    is-deeply (@m[0].List, @m[1].List), ((3, 4), (1, 2)),
        'assigning views of a native array to it assigns the values they had';
    my @p;
    @p[0][0] = 1;
    @p[0][2] = 3;
    is-deeply @p[0;0..*]:k, ((0, 0), (0, 2)),
        'a lazy index with an adverb takes the positions up to the end of the array';
    my @r[2] = (1,2),(3,4);
    my @c[2;2;2] = @r, @r;
    is @c[1;1;0], 3,
        'a shaped array of fewer dimensions gives its elements as the rows left';
    my @d = 3,;
    my @s := Array.new(:shape(@d), 1, 2, 3);
    my @v[2;3] = @s, @s;
    is @v[1;2], 3,
        'a row of a shape given as an array has that shape';
    my @a[2;2] = (1,2),(3,4);
    throws-like { my @f[3] = @a[0] }, X::Assignment::ArrayShapeMismatch,
        'a view of another length is refused by a shaped array of one dimension';
    throws-like { my @g[2;3]; @g = @a[0], @a[1] }, X::Assignment::ArrayShapeMismatch,
        'a row of another shape is refused';
    my @o[2;3] = (1,2,3),(4,5,6);
    my Int @b[2;3] = @o;
    my int @n[2;3] = @o;
    my @e[2;3] = @n;
    is-deeply (@b[1;2], @n[1;2], @e[1;2]), (6, 6, 6),
        'a shaped array takes its values from one of another type and its shape';
    my Int @t[2;2] = (1,2),(3,4);
    try @t = (5,'x'),(7,8);
    is-deeply @t[0;0], 1,
        'a refused assignment to a shaped array leaves its values';
    @t[0][1] = Nil;
    is-deeply @t[0;1], Int,
        'assigning Nil through a view of a typed array gives its default';
}

{
    my @a[2;3] = (1,2,3),(4,5,6);
    my $v = @a[1];
    @a[1;0] = 99;
    is $v[0], 99,
        'a view reads the elements the array has';
    @a = (7,8,9),(10,11,12);
    is-deeply $v.List, (10, 11, 12),
        'a view reads the values assigned to the array';
    nok @a[0][5]:exists,
        'an index past a dimension of a view does not exist';
    my @c[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    is-deeply (@c[1][*;0], @c[1].keys.head), ((5, 7), (0, 0)),
        'a view takes a multidimensional slice and gives lists of indices as keys';
    is-deeply (@c[*;1], @c[*;1]:k),
      ((3, 4, 7, 8), ((0, 1, 0), (0, 1, 1), (1, 1, 0), (1, 1, 1))),
        'a dimension left out of a multidimensional slice takes each of its indices';
}

{
    my @a[2;3] = (1,2,3),(4,5,6);
    for @a[1].kv -> $k, $v is rw { $v = $k }
    my int @n[2;3];
    for @n[1].kv -> $k, $v is rw { $v = 9 }
    is-deeply (@a[1].List, @n[1].List), ((0, 1, 2), (9, 9, 9)),
        'the values kv gives of a view are its elements to write to';
}

{
    my int @a[2;3] = (1,2,3),(4,5,6);
    @a = @a[1].hyper.map(*+0), @a[0].hyper.map(*+0);
    is-deeply (@a[0].List, @a[1].List), ((4, 5, 6), (1, 2, 3)),
        'assigning sequences that read a native array to it assigns the values they had';
}

# vim: expandtab shiftwidth=4
