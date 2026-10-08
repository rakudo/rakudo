use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use MONKEY-SEE-NO-EVAL;

plan 189;

# a set length for the first dimension
{
    my @j[2;*];
    is-deeply @j.shape, (2, *),
        'a shape that leaves the length of a dimension unset is the shape of the array';
    is @j.elems, 2,
        'an array of a set length for its first dimension has that many elements';
    @j[0;5] = 1;
    is @j[0;5], 1,
        'a dimension of no set length takes any index';
    is-deeply (@j[0].elems, @j[1].elems), (6, 0),
        'each element of the first dimension has its own length';
    push @j[1], 1, 2, 3;
    is-deeply @j[1].List, (1, 2, 3),
        'an element of the first dimension takes a push';
    throws-like { @j[2;0] }, Exception, message => /'Index 2 for dimension 1'/,
        'an index past the set length of its dimension is refused';
    throws-like { @j.push((1,)) }, X::IllegalOnFixedDimensionArray,
        'an array of a set length for its first dimension refuses a push';
    @j[0] = 7, 8;
    is-deeply @j[0].List, (7, 8),
        'assigning to an element of the first dimension assigns its values';
}

{
    my @j[2;*] = (1,2,3),(4,);
    is @j.gist, '[[1 2 3] [4]]',
        'an array of a dimension of no set length takes its values from lists';
    is @j.raku, 'Array.new(:shape(2, *), ([1, 2, 3], [4]))',
        'an array of a dimension of no set length gives its shape and values as its raku';
    ok EVAL(@j.raku) eqv @j,
        'the raku of an array of a dimension of no set length evaluates to its values';
    throws-like { my @k[2;*] = (1,),(2,),(3,) }, Exception, message => /'Index 2 for dimension 1'/,
        'an array of a set length for its first dimension refuses more values than that';
    is-deeply @j[*;0], (1, 4),
        'a whatever star takes each index of the first dimension';
    is-deeply @j[*;*], (1, 2, 3, 4),
        'whatever stars take each index of each element';
    is-deeply @j[*;*]:k, ((0, 0), (0, 1), (0, 2), (1, 0)),
        'a slice of an array of a dimension of no set length takes :k';
    is-deeply (@j[0;2]:exists, @j[1;2]:exists, @j[5;0]:exists), (True, False, False),
        'an element of an array of a dimension of no set length takes :exists';
    is @j[0;1]:delete, 2,
        'an element of an array of a dimension of no set length takes :delete';
    nok @j[0;1]:exists,
        'deleting an element of an array of a dimension of no set length deletes it';
    my $v = 1;
    @j[1;1] := $v;
    $v = 2;
    is @j[1;1], 2,
        'binding an element of an array of a dimension of no set length binds the container';
}

# no set length for the first dimension
{
    my @j[*;3];
    is-deeply (@j[4;1], @j.elems), (Any, 0),
        'reading an element of a row not made yet makes nothing';
    @j[4;1] = 7;
    is @j[4;1], 7,
        'assigning an element of a row not made yet makes the row';
    is @j.elems, 5,
        'a first dimension of no set length takes any index';
    is @j[4].elems, 3,
        'a row of a set length made on assignment has that length';
    throws-like { @j[0;3] = 1 }, Exception, message => /'Index 3 for dimension 2'/,
        'an index past the set length of a later dimension is refused';
    @j[6;0]++;
    is @j[6;0], 1,
        'an assignment operator on an element of a row not made yet makes the row';
    @j.push((1,2,3));
    is-deeply @j[7].List, (1, 2, 3),
        'a push onto a first dimension of no set length adds a row of the values';
}

{
    my @cal[12;*;24];
    @cal[1;42;8] = 'meeting';
    is @cal[1;42;8], 'meeting',
        'a dimension of no set length between two of set length takes any index';
    is-deeply (@cal[1].elems, @cal[1;42].elems), (43, 24),
        'the dimensions around one of no set length keep their lengths';
    throws-like { @cal[1;0;24] = 1 }, Exception, message => /'Index 24 for dimension 3'/,
        'an index past the set length of the last dimension is refused';
}

{
    my Int @t[2;*];
    @t[0;1] = 5;
    is-deeply (@t.of, @t[0;1], @t[0;0]), (Int, 5, Int),
        'a typed array of a dimension of no set length keeps its type';
    throws-like { @t[0;1] = 'x' }, X::TypeCheck::Assignment, expected => Int,
        'a typed array of a dimension of no set length checks its values';
}

{
    my int @n[42;*];
    push @n[41], 1, 2;
    is-deeply @n[41].List, (1, 2),
        'an element of a native array of a dimension of no set length takes a push';
    @n[0;3] = 5;
    is-deeply (@n.of, @n[0;3], @n[0;0]), (int, 5, 0),
        'a native array of a dimension of no set length keeps its native type';
}

is Array.new(:shape(2,*), [1,2], [3]).gist, '[[1 2] [3]]',
    'a new array of a dimension of no set length takes its values';

{
    my @j[2;*] = (1,),(2,3);
    is-deeply (@j[0]:exists, @j[0]:!exists), (True, False),
        'an element of the first dimension takes :exists';
    my @rows = [1,2],[3];
    my @k[2;*] = @rows;
    is-deeply @k[0].List, (1, 2),
        'an array of a dimension of no set length takes its values from arrays';
    my int @n[2;*] = @rows;
    is @n[0;1], 2,
        'a native array of a dimension of no set length takes its values from arrays';
    ok EVAL(@n.raku) eqv @n,
        'the raku of a native array of a dimension of no set length evaluates to its values';
    my @p[*;*];
    @p.push((1,2));
    ok EVAL(@p.raku) eqv @p,
        'the raku of an array of one row evaluates to that row';
    my @q[2;*];
    ok @q.WHAT =:= @j.WHAT,
        'arrays of the same shape are of the same type';
    my @c := @j.clone;
    @c[0;0] = 9;
    is @j[0;0], 1,
        'a copy of an array of a dimension of no set length has its own elements';
    is-deeply @j[0..2;0]:exists, (True, True, False),
        'a slice takes an index past a set length as one the array does not have';
    is-deeply @j[0..*;0], (1, 2),
        'a lazy index takes the indices within a set length';
    my @s[*;*] = (1,2),(5,6);
    @s.splice(1, 0, $(7,8));
    is-deeply @s.List.map(*.List), ((1, 2), (7, 8), (5, 6)),
        'a splice of a first dimension of no set length adds a row of the values';
    @s.splice(1, 1, ((3,4),(9,)));
    is-deeply @s.List.map(*.List), ((1, 2), (3, 4), (9,), (5, 6)),
        'a splice of a list of rows adds each of them';
}

{
    my @j[*;3];
    my $x := @j[4;1];
    $x = 7;
    is-deeply ($x, @j[4;1], @j.elems), (7, 7, 5),
        'an element of a row not made yet is the one assigned through it';
    my int @n[*;3];
    throws-like { @n[0][1] = 'x' }, Exception, message => /'native integer'/,
        'a cascaded assignment to a native row not made yet checks its native type';
    is @n.elems, 0,
        'a refused value for a native row not made yet makes nothing';
    @n[1][1] = 5;
    is-deeply (@n[1] ~~ array[int], @n[1;1]), (True, 5),
        'a cascaded assignment to a native row not made yet makes a native row';
    my Int @t[2;*];
    ok @t.default === Int,
        'a typed array of a dimension of no set length has the default of its type';
    throws-like { @t[0] := [1] }, X::TypeCheck::Binding,
        'binding an element of the first dimension checks the type of its array';
    my Int @r = 1, 2;
    @t[1] := @r;
    @r[0] = 3;
    is @t[1;0], 3,
        'binding an element of the first dimension binds the array given';
}

{
    my int @n[*;3];
    my $x := @n[0][1];
    $x = 5;
    $x = 6;
    is @n[0;1], 6,
        'an element of a native row not made yet is the one assigned through it';
    @n[2;0] = 1;
    is @n[1][1], 0,
        'an element of a native row not made yet reads as the native default';
    my @j[*;3];
    @j[1;0] = 5;
    is-deeply @j[0;*], (Any, Any, Any),
        'a whatever star takes the set length of a row not made yet';
    my @k[*;3] = (1,2,3),;
    my @c := @k.clone;
    @c[0;0] = 9;
    is @k[0;0], 1,
        'a copy of an array of rows of a set length has its own elements';
}

{
    throws-like { my @z[*;0] }, X::IllegalDimensionInShape,
        'a set length of 0 is refused';
    throws-like { my @z[*;-2] }, X::IllegalDimensionInShape,
        'a negative set length is refused';
    my @a[*;*];
    @a.append((5,6),(7,));
    is-deeply @a.List.map(*.List), ((5, 6), (7,)),
        'an append of several values adds a row of each';
    my @l[2;*] = (1,2) xx *;
    is-deeply @l.List.map(*.List), ((1, 2), (1, 2)),
        'lazy values fill a set length of the first dimension';
    throws-like { my @m[*;*] = (1,2) xx * }, X::Cannot::Lazy,
        'lazy values for a first dimension of no set length are refused';
    my Int @i[2;*];
    my @n := @i.new(:shape(3,*));
    @n[0;1] = 5;
    is-deeply (@n[0;1], @n[0].elems, @n.shape, @n.of), (5, 2, (3, *), Int),
        'a new array of another shape of a jagged one has arrays of its base type';
    my @e[2;*] = (1,2),(3,);
    my @f[2;*] = (1,2),(3,);
    my @g[2;*] = (1,2),(4,);
    is-deeply (@e eqv @f, @e eqv @g), (True, False),
        'arrays of a dimension of no set length are eqv when their values are';
}

# a row not made yet reads as the type object of a row, which makes one of
# its shape when written to through its container
{
    my @j[*;3];
    @j[0] = 1, 2, 3;
    ok @j[0].WHAT =:= @j[5].WHAT && !@j[5].defined,
        'a row not made yet is the type object of a row';
    my $read = @j[5][1];
    my $view = @j[5;*];
    is @j.elems, 1,
        'reading through a row not made yet makes nothing';
    @j[2][1] = 42;
    is-deeply (@j.elems, @j[2].List, @j[2].shape), (3, (Any, 42, Any), (3,)),
        'a cascaded assignment to a row not made yet makes it of its shape';
    @j[3][0] += 5;
    @j[3][0]++;
    is @j[3;0], 6,
        'an assignment operator through a row not made yet makes it';
    sub set-row($row is rw) { $row = $row.new(7, 8, 9) }
    set-row(@j[4]);
    is-deeply @j[4].List, (7, 8, 9),
        'a row not made yet is made through an rw parameter';
    my $c := @j[6];
    $c = (4, 5, 6);
    is-deeply @j[6].List, (4, 5, 6),
        'assigning values to the container of a row not made yet makes a row of them';
    my $v = 1;
    @j[7][2] := $v;
    $v = 2;
    is @j[7;2], 2,
        'binding an element of a row not made yet makes it';
}

{
    my @j[*;3];
    throws-like { @j[2][3] = 1 }, Exception, message => /'Index 3'/,
        'a cascaded assignment past the length of a row not made yet is refused';
    throws-like { @j[2].push(1) }, X::IllegalOnFixedDimensionArray,
        'a push onto a row of a set length not made yet is refused';
    is @j.elems, 0,
        'a refused write through a row not made yet makes nothing';
    my Int @t[*;2];
    throws-like { @t[1][0] = 'x' }, X::TypeCheck::Assignment,
        'a cascaded assignment to a typed row not made yet checks its type';
    is @t.elems, 0,
        'a refused value for a row not made yet makes nothing';
}

{
    my @p[*;*];
    @p[2].push(1, 2);
    @p[1].unshift(3);
    @p[0].append(4, 5);
    @p[3].prepend(6);
    is-deeply @p.List.map(*.List), ((4, 5), (3,), (1, 2), (6,)),
        'a push, unshift, append or prepend onto a row not made yet makes it';
    @p[4].push(1).push(2);
    is-deeply @p[4].List, (1, 2),
        'a push onto a row not made yet gives the row made';
    ok @p[4].WHAT =:= @p[9].WHAT,
        'a row of no set length is of the type of one not made yet';
}

{
    my @n[*;*;2];
    @n[1][3][0] = 'x';
    is-deeply (@n.elems, @n[1].elems, @n[1;3].List), (2, 4, ('x', Any)),
        'a cascaded assignment through rows not made yet makes each of them';
    my @s[*;2;2];
    @s[1][0][1] = 5;
    is-deeply @s[1;0].List, (Any, 5),
        'an assignment through a view of a shaped row not made yet makes it';
    is-deeply (@s[3;1].List, @s.elems), ((Any, Any), 2),
        'a view of a shaped row not made yet reads its defaults';
}

{
    my int @n[*;3];
    @n[2][1] = 7;
    is-deeply (@n[2].List, @n[2].WHAT =:= @n[0].WHAT, @n[0][1]), ((0, 7, 0), True, 0),
        'a native row not made yet is of the type of a row made';
    ok @n ~~ Positional[int],
        'a native array of a dimension of no set length is positional of its type';
    my int @p[*;*];
    @p[1].push(4);
    @p[1].append(5, 6);
    is-deeply @p[1].List, (4, 5, 6),
        'a push onto a native row of no set length not made yet makes it';
    my int @s[*;2;2];
    @s[1][0][1] = 3;
    is-deeply (@s[1;0;1], @s[1].shape), (3, (2, 2)),
        'an assignment through a shaped native row not made yet makes it';
}

{
    my @j[3;*];
    my @row := @j[0];
    @j[0] = 1, 2;
    is-deeply @row.List, (1, 2),
        'assigning to a row made assigns to that row';
    throws-like { @j.grab }, X::IllegalOnFixedDimensionArray,
        'an array of a set length for its first dimension refuses a grab';
    is-deeply (@j.new.shape, @j.WHAT.new.shape), ((3, *), (3, *)),
        'a new array of a jagged type has its shape';
    is-deeply @j[0].new(4, 5).List, (4, 5),
        'a new row of a row type takes its values';
    my int @x[3];
    my @y := @x.new(:shape(2,*));
    @y[0;1] = 3;
    is-deeply (@y[0].List, @y.of), ((0, 3), int),
        'a jagged array made from a shaped native one is of its native type';
    my @z[*;3];
    throws-like { @z[0] := Array.new(:shape(2)) }, X::ArrayShapeMismatch,
        action => 'bind', message => /'Cannot bind'/,
        'binding a row of another shape is refused as a bind';
    throws-like { @z[0] := 42 }, X::TypeCheck::Binding,
        'binding a value that is not a row is refused';
    fails-like { my $i = -1; @z[$i] }, X::OutOfRange,
        'a negative index fails as an array does';
}

{
    my @j[*;3];
    @j[2;0] = 1;
    my @k[*;3] = @j;
    ok @k eqv @j && @k[0].defined,
        'assigning an array with rows not made yet gives empty rows for them';
    @k[2;0] = 9;
    is @j[2;0], 1,
        'assigning an array of a dimension of no set length copies its rows';
    my int @n[*;3];
    @n[2;0] = 1;
    my int @m[*;3] = @n;
    is-deeply (@m[0].defined, @m[0].List), (True, (0, 0, 0)),
        'assigning a native array with rows not made yet gives empty rows for them';
    my @a[2;*];
    @a[0] := [1, 2];
    my @b[2;*];
    @b[0] = 1, 2;
    ok @a eqv @b,
        'a row bound to an array is eqv to one assigned the same values';
    my @c[2;*] = (1,),(2,);
    my @d[*;*] = (1,),(2,);
    my Int @e[2;*] = (1,),(2,);
    is-deeply (@c eqv @d, @c eqv @e), (False, False),
        'arrays of a dimension of no set length are not eqv when their shapes or types differ';
}

{
    my @f[2;*];
    throws-like { @f."$_"() }, X::IllegalOnFixedDimensionArray, operation => $_,
        "an array of a set length for its first dimension refuses $_" for <pop shift>;
    throws-like { @f."$_"((1,)) }, X::IllegalOnFixedDimensionArray, operation => $_,
        "an array of a set length for its first dimension refuses $_" for <unshift append prepend>;
    throws-like { @f.splice(0, 1) }, X::IllegalOnFixedDimensionArray,
        'an array of a set length for its first dimension refuses a splice';
    is @f.elems, 2,
        'a refused change keeps the set length of the first dimension';
    throws-like { @f[2] }, Exception, message => /'Index 2 for dimension 1'/,
        'a single index past a set first length is refused';
}

{
    my @c[*;3];
    @c[4;*-1] = 'z';
    is-deeply @c[4].List, (Any, Any, 'z'),
        'a Callable index takes the set length of a row not made yet';
    @c[6;*] = 1, 2, 3;
    is-deeply (@c[6].List, @c.elems), ((1, 2, 3), 7),
        'assigning through a whatever star makes a row not made yet';
    my Int @t[*;3];
    @t[2;0] = 1;
    ok @t.splice(0, 1).head.WHAT =:= @t[2].WHAT,
        'a typed array holds a row not made yet as the type of its rows';
}

{
    my @j[2;*] = (1,2),(3,);
    is-deeply (@j[1]:delete).List, (3,),
        'deleting an element of a set first dimension gives its row';
    is-deeply (@j.elems, @j[1].defined, @j[1].elems), (2, True, 0),
        'deleting an element of a set first dimension leaves an empty row';
    my @k[*;3] = (1,2,3),(4,5,6);
    is-deeply ((@k[1]:delete).List, @k.elems), ((4, 5, 6), 1),
        'deleting the last element of a first dimension of no set length removes it';
    my @r[*;3];
    is-deeply ((@r[5][1]:exists), (@r[5;1]:exists), @r.elems), (False, False, 0),
        ':exists through a row not made yet makes nothing';
    @r[5][1]:delete;
    @r[5;1]:delete;
    is @r.elems, 0,
        ':delete through a row not made yet makes nothing';
}

{
    my @j[*;3];
    my $v = 1;
    @j[2;1] := $v;
    $v = 2;
    is-deeply (@j[2;1], @j.elems), (2, 3),
        'binding an element of a row not made yet by its indices makes the row';
    my @r[3] = 1, 2, 3;
    @j[0] := @r;
    @r[0] = 9;
    is @j[0;0], 9,
        'binding a row of the shape left binds it';
    my int @n[*;*;*];
    my int @s[*;*] = (1,),;
    @n[0] := @s;
    is @n[0;0;0], 1,
        'a native jagged array binds as a row of a native jagged array';
}

{
    my @a[*;*] = (1,),;
    @a.unshift((2,),(3,));
    is-deeply @a.List.map(*.List), ((2,), (3,), (1,)),
        'an unshift of several values adds a row of each in order';
    is-deeply (@a.pop.List, @a.shift.List, @a.elems), ((1,), (2,), 1),
        'a first dimension of no set length takes a pop and a shift';
    my @p[3;*] = (1,2),;
    is-deeply (@p.elems, @p[2].defined, @p[2].elems), (3, True, 0),
        'fewer values than a set length leave empty rows';
}

{
    my @a[*;3];
    @a[1] = 1, 2, 3;
    @a[3,4] = @a[1], @a[1];
    @a[3;0] = 99;
    is-deeply (@a[1].List, @a[1] =:= @a[4]), ((1, 2, 3), False),
        'assigning a row to a slice of rows not made yet copies its values';
    my $c := @a[6];
    $c = @a[1];
    @a[6][0] = 98;
    is @a[1;0], 1,
        'assigning a row through the container of one not made yet copies it';
    my $i = -1;
    my @p[*;*];
    try @p[3][$i] = 5;
    is @p.elems, 0,
        'a write through a row not made yet that fails makes nothing';
    my $e := @a[8][0];
    $e = 1;
    @a[8;1] = 2;
    @a[8]:delete;
    $e = 9;
    is-deeply @a[8].List, (9, Any, Any),
        'an element kept from a row not made yet makes a new row each time';
}

{
    my @a[*;3];
    @a[2;0] = 1;
    .[1] = 5 for @a;
    is-deeply @a.List.map(*.List), ((Any, 5, Any), (Any, 5, Any), (1, 5, Any)),
        'iterating rows not made yet gives containers that make them';
    my @b[*;3];
    @b[2;0] = 1;
    $_ = (7, 8, 9) for @b.head(2);
    is-deeply @b[0,1].map(*.List), ((7, 8, 9), (7, 8, 9)),
        'assigning to an iterated row not made yet makes it';
    is-deeply (@a[5;1;0], @a[5][0;0]:exists, @a.elems), (Any, False, 3),
        'reading more indices than a row not made yet has makes nothing';
    my @s[*;*];
    @s[2] = 1, 2;
    @s.splice(3, 0, $(5,));
    nok @s[0]:exists,
        'a splice keeps a row not made yet as one';
    @a[1] := Array;
    nok @a[1].defined,
        'binding the type object of a row of a first dimension of no set length deletes it';
    my @f[2;*] = (1,),(2,);
    @f[0] := Array;
    is-deeply (@f[0].defined, @f[0].List), (True, ()),
        'binding the type object of a row of a set first length empties it';
}

{
    my @j[*;2;*];
    @j[3][1][4] = 7;
    @j[3][1].push(5);
    is-deeply (@j.elems, @j[3;1][^6], @j[5][1].defined), (4, (Any, Any, Any, Any, 7, 5), False),
        'writes through rows not made yet of a set length make each of them';
    my @k[2;*];
    @k[0,1] = (1,2),(3,);
    is-deeply @k.List.map(*.List), ((1, 2), (3,)),
        'a slice assignment to rows assigns their values';
    @k[*] = (4,),(5,6);
    is-deeply @k.List.map(*.List), ((4,), (5, 6)),
        'a whatever slice assignment to rows assigns their values';
    my @p[*;*];
    @p[2] = 1, 2;
    my @t[*;3];
    is-deeply (@p[0;*], @t[5][*], @t[5].elems, @t[5].defined), ((), (Any, Any, Any), 3, False),
        'the elements of a row not made yet read as those of a new row';
    is-deeply (@t[5].keys.List, @t[5].values.List, @t[5].kv.elems, @t[5].pairs.elems, @t[5].end),
      ((0, 1, 2), (Any, Any, Any), 6, 3, 2),
        'the keys and values of a row not made yet are those of a new row';
}

{
    my @j[2;*] = (1,2),(3,);
    is-deeply (@j[*;*;*], @j[0;0;*-1]), ((1, 2, 3), (1,)),
        'indices past the last dimension take the elements themselves';
    my @k[*;*];
    @k[0] = 1|2;
    @k.push(4|5);
    is-deeply (@k[0].elems, @k[0][0].^name, @k[1].elems, @k[1][0].^name),
      (1, 'Junction', 1, 'Junction'),
        'a junction assigned to a row is a value of it';
    my @b[*;*];
    my $a = [1,2];
    @b[0] := $a;
    $a = 42;
    is-deeply @b[0].List, (1, 2),
        'binding a container of a row binds the row';
    my @t[*;*];
    @t[4;0] = 1;
    @t.tail(2)[0] = (5, 6);
    $_ = (7,) unless .defined for @t.reverse;
    .push(3) for |@t;
    is-deeply @t.List.map(*.List), ((7, 3), (7, 3), (7, 3), (5, 6, 3), (1, 3)),
        'tail, reverse and a slip give rows not made yet as containers';
}

{
    my @k[*;3];
    @k[1;1] = 5;
    my @t[2;3] = @k;
    is-deeply @t[1;1], 5,
        'a shaped array takes its values from a jagged one with rows not made yet';
    my @s[2;3] = (1,2,3),(4,5,6);
    my @j[*;*] = @s;
    is-deeply @j.List.map(*.List), ((1, 2, 3), (4, 5, 6)),
        'a jagged array takes its rows from a shaped one';
    my @c[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    my @d[*;*;2] = @c;
    is-deeply @d[1;1].List, (7, 8),
        'a jagged array takes its rows from a shaped one of three dimensions';
}

{
    my @j[*;*];
    @j[4;0] = 1;
    my $i = 0;
    my $c := @j[$i];
    $i = 3;
    $c.push(9);
    is-deeply (@j[0].List, @j[3].defined), ((9,), False),
        'a row not made yet keeps the index it was read at';
    my @k[2;*] = (1,2),(3,);
    @k[0,1] = @k[1], @k[0];
    is-deeply @k.List.map(*.List), ((3,), (1, 2)),
        'a slice assignment of its own rows swaps them';
    try { @k[0,5] = (1,2),(3,4) }
    is-deeply @k.List.map(*.List), ((3,), (1, 2)),
        'a slice assignment past a set length assigns nothing';
    @k[0,1] = (5,6),;
    is-deeply @k.List.map(*.List), ((5, 6), ()),
        'a slice assignment of fewer values empties the rows left';
    my @c[*;3];
    @c[6][*-1] = 1;
    is-deeply @c[6].List, (Any, Any, 1),
        'a Callable index through a row not made yet takes its length';
    my @a[2;*];
    @a[0] = Any, 5;
    my @b[2;*];
    @b[0;1] = 5;
    ok @a eqv @b,
        'a row with an element not assigned is eqv to one assigned its default';
    my $n = -1;
    fails-like { @a[0;$n] }, X::OutOfRange,
        'a negative index of a dimension of no set length fails as an array does';
    throws-like { my @r[*;3]; my @row := @r[0]; @row[1] = 5 }, X::Assignment::RO,
        'a row not made yet without its container cannot be written to';
}

{
    my Int @t[2;*];
    @t[0;0] = 5;
    @t[0;0] = Nil;
    is-deeply @t[0;0], Int,
        'assigning Nil to an element of a typed jagged array gives its default';
    my @a[2;*;2];
    @a[0;1;1] = 5;
    my @b[2;*;2];
    @b[0;1;1] = 5;
    my @c[2;*;2];
    @c[0;1;1] = 6;
    is-deeply (@a eqv @b, @a eqv @c), (True, False),
        'jagged arrays of three dimensions are eqv when their values are';
    my @k[*;3] = (1,2,3),(4,5,6),(7,8,9);
    @k[1]:delete;
    is-deeply (@k.elems, @k[1].defined), (3, False),
        'deleting a middle row of no set length leaves it not made';
}

{
    my @s[2;3] = (1,2,3),(4,5,6);
    my @t[2;*];
    @t[1] = @s;
    @s[0;0] = 9;
    is-deeply @t[1].List, (1, 2, 3, 4, 5, 6),
        'assigning a shaped array to a row of one dimension assigns its values';
    my @k[*;3];
    @k[0] = 1, 2, 3;
    my $i = 0;
    @k[$i] = @k[0];
    is-deeply @k[0].List, (1, 2, 3),
        'assigning a row to itself keeps its values';
    my @c[*;3];
    @c[4][{ $_ - 1 }] = 'z';
    @c[6; -> $n { $n - 2 }] = 'y';
    is-deeply (@c[4].List, @c[6].List), ((Any, Any, 'z'), (Any, 'y', Any)),
        'a Block index takes the set length of a row not made yet';
    my @m[*;*];
    @m[3][4] = 7;
    my @p[*;*];
    @p[3;4] = 7;
    is-deeply (@m[3;0]:exists, @p[3;0]:exists), (False, False),
        'a cascaded write through a row not made yet makes the same row as a write by its indices';
}

{
    my @k[*;3];
    my int @n[*;3];
    is-deeply (@k[3].List, @k[3].list.List, @k[3].Seq.List, @n[3].List),
      ((Any, Any, Any), (Any, Any, Any), (Any, Any, Any), (0, 0, 0)),
        'a row not made yet lists as a new row';
    my @j[*;3];
    @j[0..2;0] = 1, 2, 3;
    is-deeply (@j.elems, @j[*;0]), (3, (1, 2, 3)),
        'a range over the first dimension makes the rows it writes to';
    my @r[*;*];
    @r[3;0] = 1;
    $_ = (5,) unless .defined for @r.rotate(1);
    is-deeply (@r.List.map(*.List).List, @r.tail.List), (((5,), (5,), (5,), (1,)), (1,)),
        'rotate gives rows not made yet as containers and tail the last row';
    my @h[*;*];
    @h[2] = 1, 2;
    is-deeply (@h.sort(*.elems).map(*.defined).List, @h.Array.map(*.defined).List, @h.combinations(2).elems),
      ((False, False, True), (False, False, True), 3),
        'list methods take a row not made yet as its type object';
    my int @d[*;3];
    my str @e[*;3];
    is-deeply (@d.default, @e.default), (0, ''),
        'a native jagged array has the default of its native type';
}

{
    my @a[*;*] = (1,),;
    @a.prepend((2,),(3,));
    is-deeply @a.List.map(*.List), ((2,), (3,), (1,)),
        'a prepend of several values adds a row of each in order';
    @a.append((1,2));
    is-deeply @a.List.map(*.List), ((2,), (3,), (1,), (1,), (2,)),
        'an append of a single list adds a row of each of its values';
    my @g[*;*] = (1,),;
    is-deeply @g.grab.List, (1,),
        'a first dimension of no set length takes a grab';
    my @s[*;*];
    @s[2] = 1, 2;
    is-deeply @s.splice(0, 2).map(*.defined), (False, False),
        'a splice gives rows not made yet as their type objects';
    throws-like { @s.append(1..*) }, X::Cannot::Lazy,
        'a lazy append is refused';
    throws-like { @s.prepend(1..*) }, X::Cannot::Lazy,
        'a lazy prepend is refused';
}

{
    my @cal[2;*;3];
    @cal[1;2;0] = 'x';
    my $days = 0;
    $days++ for @cal[1];
    is $days, 3,
        'iterating an element of the first dimension iterates its rows';
    my @a[*;*] = (1,),;
    my @b[*;*] = (1,),(2,);
    is-deeply (@a eqv @b, @b eqv @a), (False, False),
        'arrays of a dimension of no set length are not eqv when their numbers of rows differ';
    my @c[2;*;2];
    @c[0;0;0] = 1;
    @c[1;1;1] = 5;
    is-deeply (@c[*;0], @c[1;*]), ((1, Any, Any, Any), (Any, Any, Any, 5)),
        'a dimension left out of a slice takes each of its indices';
    my int @n[2;3] = (1,2,3),(4,5,6);
    my @v[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
    my @d[*;*] = @n;
    my @e[*;*] = @v[1];
    is-deeply (@d.List.map(*.List).List, @e.List.map(*.List).List),
      (((1, 2, 3), (4, 5, 6)), ((5, 6), (7, 8))),
        'a jagged array takes its rows from a native shaped array or a view of one';
}

{
    sub failed { fail 'nope' }
    my $failure = failed;
    my @m[*;*];
    @m[1;0] = $failure;
    @m[3][0] = $failure;
    @m[2;0] := failed;
    is-deeply (@m.elems, @m[1;0].^name, @m[3;0].^name, @m[2;0].^name),
      (4, 'Failure', 'Failure', 'Failure'),
        'a Failure written through a row not made yet is stored';
    my @src = [1,2],[3,4];
    my @j[*;*];
    @j[$_] = @src[$_] for ^2;
    @j[0][0] = 99;
    is-deeply (@j.List.map(*.List).List, @src[0].List), (((99, 2), (3, 4)), (1, 2)),
        'assigning an item of an array to a row assigns its values';
    my @s[2;3] = (1,2,3),(4,5,6);
    my @b[*;*];
    @b.append(@s);
    my @p[*;*];
    @p.prepend(@s);
    is-deeply (@b.List.map(*.List).List, @p.List.map(*.List).List),
      (((1, 2, 3), (4, 5, 6)), ((1, 2, 3), (4, 5, 6))),
        'an append or prepend of a shaped array adds a row of each of its rows';
}

{
    sub failed { fail 'nope' }
    my $failure = failed;
    $failure.so;
    my @m[*;3];
    @m[1;0] = $failure;
    my @d[*;3];
    @d[1][0] = $failure;
    is-deeply (@m.elems, @d.elems, @m[1;0].^name), (2, 2, 'Failure'),
        'a Failure written to a row of a set length not made yet is stored';
    my %h = a => 1, b => 2;
    my @j[*;*];
    @j.append(%h);
    @j.prepend(%h);
    is @j.elems, 4,
        'an append or prepend of a hash adds a row of each of its pairs';
    my @src[*;3];
    @src[2] = 1, 2, 3;
    my @dst[*;3];
    my @row := @src[0];
    @dst.append(@row);
    is @dst.elems, 3,
        'an append of a row not made yet appends the values of a new row';
    my @k[*;3];
    @k[1;0] = 1;
    my @a[*;*] = @k;
    is-deeply @a[0].List, (Any, Any, Any),
        'a row not made yet of a set length gives the values of a new row';
}

{
    my @k[*;3];
    $_ = 1 for @k[1].values;
    for @k[2].kv -> $i, $v is rw { $v = $i }
    .value = 4 for @k[3].pairs;
    my int @n[*;2];
    $_ = 5 for @n[0].list;
    is-deeply (@k[1].List, @k[2].List, @k[3].List, @n[0].List),
      ((1, 1, 1), (0, 1, 2), (4, 4, 4), (5, 5)),
        'writing to an element iterated from a row not made yet makes the row';
    my @j[*;*];
    @j[0;0] = @j;
    ok @j eqv @j,
        'a jagged array that holds itself is eqv to itself';
}

{
    my @j[*;2;2];
    is-deeply (@j[0].kv.head(4).List, @j[0].pairs.head.key, @j[0].keys.head),
      (((0, 0), Any, (0, 1), Any), (0, 0), (0, 0)),
        'the keys of a row not made yet of several dimensions are those of a new row';
    my @a[*;*];
    my @b[*;*];
    my $p := Proxy.new(
      FETCH => -> $ { @a[0] },
      STORE => -> $, \v { @a[0] = v; @b[0] = v }
    );
    $p.push(1);
    @a[0].push(2);
    is-deeply (@b[0].List, @a[0] =:= @b[0]), ((1,), False),
        'a row made through a container is a copy anywhere else it is assigned';
    my $lib = make-temp-dir;
    $lib.add('JaggedConstant.rakumod').spurt: q:to/MODULE/;
        unit module JaggedConstant;
        our constant J is export = do { my @j[*;*;*]; @j };
        MODULE
    my $made = EVAL qq:to/CODE/;
        use lib '$lib.absolute()';
        use JaggedConstant;
        my \\j = J.clone;
        j[0;0;0] = 1;
        j[0;0].List
        CODE
    is-deeply $made, (1,),
        'a jagged array made while precompiling makes its rows';
}

{
    my int @n[*;2;2];
    @n[1][1][0] = 3;
    @n[1][1][1] = 4;
    is-deeply @n[1][1].List, (3, 4),
        'a cascaded write to a native row of several dimensions works once it is made';
}

{
    my @j[*;3];
    @j[1;1] = 5;
    my @m[3] = @j[0];
    my @k[*;2;3];
    @k[1;1;1] = 5;
    my @n[2;3] = @k[0];
    my int @q[*;3];
    @q[1;0] = 1;
    my int @w[3] = @q[0];
    is-deeply (@m.List, @n[1].List, @w.List), ((Any, Any, Any), (Any, Any, Any), (0, 0, 0)),
        'a row not made yet assigned to a shaped array gives the values of a new row';
    my @d = 3,;
    @j[0] := Array.new(:shape(@d), 1, 2, 3);
    is @j[0;2], 3,
        'a row of a shape given as an array binds as a row of that shape';
}

{
    my int @j[*;3];
    @j[1;0] = 5;
    my int @r[2;3];
    @r = @j[0], @j[1];
    is-deeply (@r[0].List, @r[1].List), ((0, 0, 0), (5, 0, 0)),
        'a native shaped array takes a row not made yet in a list as a new row';
}

{
    my @y[*;*];
    @y[1] = 1, 2;
    my int @s[2;2] = @y[0], @y[1];
    is-deeply (@s[0].List, @s[1].List), ((0, 0), (1, 2)),
        'a native shaped array takes a row not made yet of an array of objects as a new row';
}

# vim: expandtab shiftwidth=4
