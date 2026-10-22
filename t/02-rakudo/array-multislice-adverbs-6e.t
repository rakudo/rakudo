use v6.e.PREVIEW;
use Test;
use nqp;

plan 20;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

my @a = [1, 2], [3, 4];
if $rakuast {
    throws-like { @a[1;0]:foo }, X::Adverb, unexpected => ('foo',), source => '@a',
        'a multidimensional array subscript refuses an unknown adverb';
    throws-like { @a[1;0]:exists:foo }, X::Adverb,
        unexpected => ('foo',), nogo => ('exists',),
        'a multidimensional array subscript refuses an unknown adverb with a known one';
    throws-like { @a[0..*; 0]:foo }, X::Adverb, unexpected => ('foo',),
        'a multidimensional array subscript with a lazy dimension refuses an unknown adverb';
    throws-like { @a[1;0]:foo = 5 }, X::Adverb, unexpected => ('foo',),
        'assigning through a multidimensional array subscript refuses an unknown adverb';
    throws-like { @a[1;0]:foo := 5 }, X::Adverb, unexpected => ('foo',),
        'binding through a multidimensional array subscript refuses an unknown adverb';
    my $evaluated = 0;
    try { @a[1;0]:foo := do { $evaluated++; 1 } }
    is $evaluated, 1,
        'binding through a multidimensional array subscript that refuses an adverb evaluates its source';
    my $at-begin = BEGIN { my @b = [1, 2], [3, 4]; (try @b[1;0]:foo) // $!.^name };
    is $at-begin, 'X::Adverb',
        'a multidimensional array subscript run at BEGIN time refuses an unknown adverb';
    throws-like { @a[||(1,0)]:foo }, X::Adverb, unexpected => ('foo',),
        'an array subscript that interpolates with || refuses an unknown adverb';
    throws-like { @a[||(1,0)]:foo = 5 }, X::Adverb, unexpected => ('foo',),
        'assigning through an array subscript that interpolates with || refuses an unknown adverb';
    throws-like { @a[1;0]:foo:bar }, X::Adverb, unexpected => <bar foo>,
        'a multidimensional array subscript refuses each unknown adverb';
}
else {
    skip 'the legacy frontend passes an unknown adverb on to the subscript', 10;
}

is EVAL('use v6.d; my @b = [1, 2], [3, 4]; @b[1;0]:foo'), 3,
    'a 6.d multidimensional array subscript is not checked for an unknown adverb';

my $v = 9;
@a[0;1]:BIND($v);
$v = 10;
is @a[0;1], 10,
    'a multidimensional array subscript takes :BIND as binding does';

{
    class Grid { }
    multi sub postcircumfix:<[; ]>(Grid \SELF, @indices, :$foo!) { "foo @indices[]" }
    is Grid.new[1;2]:foo, 'foo 1 2',
        'a multidimensional subscript candidate of its own takes the adverb it declares';
}

{
    my @c = [[1, 2], [3, 4]],;
    sub later() { @c[0;1]:bar }
    multi sub postcircumfix:<[; ]>(Array \SELF, @indices, :$bar!) { "bar @indices[]" }
    is later(), 'bar 0 1',
        'a multidimensional subscript takes an adverb that a candidate declared after it declares';
}

{
    my class Cell { }
    my $at-begin = BEGIN {
        multi sub postcircumfix:<[; ]>(Cell \SELF, @indices, :$foo!) { "foo @indices[]" }
        (try Cell.new[1;2]:foo) // $!.^name
    };
    is $at-begin, 'foo 1 2',
        'a multidimensional subscript run at BEGIN time takes an adverb its own candidate declares';
}

{
    my class Board { method corner { self[0;0]:wrap } }
    multi sub postcircumfix:<[; ]>(Board \SELF, @indices, :$wrap!) { "wrapped @indices[]" }
    my constant corner = Board.new.corner;
    is corner, 'wrapped 0 0',
        'a subscript run at BEGIN time takes an adverb that a candidate declared after it declares';
}

{
    my @w = [1, 2], [3, 4];
    my $handle = &postcircumfix:<[; ]>.wrap(-> |c { c<foo>:exists ?? 'wrapped' !! callsame });
    LEAVE &postcircumfix:<[; ]>.unwrap($handle) if $handle;
    is (try @w[1;0]:foo), 'wrapped',
        'a wrapped multidimensional subscript gets an adverb the setting would ignore';
    &postcircumfix:<[; ]>.unwrap($handle);
    $handle = Nil;
    if $rakuast {
        throws-like { @w[1;0]:foo }, X::Adverb,
            'a multidimensional subscript refuses an unknown adverb again once its wrapper is removed';
    }
    else {
        skip 'the legacy frontend passes an unknown adverb on to the subscript', 1;
    }
}

{
    my class Slot { }
    multi sub postcircumfix:<[; ]>(Slot \SELF, @indices) { 0 }
    my @s = [1, 2], [3, 4];
    my $v = 9;
    if $rakuast {
        @s[0;1]:foo := $v;
        $v = 10;
        is @s[0;1], 10,
            'binding through a multidimensional array subscript with a candidate of its own in scope binds the element';
    }
    else {
        skip 'the legacy frontend passes an unknown adverb on to the subscript', 1;
    }
}

{
    my @c = [1, 2], [3, 4];
    sub sees-match() {
        my &postcircumfix:<[; ]> := sub (\SELF, @indices, *%adverbs) is raw { CALLER::<$/> };
        "abc" ~~ /b/;
        @c[1;0]:foo
    }
    is ~sees-match(), 'b',
        'a subscript routine of its own given an adverb is called from the scope of the subscript';
}

# vim: expandtab shiftwidth=4
