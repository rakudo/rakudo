use Test;
use nqp;

plan 5;

# A maybe-method call `$obj.?meth` resolves the named method through the
# invocant's metaclass and calls it, or yields Nil when it is absent.

is 42.?Str, '42', 'a maybe-method call invokes a method that exists';
ok (42.?no-such-method) =:= Nil, 'a maybe-method call yields Nil when the method is absent';

# It must not require the invocant to be a Mu-rooted object. A raw NQP object
# has no Raku `dispatch:<.?>` method, so a maybe-method call on one must still
# resolve through its metaclass rather than dying "dispatch:<.?> not found".
my $nqp-object := nqp::list(1, 2, 3);
lives-ok { $nqp-object.?elems }, 'a maybe-method call on a non-Mu object does not throw';
ok (try $nqp-object.?elems) =:= Nil,
    'a maybe-method call on a non-Mu object without the method yields Nil';

# A computed (non-identifier) method name still works.
my $name = 'Str';
is 42.?"$name"(), '42', 'a maybe-method call with a computed name still works';

# vim: expandtab shiftwidth=4
