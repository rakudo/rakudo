use Test;

plan 6;

# Hypering a postfix should work for the same postfixes the legacy frontend
# accepts, not just method calls and postcircumfix indexing.

# The power postfix hypers element-wise.
is ((1, 2, 3)>>²).join(','), '1,4,9',
    'a hyper power postfix applies element-wise';

# A meta-method acts on the meta-object, so the hyper applies it once to the
# operand rather than distributing (matching the legacy frontend).
my @a = 1, 'x', 3.0;
is @a>>.^name, 'Array', 'a hyper meta-method call acts on the operand itself';
is (Int)>>.^name, 'Int', 'a hyper meta-method call on a type object resolves';

# A simple (unqualified) private method hypers.
class WithPriv {
    method !twice { 42 }
    method run { (self, self)>>!twice }
}
is WithPriv.run.join(','), '42,42',
    'a hyper unqualified private method call applies element-wise';

# Forms that already worked keep working.
is ((1, 2, 3)>>.succ).join(','), '2,3,4',
    'a hyper method call applies element-wise';
my @nested = [1, 2], [3, 4];
is (@nested>>[0]).join(','), '1,3',
    'a hyper postcircumfix index applies element-wise';

# vim: expandtab shiftwidth=4
