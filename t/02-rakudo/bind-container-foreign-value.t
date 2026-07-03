use Test;
use nqp;

plan 5;

# A foreign value, like one an nqp op produced, is mapped to its Raku type
# when passed bare to a routine. One sitting inside a Scalar container gets
# the same mapping when the parameter binds the value: without it the type
# check saw the raw foreign value and died "expected Any but got BOOTHash".
{
    my $h = nqp::hash('key', 'value');
    my sub take-hash($x) { $x }
    is take-hash($h).WHAT.^name, 'Hash',
        'a container-held nqp hash binds a value parameter as Hash';
    is take-hash($h)<key>, 'value',
        'the mapped hash still holds its entries';
}

{
    my $l = nqp::list('x', 'y');
    my sub take-list($x) { $x }
    is take-list($l).elems, 2,
        'a container-held nqp list binds a value parameter';
}

# A raw parameter binds the container itself, so no mapping applies.
{
    my $h = nqp::hash('key', 'value');
    my sub take-raw($x is raw) {
        nqp::how_nd(nqp::decont($x)).name(nqp::decont($x))
    }
    is take-raw($h), 'BOOTHash',
        'a raw parameter still receives the unmapped foreign value';
}

# A bound (containerless) foreign value keeps working as before.
{
    my $h := nqp::hash('key', 'value');
    my sub take-bound($x) { $x<key> }
    is take-bound($h), 'value',
        'a bare foreign value still binds and maps';
}

# vim: expandtab shiftwidth=4
