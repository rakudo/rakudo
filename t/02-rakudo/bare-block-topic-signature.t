use Test;

plan 11;

{
    my $sig = { $_ }.signature;
    is $sig.raku, ':(;; $_? is raw = OUTER::<$_>)',
        'bare block signature renders its implicit topic as raw with an outer default';
    my $param = $sig.params[0];
    ok $param.raw, 'the implicit topic parameter is raw';
    ok $param.optional, 'the implicit topic parameter is optional';
    is $param.type.^name, 'Mu', 'the implicit topic parameter is typed Mu';
}

# Trial binding invokes under the code object the signature belongs to, so
# these die if a block's signature has no code object bound.
{
    my $b = { $_ };
    lives-ok { $b.signature.ACCEPTS(\("x")) },
        'ACCEPTS on a bare block signature does not die';
    ok $b.signature.ACCEPTS(\("x")), 'a one-argument capture matches a bare block signature';
    ok $b.signature.ACCEPTS(\()), 'an empty capture matches a bare block signature';
    is $b.cando(\("x")).elems, 1, 'cando finds a bare block callable with one argument';
}

ok !({ $_ }.signature === { 42 }.signature),
    'two bare blocks do not share one signature object';

{
    my @a = 1, 2, 3;
    for @a { $_++ }
    is-deeply @a, [2, 3, 4], 'the implicit topic still aliases the iterated containers';
}

with 5 {
    ok &?BLOCK.signature.ACCEPTS(\(5)),
        'ACCEPTS works on the implicit topic signature of a with block';
}

# vim: expandtab shiftwidth=4
