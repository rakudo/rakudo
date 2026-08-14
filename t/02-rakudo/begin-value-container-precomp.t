use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;

# A constant binds the container its initializer produces, and a
# precompiled module serializes that container, so a Proxy constant keeps
# calling its FETCH after a precompiled load. The legacy frontend loses
# the FETCH closure in serialization, so this coverage is gated to the
# RakuAST frontend.

plan 3;

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    require BeginValueContainer;
    my \prox = ::('BeginValueContainer::PROX');
    my \scal = ::('BeginValueContainer::SCAL');
    is prox.VAR.^name, 'Proxy', 'a precompiled Proxy constant binds the Proxy itself';
    my $first  = prox;
    my $second = prox;
    isnt $first, $second, 'each read of a precompiled Proxy constant calls its FETCH';
    is scal.VAR.^name, 'Scalar', 'a precompiled scalar constant keeps the container';
}
else {
    skip 'legacy serialization loses the Proxy FETCH closure', 3;
}

# vim: expandtab shiftwidth=4
