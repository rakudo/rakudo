use v6;
use lib 'lib';
use Test;

plan 7;

sub check-fail (&test-to-run) {
    my $message = 'should fail; expected is not within precision';
    todo $message;
    nok test-to-run(), $message;
}

subtest 'two-arg version + optional description', {
    is-approx 0, 0,
    is-approx 5, 5 + 1e-6;
    is-approx 5, 5 - 1e-6;

    is-approx 0, 0,        'test desc zero';
    is-approx 5, 5 + 1e-6, 'test desc one';
    is-approx 5, 5 - 1e-6, 'test desc two';

    check-fail { is-approx 5, 6 };
    check-fail { is-approx 5, 6, 'test desc three' };
}

subtest 'tree-arg version + optional description', {
    is-approx 0, 0, 1;
    is-approx 5, 6, 1;
    is-approx 5, 10, 10;

    is-approx 0, 0,   1, 'test desc zero';
    is-approx 5, 6,   1, 'test desc one';
    is-approx 5, 10, 10, 'test desc two';

    check-fail { is-approx 5, 5 + 1e-6, 1e-10 };
    check-fail { is-approx 5, 5 - 1e-6, 1e-10, 'test desc three' };
}

subtest 'rel-tol version + optional description', {
    is-approx   0,   0, :rel-tol<.9>;
    is-approx 1e1, 1e2, :rel-tol<.9>;
    is-approx 1e2, 1e3, :rel-tol<.9>;
    is-approx 1e3, 1e4, :rel-tol<.9>;
    is-approx 1e3, 1e6, :rel-tol<.999>;

    is-approx   0,   0, :rel-tol<.9>,   'test desc zero';
    is-approx 1e1, 1e2, :rel-tol<.9>,   'test desc one';
    is-approx 1e2, 1e3, :rel-tol<.9>,   'test desc two';
    is-approx 1e3, 1e4, :rel-tol<.9>,   'test desc three';
    is-approx 1e3, 1e6, :rel-tol<.999>, 'test desc four';

    check-fail { is-approx 1e3, 1e6, :rel-tol<.9> };
    check-fail { is-approx 1e3, 1e6, :rel-tol<.9>, 'test desc five' };
}

subtest 'abs-tol version + optional description', {
    is-approx   0,   0, :abs-tol<9>;
    is-approx 1e0, 1e1, :abs-tol<9>;
    is-approx 1e2, 1e3, :abs-tol<900>;
    is-approx 1e3, 1e5, :abs-tol<99e3>;
    is-approx   1, 1.5, :abs-tol<1>;

    is-approx   0,   0, :abs-tol<9>,    'test desc one';
    is-approx 1e0, 1e1, :abs-tol<9>,    'test desc one';
    is-approx 1e2, 1e3, :abs-tol<900>,  'test desc two';
    is-approx 1e3, 1e5, :abs-tol<99e3>, 'test desc three';
    is-approx   1, 1.5, :abs-tol<1>,    'test desc four';

    check-fail { is-approx 1e3, 1e6, :abs-tol<10> };
    check-fail { is-approx   1, 1.5, :abs-tol<.4>, 'test desc five' };
}

subtest 'abs-tol + rel-tol version + optional description', {
    is-approx   0,   0, :abs-tol<9>,    :rel-tol<.9>;
    is-approx 1e0, 1e1, :abs-tol<9>,    :rel-tol<.9>;
    is-approx 1e2, 1e3, :abs-tol<900>,  :rel-tol<.9>;
    is-approx 1e3, 1e5, :abs-tol<99e3>, :rel-tol<.99>;
    is-approx   1, 1.5, :abs-tol<1>,    :rel-tol<.4>;

    is-approx   0,   0, :abs-tol<9>,    :rel-tol<.9>,  'test desc one';
    is-approx 1e0, 1e1, :abs-tol<9>,    :rel-tol<.9>,  'test desc one';
    is-approx 1e2, 1e3, :abs-tol<900>,  :rel-tol<.9>,  'test desc two';
    is-approx 1e3, 1e5, :abs-tol<99e3>, :rel-tol<.99>, 'test desc three';
    is-approx   1, 1.5, :abs-tol<1>,    :rel-tol<.4>,  'test desc four';

    check-fail {is-approx 1, 10, :abs-tol<5>,  :rel-tol<.9>; };
    check-fail {is-approx 1, 10, :abs-tol<90>, :rel-tol<.5>; };
    check-fail {is-approx 1, 10, :abs-tol<5>,  :rel-tol<.5>; };
    check-fail {
        is-approx 1, 10, :abs-tol<5>,  :rel-tol<.9>, 'test desc five';
    };
    check-fail {
        is-approx 1, 10, :abs-tol<90>, :rel-tol<.5>, 'test desc six';
    };
    check-fail {
        is-approx 1, 10, :abs-tol<5>, :rel-tol<.5>, 'test desc seven';
    };
}

subtest 'abs tol is correctly calculated', {
    is-approx 1, 2, :abs-tol<1>;
    check-fail { is-approx 1, 2 + 1e-10, :abs-tol<1> };

    is-approx 2, 1, :abs-tol<1>;
    check-fail { is-approx 2 + 1e-10, 1, :abs-tol<1> };

    is-approx -1, -2, :abs-tol<1>;
    check-fail { is-approx -1, -2 - 1e-10, :abs-tol<1> };

    is-approx -2, -1, :abs-tol<1>;
    check-fail { is-approx -2 - 1e-10, -1, :abs-tol<1> };
}

subtest 'rel tol is correctly calculated', {
    is-approx 1, 10, :rel-tol<.9>;
    check-fail { is-approx 1, 10 + 1e-10, :rel-tol<.9> };

    is-approx 10, 1, :rel-tol<.9>;
    check-fail { is-approx 10 + 1e-10, 1, :rel-tol<.9> };

    is-approx -1, -10, :rel-tol<.9>;
    check-fail { is-approx -1, -10 - 1e-10, :rel-tol<.9> };

    is-approx 10, 1, :rel-tol<.9>;
    check-fail { is-approx -10 - 1e-10, -1, :rel-tol<.9> };
}
