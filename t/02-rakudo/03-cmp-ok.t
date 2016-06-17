use v6;
use lib 'lib';
use Test;

plan 4;

sub check-fail (&test-to-run) {
    my $message = 'should fail due to requested comparison';
    todo $message;
    nok test-to-run(), $message;
}

subtest 'string comparators', {
    cmp-ok 'foo', 'eq', 'foo';
    cmp-ok 1, '<', 2;

    cmp-ok 'foo', 'eq', 'foo', '"foo" eq "foo"';
    cmp-ok   'a', 'lt',   'b',   '"a" lt "b"';
    cmp-ok   'b', 'gt',   'a',   '"b" gt "a"';

    cmp-ok 1,  '<', 2, '1 < 2';
    cmp-ok 2, '>=', 1, '2 >= 1';

    check-fail { cmp-ok 2, '<', 1, '2 < 1' }
    check-fail { cmp-ok 'foo', 'eq', 'bar', '"foo" eq "bar"' }
    check-fail { cmp-ok 2, 'non-exisistant-op', 2 }
}

subtest '&[] comparators', {
    cmp-ok 'foo', &[eq], 'foo';
    cmp-ok 1, &[<], 2;

    cmp-ok 'foo', &[eq], 'foo', '"foo" eq "foo"';
    cmp-ok   'a', &[lt],   'b',   '"a" lt "b"';
    cmp-ok   'b', &[gt],   'a',   '"b" gt "a"';

    cmp-ok 1, &[<], 2, '1 < 2';
    cmp-ok 2, &[>=], 1, '2 >= 1';

    check-fail { cmp-ok 2, &[<], 1, '2 < 1' }
    check-fail { cmp-ok 'foo', &[eq], 'bar', '"foo" eq "bar"' }
}

subtest 'custom operators', {
    sub infix:<◀> { $^a < $^b };
    cmp-ok 1, &[◀], 2, 'comparing using a fancy operator';
    check-fail { cmp-ok 2, &[◀], 1, 'failing comparison using a fancy op' }
    check-fail { cmp-ok 1, '◀', 2 }
}

subtest 'no EVAL exploit (RT#128283)', {
    check-fail { cmp-ok '', '~~>;exit; <z', '', '' };
}
