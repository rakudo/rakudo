use v6;
use lib <lib  t/02-rakudo/test-packages>;
use Test;
use CustomOps; # test cmp-ok handling custom infixes that we imported

plan 6;

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

subtest 'custom operators (in code)', {
    sub infix:<◀> { $^a < $^b };
    cmp-ok 1, &[◀], 2, 'comparing using a fancy operator (Callable version)';
    cmp-ok 1,  '◀', 2, 'comparing using a fancy operator (Str version)';
    check-fail { cmp-ok 2, &[◀], 1, 'failing comparison custom op (Callable)' }
    check-fail { cmp-ok 2,  '◀', 1, 'failing comparison custom op (Str)' }
}

subtest 'custom operators (in nested scope)', {
    sub infix:<◀> { $^a < $^b };
    {
        {
            cmp-ok 1, &[◀], 2, 'passing, Callable';
            cmp-ok 1,  '◀', 2, 'passing, Str';
            check-fail { cmp-ok 2, &[◀], 1, 'failing, Callable' }
            check-fail { cmp-ok 2,  '◀', 1, 'failing, Str'      }
        }
    }
}

subtest 'custom operators (imported)', {
    # Commented out tests fails due to some other bug regarding using
    # &[...] notation with imported ops with `<`/`>` in them, like `<=»`

    cmp-ok 1,  &[<=!], 2, 'passing <=! op, Callable';
    cmp-ok 1,   '<=!', 2, 'passing <=! op, Str';
    # cmp-ok 1,  &[<=»], 2, 'passing <=» op, Callable';
    cmp-ok 1,   '<=»', 2, 'passing <=» op, Str';
    cmp-ok 1,    &[«], 2, 'passing « op, Callable';
    cmp-ok 1,     '«', 2, 'passing « op, Str';
    # cmp-ok 1,   &[<«], 2, 'passing <« op, Callable';
    cmp-ok 1,    '<«', 2, 'passing <« op, Str';
    # cmp-ok 1,   &[>»], 2, 'passing >» op, Callable';
    cmp-ok 1,    '>»', 2, 'passing >» op, Str';
    # cmp-ok 1, &[<«>»], 2, 'passing <«>» op, Callable';
    cmp-ok 1,  '<«>»', 2, 'passing <«>» op, Str';

    check-fail { cmp-ok 2,  &[<=!], 1, 'failing <=! op, Callable'  }
    check-fail { cmp-ok 2,   '<=!', 1, 'failing <=! op, Str'       }
    # check-fail { cmp-ok 2,  &[<=»], 1, 'failing <=» op, Callable'  }
    check-fail { cmp-ok 2,   '<=»', 1, 'failing <=» op, Str'       }
    check-fail { cmp-ok 2,    &[«], 1, 'failing « op, Callable'    }
    check-fail { cmp-ok 2,     '«', 1, 'failing « op, Str'         }
    # check-fail { cmp-ok 2,   &[<«], 1, 'failing <« op, Callable'   }
    check-fail { cmp-ok 2,    '<«', 1, 'failing <« op, Str'        }
    # check-fail { cmp-ok 2,   &[>»], 1, 'failing >» op, Callable'   }
    check-fail { cmp-ok 2,    '>»', 1, 'failing >» op, Str'        }
    # check-fail { cmp-ok 2, &[<«>»], 1, 'failing <«>» op, Callable' }
    check-fail { cmp-ok 2,  '<«>»', 1, 'failing <«>» op, Str'      }
}

subtest 'no EVAL exploit (RT#128283)', {
    check-fail { cmp-ok '', '~~>;exit; <z', '', '' };
}

done-testing;
