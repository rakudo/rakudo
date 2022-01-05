use v6;
use lib <lib  t/packages t/02-rakudo/test-packages>;
use Test;
use Test::Helpers;
use CustomOps; # test cmp-ok handling custom infixes that we imported

plan 7;

sub check-fail (&test-to-run) {
    my $message = 'should fail due to requested comparison';
    todo $message;
    nok test-to-run(), $message;
}

group-of 13 => 'string comparators' => {
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

group-of 11 => '&[] comparators' => {
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

group-of 6 => 'custom operators (in code)' => {
    sub infix:<◀> { $^a < $^b };
    cmp-ok 1, &[◀], 2, 'comparing using a fancy operator (Callable version)';
    cmp-ok 1,  '◀', 2, 'comparing using a fancy operator (Str version)';
    check-fail { cmp-ok 2, &[◀], 1, 'failing comparison custom op (Callable)' }
    check-fail { cmp-ok 2,  '◀', 1, 'failing comparison custom op (Str)' }
}

group-of 6 => 'custom operators (in nested scope)' => {
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

group-of 24 => 'custom operators (imported)' => {
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

# https://github.com/Raku/old-issue-tracker/issues/5347
group-of 2 => 'no EVAL exploit (RT#128283)' => {
    check-fail { cmp-ok '', '~~>;exit; <z', '', '' };
}

group-of 2 => 'objects lacking support for methods' => {
    my class FooHOW does Metamodel::Naming {
        method new_type(::?CLASS:_: Str:D :$name! --> Mu) {
            my ::?CLASS:D $meta := self.new;
            my Mu         $obj  := Metamodel::Primitives.create_type: $meta, 'Uninstantiable';
            $meta.set_name: $obj, $name;
            $obj
        }
    }

    lives-ok {
        my Mu \Foo = FooHOW.new_type: :name<Foo>;
        cmp-ok Foo, &[=:=], Foo, 'can compare two objects without support for methods...';
    }, '...without throwing';
}

# vim: expandtab shiftwidth=4
