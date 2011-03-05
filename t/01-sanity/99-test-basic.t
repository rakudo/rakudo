use v6;
use Test;
# plan *;     # This does not test having a real plan.

pass( 'pass($desc)' );

ok 1, 'ok with description';
ok 1;

nok 0, 'nok with description';
nok 0;

is 1, 1, 'is with description';
is 1, 1;

isnt 1, 0, 'isnt with description';
isnt 1, 0;

is_approx 1, 1, 'is_approx with description';
is_approx 1, 1;
# is_approx 1, 1.000001, 'is_approx with small difference';

todo( 'testing todo twice', 2 );
ok 0, 'this should fail, to test todo()';
ok 0, 'this should also fail, to test todo()';
ok 1, 'passing test (todo is done)';

todo( 'todo with no count' );
ok 0, 'todo with no count covers one test';
ok 1, 'passing test (not todo)';

skip( 'skip with reason' );
skip;
skip( 'skip with count and reason', 2 );

# skip_rest();

diag( 'diag works, FYI' );

todo( 'testing flunk', 1 );
flunk( 'flunk' );

{
    my $x = 3;
    isa_ok( $x, Int, 'isa_ok with message' );
    isa_ok( $x, Int );
}

dies_ok { skip( 2, 'reason' ) },
        'skip() dies when given the arguments in the wrong order';

# dies_ok { die }, 'dies_ok';
# dies_ok { die };
#
# lives_ok { 1 }, 'lives_ok';
# lives_ok { 1 };
#
# eval_dies_ok 'die', 'eval_dies_ok';
# eval_dies_ok 'die';
#
# eval_lives_ok '1', 'eval_lives_ok';
# eval_lives_ok '1';
#
# {
#     my $deeply = {
#         list  => (1, 2),
#         hash  => { a => 1, b => 2 },
#         str   => 'hello',
#         num   => 1.2,
#         int   => 33,
#         pair  => :a(3),
#         undef => undef,
#         bool  => Bool::True,
#         array => [3, 4],
#     };
#     is_deeply $deeply, $deeply, 'is_deeply';
#     is_deeply $deeply, $deeply;
# }

done;

# vim: ft=perl6
