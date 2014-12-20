use v6;
use Test;
# plan *;     # This does not test having a real plan.

pass( 'pass($desc)' );

my $ok1 = ok 1, 'ok with description';
ok $ok1, 'ok returns True';
my $ok2 = ok 1;
ok $ok2, 'ok returns True';

# NOT_TODO
# next is TODO only so our test script won't fail
# we are only testing the return value of &ok
todo( 'testing failure' );
my $ok3 = ok False, 'calling ok False';
nok $ok3, 'failure returns False';


my $nok1 = nok 0, 'nok with description';
ok $nok1, 'nok 0 returns True';
my $nok2 = nok 0;
ok $nok2, 'nok 0 returns True';

# NOT_TODO
todo( 'tesing nok True' );
my $nok3 = nok 1, 'nok 1 with description';
nok $nok3, 'nok 1 returns False';


my $is1 = is 1, 1, 'is with description';
ok $is1, 'is returns True';
is 1, 1;

# NOT_TODO
todo( 'failing is' );
my $is3 = is 1, 0, 'is 1, 0; with description';
nok $is3, 'is 1, 0;  returns False';


my $isnt1 = isnt 1, 0, 'isnt with description';
ok $isnt1, 'isnt 1, 0; returns True';
isnt 1, 0;

# NOT_TODO
todo( 'testing isnt 1,1' );
my $isnt3 = isnt 1, 1, 'isnt 1,1, with description';
nok $isnt3, 'isnt 1, 1; returns False';


my $approx1 = is_approx 1, 1, 'is_approx with description';
ok $approx1, 'is_approx 1,1, returns True';
my $approx2 = is_approx 1, 1;
my $approx3 = is_approx 1, 1.000001, 'is_approx with small difference';
ok $approx3, 'is_approx 1,1.000001, returns True';

# NOT_TODO
todo( 'failing is_approx 1,2;');
my $approx4 = is_approx 1, 2, 'is_approx with small difference';
nok $approx4, 'is_approx 1, 2; fails and returns False';


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
    my $isa1 = isa_ok( $x, Int, 'isa_ok with message' );
    ok $isa1, 'isa_ok returns True';
    isa_ok( $x, Int );

    # NOT_TODO
    todo( 'failing isa_ok returns False' );
    my $isa2 = isa_ok( 'abc', Int );
    nok $isa2, 'Failing isa_ok returns False';
}

my $dies_ok1 = dies_ok { skip( 2, 'reason' ) },
        'skip() dies when given the arguments in the wrong order';
ok $dies_ok1, 'dies_ok returns True';

# NOT_TODO
todo( 'failing dies_ok returns False' );
my $dies_ok2 = dies_ok { 1 }, 'dies_ok {1}';
nok $dies_ok2, 'dies_ok returns False if code did not die';

dies_ok { die }, 'dies_ok';
dies_ok { die };

my $lives_ok1 = lives_ok { 1 }, 'lives_ok';
ok $lives_ok1, 'lives_ok returns True';
lives_ok { 1 };

# NOT_TODO
todo( 'failing lives_ok returns False' );
my $lives_ok2 = lives_ok { die }, 'lives_ok { die }';
nok $lives_ok2, 'failing lives_ok returns False';

my $ed_ok1 = eval_dies_ok 'die', 'eval_dies_ok';
ok $ed_ok1, 'eavl_dies_ok returns True';
eval_dies_ok 'die';

# NOT_TODO
todo( 'eval_dies_ok 1 returns False' );
my $ed_ok2 = eval_dies_ok '1', 'eval_dies_ok 1 fails';
nok $ed_ok2, 'eval_dies_ok 1 returns False';

my $el_ok1 = eval_lives_ok '1', 'eval_lives_ok';
ok $el_ok1, 'eval_lives_ok 1 returns True';
eval_lives_ok '1';

# NOT_TODO
todo( 'failing eval_lives_ok returns False' );
my $el_ok2 = eval_lives_ok 'die', 'lives_ok { die }';
nok $el_ok2, 'failing eval_lives_ok returns False';

{
    my $deeply = {
        list  => (1, 2),
        hash  => { a => 1, b => 2 },
        str   => 'hello',
        num   => 1.2,
        int   => 33,
        pair  => :a(3),
#        undef => undef,
        bool  => Bool::True,
        array => [3, 4],
    };
    my $is_deeply = is_deeply $deeply, $deeply, 'is_deeply';
    ok $is_deeply, 'is_deeply returns True';
    is_deeply $deeply, $deeply;
}

# NOT_TODO
todo( 'failing is_deeply returns False' );
my $is_deeply = is_deeply {a => 1}, {}, 'is_deeply with exta key fails';
nok $is_deeply, 'failing is_deeply returns False';

done;

# vim: ft=perl6
