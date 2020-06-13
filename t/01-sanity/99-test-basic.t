use v6;

use lib <lib>;
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

# skip-rest();

diag( 'diag works, FYI' );

todo( 'testing flunk', 1 );
flunk( 'flunk' );

{
    my $x = 3;
    my $isa1 = isa-ok( $x, Int, 'isa-ok with message' );
    ok $isa1, 'isa-ok returns True';
    isa-ok( $x, Int );

    # NOT_TODO
    todo( 'failing isa-ok returns False' );
    my $isa2 = isa-ok( 'abc', Int );
    nok $isa2, 'Failing isa-ok returns False';
}

my $dies-ok1 = dies-ok { skip( 2, 'reason' ) },
        'skip() dies when given the arguments in the wrong order';
ok $dies-ok1, 'dies-ok returns True';

# NOT_TODO
todo( 'failing dies-ok returns False' );
my $dies-ok2 = dies-ok { 1 }, 'dies-ok {1}';
nok $dies-ok2, 'dies-ok returns False if code did not die';

dies-ok { die }, 'dies-ok';
dies-ok { die };

my $lives_ok1 = lives-ok { 1 }, 'lives_ok';
ok $lives_ok1, 'lives-ok returns True';
lives-ok { 1 };

# NOT_TODO
todo( 'failing lives-ok returns False' );
my $lives-ok2 = lives-ok { die }, 'lives-ok { die }';
nok $lives-ok2, 'failing lives-ok returns False';

my $ed-ok1 = eval-dies-ok 'die', 'eval-dies-ok';
ok $ed-ok1, 'eavl-dies-ok returns True';
eval-dies-ok 'die';

# NOT_TODO
todo( 'eval-dies-ok 1 returns False' );
my $ed-ok2 = eval-dies-ok '1', 'eval-dies-ok 1 fails';
nok $ed-ok2, 'eval-dies-ok 1 returns False';

my $el-ok1 = eval-lives-ok '1', 'eval-lives-ok';
ok $el-ok1, 'eval-lives-ok 1 returns True';
eval-lives-ok '1';

# NOT_TODO
todo( 'failing eval-lives-ok returns False' );
my $el-ok2 = eval-lives-ok 'die', 'lives-ok { die }';
nok $el-ok2, 'failing eval-lives-ok returns False';

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
    my $is-deeply = is-deeply $deeply, $deeply, 'is-deeply';
    ok $is-deeply, 'is-deeply returns True';
    is-deeply $deeply, $deeply;
}

# NOT_TODO
todo( 'failing is-deeply returns False' );
my $is-deeply = is-deeply {a => 1}, {}, 'is-deeply with exta key fails';
nok $is-deeply, 'failing is-deeply returns False';

# Testing issue #3535
is Buf.new(42), Buf.new(42), "Comparing eq Buf";
lives-ok {
    # NOT_TODO
    todo("Comparing 2 not eq Buf, should not pass");
    is Buf.new(42), Buf.new(43);
}, "Comparing neq Buf";

done-testing;

# vim: expandtab shiftwidth=4
