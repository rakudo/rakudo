use Test;
use nqp;

plan 31;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

{
    my $s = do loop (my $i = 5; True;) { last if $i++ > 7; $i };
    is-deeply $s.head(10).List, (6, 7, 8),
        'the setup of a loop with a constant true condition giving a value runs';
}
{
    my $s = do loop (my $i = 5;;) { last if $i++ > 7; $i };
    is-deeply $s.List, (6, 7, 8),
        'the setup of a loop with no condition giving a value runs';
}
is-deeply (try EVAL(q[my $i = 0; (do loop (; ; $i++) { last if $i > 2; $i }).List])), (0, 1, 2),
    'a loop with an increment and no condition giving a value runs';
is-deeply (try EVAL(q[my $s = do loop (my $i = 0; ; $i++) { last if $i > 2; $i }; $s.List])), (0, 1, 2),
    'the setup of a loop with an increment and no condition giving a value runs';
{
    my $i = 0;
    while True { UNDO { }; last if ++$i > 2; 1 }
    is $i, 3, 'a while loop with a constant true condition and an UNDO phaser runs its body';
}
{
    my $i = 0;
    loop { UNDO { }; last if ++$i > 2 }
    is $i, 3, 'a loop with no condition and an UNDO phaser runs its body';
}
if $rakuast {
    is (try EVAL(q[my $n = 0; loop (my $i = 5; ; $i++) { UNDO { }; $n++; last if $i > 6 }; $n])), 3,
        'the setup of a loop with an increment, no condition and an UNDO phaser runs';
}
else {
    skip 'dies on the legacy frontend', 1;
}
{
    my $i = 0;
    my $seq;
    { $seq = do while 1 + 0 { last if $i++ > 2; $i }; CATCH { default { } } }
    ok $seq ~~ Seq:D && !$seq.is-lazy,
        'a folded true condition does not make a while loop giving a value lazy';
    is-deeply $seq.List, (1, 2, 3), 'a while loop with a folded true condition gives its values';
}
{
    my $i = 0;
    my $seq;
    { $seq = do loop (; 1 + 0;) { last if $i++ > 2; $i }; CATCH { default { } } }
    ok $seq ~~ Seq:D && !$seq.is-lazy,
        'a folded true condition does not make a loop giving a value lazy';
    is-deeply $seq.List, (1, 2, 3), 'a loop with a folded true condition gives its values';
}
{
    my $i = 0;
    my $seq;
    { $seq = do repeat { last if $i++ > 2; $i } while 1 + 0; CATCH { default { } } }
    ok $seq ~~ Seq:D && !$seq.is-lazy,
        'a folded true condition does not make a repeat loop giving a value lazy';
    is-deeply $seq.List, (1, 2, 3), 'a repeat loop with a folded true condition gives its values';
}
{
    my $i = 0;
    is-deeply (try (do until my $x = 5 { last if $i++ > 2; $i }).List), (),
        'a declaration as the condition of an until loop giving a value is tested';
}
{
    my $i = 0;
    is-deeply (try (do repeat { last if $i++ > 2; $i } until my $x = 5).List), (1,),
        'a declaration as the condition of a repeat until loop giving a value is tested';
}
{
    my $warned = 0;
    CONTROL { when CX::Warn { $warned++; .resume } }
    EVAL q[my @q = 1, 2, 3; my @r = do while my $x = @q.shift { $x }];
    is $warned, 0, 'a declaration as the condition of a while loop giving a value compiles without a warning';
}
is (try EVAL(q[my $n = 0; my $i = 0; my @r = do until my class C { $n++ } { last if $i++ > 2; $i }; $n])), 4,
    'a class declared as the condition of an until loop giving a value is evaluated each iteration';
is-deeply (try EVAL(q[my $i = 0; (do until my enum E <a b> { last if $i++ > 2; $i }).List])), (),
    'an enum declared as the condition of an until loop giving a value is tested';
{
    my $i = 0;
    is-deeply (try (do until my @a = 1 { last if $i++ > 2; $i }).List), (),
        'an array declaration as the condition of an until loop giving a value is tested';
}
{
    my $i = 0;
    ok (do while True { last if $i++ > 2; $i }).is-lazy,
        'a constant true condition makes a while loop giving a value lazy';
}
is-deeply (try EVAL(q[my $i = 0; (do while try 0 { last if $i++ > 2; $i }).List])), (),
    'a try as the condition of a while loop giving a value is tested';
is (try EVAL(q[my $i = 0; while try 0 { UNDO { }; last if ++$i > 2; 1 }; $i])), 0,
    'a try as the condition of a while loop with an UNDO phaser is tested';
is-deeply (try EVAL(q[my $i = 0; (do while gather { } { last if $i++ > 2; $i }).List])), (),
    'a gather as the condition of a while loop giving a value is tested';
is-deeply (try EVAL(q[my $i = 0; (do while INIT False { last if $i++ > 2; $i }).List])), (),
    'an INIT phaser as the condition of a while loop giving a value is tested';
is-deeply (try EVAL(q[my $i = 0; (do while (try 0) { last if $i++ > 2; $i }).List])), (),
    'a parenthesized try as the condition of a while loop giving a value is tested';
is-deeply (try EVAL(q[my $i = 0; (do while 2 { last if $i++ > 2; $i }).head(5).List])), (1, 2, 3),
    'a while loop giving a value with a true Int condition gives its values';
is-deeply (try EVAL(q[my $i = 0; (do while "abc" { last if $i++ > 2; $i }).head(5).List])), (1, 2, 3),
    'a while loop giving a value with a true Str condition gives its values';
is (try EVAL(q[my $i = 0; while 2 { UNDO { }; last if ++$i > 2; 1 }; $i])), 3,
    'a while loop with a true Int condition and an UNDO phaser runs its body';
is-deeply (try EVAL(q[my $i = 0; (do loop (; 2 ;) { last if $i++ > 2; $i }).head(5).List])), (1, 2, 3),
    'a loop giving a value with a true Int condition gives its values';
is-deeply (try EVAL(q[my $i = 0; (do repeat { last if $i++ > 2; $i } while 2).head(5).List])), (1, 2, 3),
    'a repeat loop giving a value with a true Int condition gives its values';
is (try EVAL(q[my $i = 0; repeat { UNDO { }; last if ++$i > 2; 1 } while 2; $i])), 3,
    'a repeat loop with a true Int condition and an UNDO phaser runs its body';
