use Test;
use nqp;

plan 17;

# Nothing can observe the result of a sunk start, so its code is sunk too.
# A loop whose value is wanted is a lazy Seq that would never run.
sub finished(Promise:D $done) {
    await Promise.anyof($done, Promise.in(5));
    $done.status ~~ Kept
}

# The message of whatever a sunk start in &code reports as uncaught.
sub reported(&code) {
    my $*SCHEDULER = ThreadPoolScheduler.new;
    my $reported = Promise.new;
    my $vow = $reported.vow;
    $*SCHEDULER.uncaught_handler = -> $ex { $vow.keep($ex.message) };
    code();
    finished($reported) ?? $reported.result !! Nil
}

sub failing() { fail 'boom' }

{
    my $done = Promise.new;
    my $n = 0;
    start while $n < 3 { $done.keep if ++$n == 3 }
    ok finished($done), 'a sunk start runs its while loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start until $n == 3 { $done.keep if ++$n == 3 }
    ok finished($done), 'a sunk start runs its until loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start repeat { $done.keep if ++$n == 3 } while $n < 3;
    ok finished($done), 'a sunk start runs its repeat while loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start repeat { $done.keep if ++$n == 3 } until $n == 3;
    ok finished($done), 'a sunk start runs its repeat until loop';
}

{
    my $done = Promise.new;
    start loop (my $i = 1; $i <= 3; $i++) { $done.keep if $i == 3 }
    ok finished($done), 'a sunk start runs its C-style loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start loop { $done.keep, last if ++$n == 3 }
    ok finished($done), 'a sunk start runs its bare loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start (++$n == 3 && $done.keep) while $n < 3;
    ok finished($done), 'a sunk start runs its while modifier loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start (++$n == 3 && $done.keep) until $n == 3;
    ok finished($done), 'a sunk start runs its until modifier loop';
}

{
    my $done = Promise.new;
    my $n = 0;
    start quietly until $n == 3 { $done.keep if ++$n == 3 }
    ok finished($done), 'a sunk start runs the loop of its quietly statement';
}

{
    my $done = Promise.new;
    my $n = 0;
    start do until $n == 3 { $done.keep if ++$n == 3 }
    ok finished($done), 'a sunk start runs the loop of its do statement';
}

{
    my $done = Promise.new;
    my $n = 0;
    start try until $n == 3 { $done.keep if ++$n == 3 }
    ok finished($done), 'a sunk start runs the loop of its try statement';
}

{
    my $result := (start failing()).result;
    isa-ok $result, Failure,
      'a start whose value is used keeps the Failure its statement returns';
    $result.so;
}

{
    my $result := await start { failing() };
    isa-ok $result, Failure,
      'a start whose value is used keeps the Failure its block returns';
    $result.so;
}

# The legacy frontend never sinks the code of a start.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    {
        my $done = Promise.new;
        start (1..3).map({ $done.keep if $_ == 3 });
        ok finished($done), 'a sunk start iterates the Seq its statement returns';
    }

    {
        my $done = Promise.new;
        start { (1..3).map({ $done.keep if $_ == 3 }) }
        ok finished($done), 'a sunk start iterates the Seq its block returns';
    }

    is reported({ start failing(); Nil }), 'boom',
      'a sunk start throws the Failure its statement returns';

    is reported({ start { failing() }; Nil }), 'boom',
      'a sunk start throws the Failure its block returns';
}
else {
    skip 'the legacy frontend does not sink the code of a start', 4;
}
