use Test;

plan 142;

# Reductions that already have their answer must not keep walking the values.
# A short-circuit operator has one as soon as its result settles, and a Range
# that goes on producing values has one for the operators that know it.

# --- folding that reaches the end -------------------------------------------

is ([||] 0, 0, 5, 7), 5, '[||] returns the first true value';
is ([||] Nil, 0, ""), "", '[||] with no true value returns the last one';
is ([||] 5), 5, '[||] on a single value returns that value';
is-deeply ([||]), False, '[||] with no values returns False';

is ([&&] 1, 2, 3), 3, '[&&] with no false value returns the last one';
is ([&&] 1, 0, 3), 0, '[&&] returns the first false value';
is-deeply ([&&]), True, '[&&] with no values returns True';

is ([//] Nil, 7, 9), 7, '[//] returns the first defined value';
is-deeply ([//]), Any, '[//] with no values returns Any';

is ([+] 1..5), 15, '[+] on a bounded Range sums its values';
is ([+] 1, 2, 3), 6, '[+] on separate values sums them';
is ([+] ()), 0, '[+] with no values returns the identity';
is ([+] (7,)), 7, '[+] on a sole List of one value returns that value';
is ([+] (3, 4)), 7, '[+] on a sole List of two values sums them';
is ([+] (3, 4, 5)), 12, '[+] on a sole List of three values sums them';
{
    my @two = 3, 4;
    is ([+] @two), 7, '[+] on an Array of two values sums them';
}
{
    my @counted = 3, 4, 5;
    @counted.elems;
    is ([+] @counted), 12, '[+] on an Array of values it has counted sums them';
}
is ([min] (3, 1, 2)), 1, '[min] on a sole List returns the smallest';
is ([max] (3, 1, 2)), 3, '[max] on a sole List returns the largest';
is-deeply ([min] (1, 1.0)), 1.0, '[min] on a sole List of a pair applies the operator to a pair';
{
    my @spoken = 1, 2, 3;
    @spoken.elems;
    @spoken does role { method list() { (100, 200) } };
    is ([+] @spoken), 300, '[+] on a list that answers list for itself takes that answer';
    is ([min] @spoken), 100, '[min] on a list that answers list for itself takes that answer';
    is ([max] @spoken), 200, '[max] on a list that answers list for itself takes that answer';
}
{
    my @pair = 5, 6;
    @pair.elems;
    @pair does role { method list() { (100, 200) } };
    is ([min] @pair), 100, '[min] on a pair answering list for itself takes that answer';
    is ([max] @pair), 200, '[max] on a pair answering list for itself takes that answer';
}

# A list holding values it has yet to produce keeps only some of them in its
# buffer, so reading that buffer alone would answer from a part of the list.
{
    my \partial = (gather { take 5; take 3; take 9 }).List;
    partial[0];
    is ([+] partial), 17, '[+] on a list still producing values sums the ones it has yet to produce';
}
{
    my \pending = (1..70000).reverse.map({ $_ }).List;
    pending[0];
    is ([min] pending), 1, '[min] on a list still producing values reaches the ones it has yet to produce';
}
{
    my \pending = (1..70000).map({ $_ }).List;
    pending[0];
    is ([max] pending), 70000, '[max] on a list still producing values reaches the ones it has yet to produce';
}

# A Slip stands for the values it holds once it reaches an argument list, so a
# list holding one goes to the operator spread however long it is.
{
    my @three = 1, 2, 3;   @three[0] = (7, 1).Slip;
    is ([min] @three), 1, '[min] on three values holding a Slip reads the values it holds';
    is ([max] @three), 7, '[max] on three values holding a Slip reads the values it holds';
}
{
    my @empty = 1, 2, 3;
    @empty[0] = Empty;
    is ([min] @empty), 2, '[min] on values holding a Slip of nothing passes it over';
}
{
    my @counted = 1, 2, 3;
    @counted.elems;
    @counted[2] = (0, 9).Slip;
    is ([min] @counted), 0,
      '[min] on counted values holding a Slip after the first reads the values it holds';
    is ([max] @counted), 9,
      '[max] on counted values holding a Slip after the first reads the values it holds';
}
{
    my @shaped[3] = 1, 2, 3;
    @shaped[0] = (7, 1).Slip;
    is ([min] @shaped), 1, '[min] on a shaped array holding a Slip reads the values it holds';
}

# A Slip is spread however many values are alongside it, so more of them than
# an argument list carries reports that rather than counting the Slip as one.
{
    my @big[70000];
    @big = 1..70000;
    @big[500] = (99999, 4).Slip;
    dies-ok { [max] @big },
      '[max] on more values than an argument list holds spreads a Slip among them';
}
{
    my @slipped = 1, 2, 3;
    @slipped[0] = (7, 1).Slip;
    is ([min] @slipped.Seq), 1, '[min] on pulled values holding a Slip reads the values it holds';
}
{
    my @slipped = 1, 2, 3;
    @slipped[0] = (7, 1).Slip;
    is ([max] @slipped.Seq), 7, '[max] on pulled values holding a Slip reads the values it holds';
}

{
    my $itemized = (30, 40);
    is-deeply ([+] $itemized), 2, '[+] on an itemized List counts it as one value';
    is-deeply ([min] $itemized), (30, 40), '[min] on an itemized List returns that List';
    is-deeply ([max] $itemized), (30, 40), '[max] on an itemized List returns that List';
}
{
    my $itemized = [30, 40];
    is-deeply ([+] $itemized), 2, '[+] on an itemized Array counts it as one value';
}

# A hole in a list warns under the name it has there, which pulling the
# values gives it.
{
    my @holed;
    @holed[3] = 5;
    my @warnings;
    my $sum = do {
        CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
        [+] @holed;
    };
    is $sum, 5, '[+] on an Array with holes sums the values it does hold';
    like @warnings[0], /'@holed[0]'/,
      '[+] on an Array with holes warns under the name a hole has in its list';
}
{
    my @gapped = 1, 2;
    @gapped[4] = 5;
    @gapped.elems;
    my @warnings;
    my $sum = do {
        CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
        [+] @gapped;
    };
    is $sum, 8, '[+] on an Array whose holes follow its values sums the values it holds';
    like @warnings[0], /'@gapped[2]'/,
      '[+] on an Array whose holes follow its values names the first of them';
}

# Summing a list can run code that shortens that same list, and the slots
# summed are the ones it held when the summing started.  Asking for the count
# first puts the values where that reading of them applies.
{
    my @shrinking;
    my class Shrinker { method Numeric() { @shrinking.pop; @shrinking.pop; 1 } }
    @shrinking = Shrinker.new, Shrinker.new, 5, 6, 7;
    @shrinking.elems;
    is ([+] @shrinking), 20, '[+] on an Array that shortens as it sums holds on to its values';
}

{
    my @none = ();
    is ([+] @none), 0, '[+] on an Array of no values returns the identity';
}
is ([min] ()), Inf, '[min] on a sole List of no values returns the identity of min';
is ([max] ()), -Inf, '[max] on a sole List of no values returns the identity of max';
is ([min] (7,)), 7, '[min] on a sole List of one value returns that value';
is ([max] (7,)), 7, '[max] on a sole List of one value returns that value';
is ([min] 3, 1, 2), 1, '[min] on separate values returns the smallest';
is ([max] 3, 1, 2), 3, '[max] on separate values returns the largest';
is ([min] 1..5), 1, '[min] on a bounded Range returns the smallest';
is ([max] 1..5), 5, '[max] on a bounded Range returns the largest';
is ([max] my @long = ^100000), 99999, '[max] on more values than an argument list holds works';

# More values than an argument list holds are handed to the operator whole.
is ([min] 1..100000), 1, '[min] on more values than an argument list holds works';
is ([max] 1..100000), 100000, '[max] on more values than an argument list holds works';
is ([min] my @big = ^100000), 0, '[min] on a long Array works';
dies-ok { [min] (1..70000).Seq },
  '[min] on more values than an argument list holds pulled one at a time reports that limit';
dies-ok { [max] (1..70000).Seq },
  '[max] on more values than an argument list holds pulled one at a time reports that limit';
is ([min] (3, 1, 2).lazy), 1, '[min] on a lazy list works';
is ([max] (3, 1, 2).lazy), 3, '[max] on a lazy list works';

# Two values reach the candidates the operator declares for a pair, which answer
# a tie with the second.  More reach the candidate taking them all, which answers
# a tie with the first.
is-deeply ([min] 1, 1.0), 1.0, '[min] on a pair applies the operator to a pair';
is-deeply ([max] 1, 1.0), 1.0, '[max] on a pair applies the operator to a pair';
is-deeply ([min] 1, 1.0, 2), 1, '[min] on three values hands them over at once';
is-deeply ([max] 1, 1.0, 0), 1, '[max] on three values hands them over at once';

# --- operators of the same name declared in scope ---------------------------

# The answers above belong to the setting's operators, which are matched by
# address.
# An operator declared in scope must fold like any other.
{
    my sub infix:<+>(\a, \b) { a * b }
    is ([+] 1..4), 24, '[+] on a + declared in scope folds with that operator';
}
{
    my sub infix:<||>(\a, \b) { a + b }
    is ([||] 1, 2, 3), 6, '[||] on a || declared in scope folds with that operator';
}
{
    my sub infix:<&&>(\a, \b) { a - b }
    is ([&&] 1, 1, 5), -5, '[&&] on an && declared in scope folds with that operator';
}
{
    my sub infix:<//>(\a, \b) { a + b }
    is ([//] 1, 2, 3), 6, '[//] on a // declared in scope folds with that operator';
}
{
    my sub infix:<or>(\a, \b) { a + b }
    is ([or] 1, 2, 3), 6, '[or] on an or declared in scope folds with that operator';
}
{
    my sub infix:<and>(\a, \b) { a - b }
    is ([and] 1, 1, 5), -5, '[and] on an and declared in scope folds with that operator';
}
# A list-associative reduction hands every value over at once, so these need
# to accept them all.
{
    my sub infix:<min>(|c) { c.elems }
    is ([min] 1, 2, 3), 3, '[min] on a min declared in scope hands it the values apart';
}
{
    my sub infix:<max>(|c) { c.elems }
    is ([max] 1, 2, 3), 3, '[max] on a max declared in scope hands it the values apart';
}

# --- values the operator alone may inspect ----------------------------------

# A reduction of one value never applies the operator to a pair, so nothing
# may ask that value whether it is true.
{
    my $calls = 0;
    my class Counted { method Bool() { $calls++; True } }
    my $ignored := ([||] Counted.new);
    is $calls, 0, '[||] on a single value does not ask it whether it is true';
}
{
    my $calls = 0;
    my class Counted { method Bool() { $calls++; False } }
    my $ignored := ([||] Counted.new, Counted.new, Counted.new);
    is $calls, 3, '[||] asks a result it does not settle on once per further value';
}

# Asking a Failure whether it is defined marks it handled, which suppresses
# the error it carries.
{
    sub risky() { fail "boom" }
    my $result := ([//] Nil, risky());
    my $handled = $result.handled;
    $result.so;                      # mark it, so it stays quiet at exit
    is-deeply $handled, False, '[//] leaves a Failure result unhandled';
}
{
    sub risky() { fail "boom" }
    my $threw;
    { sink ([||] risky()); CATCH { default { $threw = $_ } } }
    is $threw.?message, 'boom', '[||] on a lone Failure still throws where the result is used';
}

# The test for having settled takes a result of any type.  One standing for
# several answers it for all of them at once, so a true one settles || and
# leaves && unsettled.
is ([//] Mu, Mu, 3, Mu, 5), 3, '[//] carries a Mu result through to a defined one';
is ([||] Mu, Mu, 0, 3, Mu, 5), 3, '[||] carries a Mu result through to a true one';
is-deeply ([&&] Mu, Mu, 0, 3, Mu, 5), Mu, '[&&] on values starting with Mu returns Mu';
is ([&&] 1, (0|1), 5), 5, '[&&] does not settle on a Junction that is not all false';

# --- Ranges that produce values of their own choosing -----------------------

# The answers read off the endpoints belong to the values a Range produces for
# itself, so one that produces others is folded like any other list.
{
    my \spoken = (1..Inf) but role { method iterator() { (9, 8, 7).iterator } };
    is ([min] spoken), 7, '[min] on a Range producing its own values returns their smallest';
    is ([max] spoken), 9, '[max] on a Range producing its own values returns their largest';
    is ([+] spoken), 24, '[+] on a Range producing its own values sums them';
}
{
    my $both = -Inf..Inf;
    is ([+] $both), Inf, '[+] on an itemized Range counts it as one value';
}

# --- Ranges that report themselves infinite but produce nothing -------------

is ([+] Inf..Inf), 0, '[+] on an empty Range returns the identity of +';
is ([min] NaN..5), Inf, '[min] on a Range starting at NaN returns the identity of min';
is ([max] NaN..Inf), -Inf, '[max] on a Range starting at NaN returns the identity of max';
is ([min] Inf^..Inf), Inf, '[min] on an empty Range excluding its start returns the identity';

# An undefined list has no values to read and no buffer to look at.
{
    my @warnings;
    my $sum = do {
        CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
        [+] Array;
    };
    is $sum, 0, '[+] on an undefined Array returns the identity';
    like @warnings[0], /'uninitialized value'/,
      '[+] on an undefined Array warns that it has no values';
}
is ([min] List), Inf, '[min] on an undefined List returns the identity of min';

# A shaped array keeps its values in a buffer of its own kind, which no
# reduction reads.
{
    my @shaped[2;2] = (1,2),(3,4);
    is ([+] @shaped), 10, '[+] on a shaped array sums its values';
    is ([min] @shaped), 1, '[min] on a shaped array returns the smallest';
    is ([max] @shaped), 4, '[max] on a shaped array returns the largest';
}

# --- an itemized argument counts as one value -------------------------------

{
    my $endless = 1..Inf;
    is-deeply ([min] $endless), (1..Inf), '[min] on an itemized Range returns that Range';
    is-deeply ([max] $endless), (1..Inf), '[max] on an itemized Range returns that Range';
}
is-deeply ([min] $(1..Inf)), (1..Inf), '[min] on an itemized Range term returns that Range';

# --- an undefined Range has no endpoints to read ----------------------------

is ([min] Range), Inf, '[min] on an undefined Range returns the identity of min';
is ([max] Range), -Inf, '[max] on an undefined Range returns the identity of max';
{
    my Range $unset;
    is ([min] $unset), Inf, '[min] on an itemized undefined Range returns the identity';
}

# --- values that never run out ----------------------------------------------

# Each reduction below answers without reaching the end of its values, so a
# fault here is an endless loop.  Run them apart from the main thread, and
# report both a timeout and a throw, since neither shows up on its own.
my Bool $complete = False;
my $work = start {
    is ([||] flat(0, 0, 5, 1..Inf)), 5,
      '[||] on endless values returns the first true one';
    is ([||] flat(0, (0|1), 1..Inf)).gist, 'any(0, 1)',
      '[||] settles on a Junction that is not all false';
    is ([or] flat(0, 0, 5, 1..Inf)), 5,
      '[or] on endless values returns the first true one';
    is ([||] flat(5, 1..Inf)), 5,
      '[||] settles on a first value that is already true';
    is ([&&] flat(1, 0, 1..Inf)), 0,
      '[&&] on endless values returns the first false one';
    is ([and] flat(1, 0, 1..Inf)), 0,
      '[and] on endless values returns the first false one';
    is ([//] flat(Nil, 7, 1..Inf)), 7,
      '[//] on endless values returns the first defined one';
    is ([//] flat(Nil, 0, 1..Inf)), 0,
      '[//] settles on a defined value that is false';

    # The value that settles the result is pulled, then one more, which is
    # what tells the reduction the result is not the last value.
    {
        my $pulled = 0;
        my $values := lazy gather {
            for flat(0, 7, 1..Inf) { $pulled++; take $_ }
        }
        is ([||] $values), 7, '[||] returns the value it settled on';
        is $pulled, 3, '[||] pulls one value past the one it settled on';
    }
    {
        my $pulled = 0;
        my $values := lazy gather {
            for flat(1, 0, 1..Inf) { $pulled++; take $_ }
        }
        is ([&&] $values), 0, '[&&] returns the value it settled on';
        is $pulled, 3, '[&&] pulls one value past the one it settled on';
    }

    is ([+] 0..Inf), Inf, '[+] on a Range without an end returns Inf';
    is ([+] 1..*), Inf, '[+] on a Range ending in Whatever returns Inf';
    is ([+] 0^..Inf), Inf, '[+] on an endless Range excluding its start returns Inf';
    is ([+] -Inf..0), -Inf, '[+] on a Range starting at -Inf returns -Inf';
    is ([+] -Inf..Inf), NaN, '[+] on a Range unbounded both ways returns NaN';
    is ((0..Inf).reduce(&[+])), Inf, '.reduce(&[+]) on an endless Range returns Inf';

    # Only a Range of Reals has a sum to take.  Anything else has to fold, so
    # that the operator raises what it raises on a bounded Range of the same.
    {
        my $threw;
        { my $ignored = [+] 'a'..*; CATCH { default { $threw = $_ } } }
        isa-ok $threw, X::Str::Numeric,
          "[+] on an endless Range of strings raises the operator's error";
    }

    is ([min] 0..Inf), 0, '[min] on a Range without an end returns the smallest value it produces';
    is ([min] 0^..Inf), 1,
      '[min] on an endless Range excluding its start skips that start';
    is ([min] 0..NaN), 0, '[min] on a Range ending at NaN returns the smallest value it produces';
    is-deeply ([min] -Inf..0), -Inf, '[min] on a Range starting at -Inf returns the smallest value it produces';
    is-deeply ([min] 1.5..Inf), 1.5, '[min] on an endless Range of Rationals returns the smallest value it produces';
    is ([max] 1.5..Inf), Inf, '[max] on an endless Range of Rationals returns Inf';

    # A Range of Strs climbs through .succ, which rolls past the last letter
    # and reaches values below its start, so its start is not the smallest
    # value it produces.
    is ('y'..*).min, 'y', 'a Range of Strs reports a start that is not its smallest value';
    is ('y'..*).head(4).List.min, 'aa', 'a Range of Strs reaches values below its start';

    # A Num stops climbing once a step of one is below its precision, and a
    # Rational with nothing to divide by never climbs at all.
    ok (2e0 ** 53).succ == 2e0 ** 53, 'a Num at the width of its kind does not climb';
    ok <-1/0> + 1 == <-1/0>, 'a Rational with a zero denominator does not climb';

    # A Whatever endpoint reports a Range infinite whatever its start, so one
    # starting at a value that climbs its own way is folded, where its climb
    # runs out loudly rather than answering from an endpoint.
    {
        my class Climb {
            has $.n;
            method succ() { $!n >= 3 ?? die "climbed off" !! Climb.new(n => $!n + 1) }
        }
        throws-like { [min] Climb.new(n => 1)..* }, X::AdHoc, message => 'climbed off',
          '[min] on an endless Range of values that climb their own way reads the values';
        throws-like { [max] Climb.new(n => 1)..* }, X::AdHoc, message => 'climbed off',
          '[max] on an endless Range of values that climb their own way reads the values';
    }

    # Folding an endless Range with a scoped operator reaches that operator on
    # the first pair, where taking Range.sum would never reach it at all.
    {
        my sub infix:<+>(\a, \b) { die "scoped + folded" }
        my $folded = False;
        { my $ignored = [+] 0..Inf; CATCH { default { $folded = .message eq "scoped + folded" } } }
        ok $folded, '[+] on a + declared in scope folds an endless Range with that operator';
    }
    is ([max] 0..Inf), Inf, '[max] on a Range without an end returns Inf';
    is ([max] 1..*), Inf, '[max] on a Range ending in Whatever returns Inf';
    is ((0..Inf).reduce(&[min])), 0, '.reduce(&[min]) on an endless Range returns the smallest value it produces';
    is ((0..Inf).reduce(&[max])), Inf, '.reduce(&[max]) on an endless Range returns Inf';
    is (flat(0, 0, 5, 1..Inf).reduce(&[||])), 5,
      '.reduce(&[||]) on endless values returns the first true one';

    is-deeply ([\+] 1..Inf).head(3).List, (1, 3, 6),
      '[\+] on a Range without an end stays lazy';

    $complete = True;
};
await Promise.anyof($work, Promise.in(60));
diag "reduction threw: {$work.cause.gist}" if $work.status ~~ Broken;
ok $complete, 'every reduction over endless values finished';

# vim: expandtab shiftwidth=4
