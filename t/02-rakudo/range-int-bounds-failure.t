use lib 't/packages/Test-Helpers';
use Test;
use Test::Helpers;

plan 16;

# Asking a Range for integer bounds can reach .Int on an endpoint that has no
# integer value, and .Int hands back a Failure for those.  The garbage collector
# is what reports a Failure nothing marks handled, so each of these needs enough
# iterations to collect what its loop discards.

# .sum reaches the candidate taking two arguments.  An Inf endpoint is ruled out
# before .Int is asked, and a NaN low endpoint fails the test for being its own
# floor before that.
is-run 'for ^5000 { my $ignored = (0..Inf).sum }; print "done"',
  :out<done>,
  'a high endpoint of Inf leaves no unhandled Failure behind';

is-run 'for ^5000 { my $ignored = (-Inf..0).sum }; print "done"',
  :out<done>,
  'a low endpoint of -Inf leaves no unhandled Failure behind';

# Range.minmax reaches the candidate taking no arguments, but only where the
# bounds are already integers, so drive its guard directly.  .sum reaches a NaN
# high endpoint but never returns for one, so drive that directly too.  The
# argless candidate returns a Failure by contract.  Marking that one handled
# leaves stderr carrying only what nothing accounted for.
is-run 'for ^5000 {
            my $a; my $b; (0..NaN).int-bounds($a, $b);
            my $ignored = (0..Inf).int-bounds; $ignored.defined;
        }; print "done"',
  :out<done>,
  'a NaN endpoint and the argless bounds leave no unhandled Failure behind';

# An endpoint with no integer value need not be an Inf or a NaN.  A Rational
# with nothing to divide by reaches .Int as well.
is-run 'for ^5000 { my $ignored = (0..<1/0>).sum }; print "done"',
  :out<done>,
  'a Rational endpoint with a zero denominator leaves no unhandled Failure behind';

is-run 'for ^5000 { my $ignored = (0..<1/0>).int-bounds; $ignored.defined }; print "done"',
  :out<done>,
  'the argless bounds of a Rational with a zero denominator leave nothing behind';

# The bounds a Range reports are values rather than the containers they were
# written through.
{
    my $bounds := (0e0..3e0).int-bounds;
    dies-ok { $bounds[0] = 99 }, 'a Range of Nums reports bounds that cannot be written to';
}

# An endpoint that is not an integer still has integer bounds to read, and the
# guard against a Failure has to leave those alone.
is-deeply (0e0..3e0).int-bounds, (0, 3), 'a Range of Nums reads its integer bounds';
is (0e0..3e0).sum, 6, 'a Range of Nums sums its values through its integer bounds';
is (2e0^..^5e0).sum, 7, 'a Range of Nums excluding both ends sums its values';
is-deeply (0..<7/2>).int-bounds, (0, 3), 'a Range ending at a Rational reads its integer bounds';
{
    my $from; my $to;
    ok (0e0..3e0).int-bounds($from, $to), 'a Range of Nums reports having integer bounds';
    is-deeply ($from, $to), (0, 3), 'a Range of Nums writes its integer bounds';
}

# The argless candidate reaches .Int for a Rational with a zero denominator, and
# has to report having no bounds rather than let that Failure escape.
{
    my $bounds := (0..<1/0>).int-bounds;
    is $bounds.exception.message, 'Cannot determine integer bounds',
      'a Range ending at a Rational with a zero denominator reports no bounds';
    $bounds.so;
}

# .Int may answer a Failure that is a type object, which has no handled flag to
# set.  Ruling that out is what leaves the endpoint reported as having no
# integer bounds rather than throwing.
{
    my class NoIntType does Real {
        method Int()    { Failure }
        method floor()  { 2       }
        method Bridge() { 2e0     }
    }
    my $bounds := Range.new(NoIntType.new, 5).int-bounds;
    is $bounds.exception.message, 'Cannot determine integer bounds',
      'a Range whose endpoint answers .Int with a Failure type object reports no bounds';
    $bounds.so;
    my $from; my $to;
    nok Range.new(NoIntType.new, 5).int-bounds($from, $to),
      'a Range whose endpoint answers .Int with a Failure type object reports having none';
}

# An endpoint that answers .Int for itself reaches the marking at both endpoints
# and through both candidates, which no Range of setting types does.
is-run 'my class Unmarked is Failure { method Bool() { True } }
        my class NoInt does Real {
            method Int()    { Unmarked.new(X::AdHoc.new(payload => "no integer value")) }
            method floor()  { self }
            method Bridge() { 0e0 }
        }
        for ^5000 {
            my $a; my $b; Range.new(NoInt.new, 5).int-bounds($a, $b);
            my $ignored = Range.new(0, NoInt.new).int-bounds; $ignored.defined;
        }
        print "done"',
  :out<done>,
  'an endpoint answering .Int for itself leaves nothing behind at either end';

# vim: expandtab shiftwidth=4
