use Test;

plan 19;

# A coercion involving a subset must run the subset's refinement on every
# value. The dispatch program recorded for a coercion only guards on the
# type and concreteness of the value, so the fate of an earlier value of
# the same type must not decide the fate of later ones. The order of the
# assertions below is what exercises this: accepted values are followed by
# rejected values of the same type, and the other way around.

subset Even of UInt where * %% 2;

is Even(56), 56, 'a value satisfying the subset coerces to itself';
throws-like { Even(-3) }, X::Coerce::Impossible,
    'a value failing the refinement is rejected after one was accepted';
is Even(58), 58, 'a value satisfying the subset is accepted after one was rejected';

is-deeply
    (56, -3, 58).map({ (try Even($_)) // 'rejected' }).List,
    (56, 'rejected', 58),
    'a single call site accepts and rejects by value, not by type';

is UInt(5), 5, 'a natural number coerces to UInt';
throws-like { UInt(-3) }, X::Coerce::Impossible,
    'a negative Int is rejected by UInt after a natural number was accepted';

is Even("56"), 56, 'a Str holding an even number coerces into the subset';
throws-like { Even("57") }, X::Coerce::Impossible,
    'a Str holding an odd number is rejected after an even one was accepted';
is Even("58"), 58, 'a Str holding an even number is accepted after an odd one was rejected';

sub with-subset-target(Even() $x) { $x }
is with-subset-target(4), 4, 'a parameter coercing to a subset accepts a satisfying value';
throws-like { with-subset-target(-3) }, X::Coerce::Impossible,
    'a parameter coercing to a subset rejects a failing value after accepting one';
is with-subset-target(8), 8,
    'a parameter coercing to a subset accepts a satisfying value after rejecting one';

sub with-subset-constraint(Str(Even) $x) { $x }
is with-subset-constraint(4), "4", 'a subset constraint on a coercion accepts a satisfying value';
dies-ok { with-subset-constraint(-3) },
    'a subset constraint on a coercion rejects a failing value after accepting one';
is with-subset-constraint(6), "6",
    'a subset constraint on a coercion accepts a satisfying value after rejecting one';

subset DoubleEven of Even;
is DoubleEven(56), 56, 'a subset of a refined subset accepts a satisfying value';
throws-like { DoubleEven(-3) }, X::Coerce::Impossible,
    'a subset of a refined subset rejects a failing value after accepting one';

class NoCoercer { }
subset AnyNoCoercer of NoCoercer where { True };
my $instance = NoCoercer.new;
ok AnyNoCoercer($instance) === $instance,
    'a matching value with no coercion method passes through unchanged';
ok AnyNoCoercer($instance) === $instance,
    'a matching value with no coercion method still passes through on a repeated call';

# vim: expandtab shiftwidth=4
