use Test;

plan 5;

throws-like { my Hash[Int](Any) $h = Failure.new('lookup failed') }, Exception,
    message => 'lookup failed',
    'a Failure coerced into a parameterized hash type throws itself';
throws-like { sub take(Array[Int]() $a) { }; take(Failure.new('lookup failed')) }, Exception,
    message => 'lookup failed',
    'a Failure coerced into a parameterized array parameter throws itself';

{
    my class Celsius { }
    my class Reading {
        method FALLBACK($name, |) { $name eq 'Celsius' ?? Celsius.new !! Nil }
    }
    my Celsius(Reading) $c = Reading.new;
    isa-ok $c, Celsius,
        'a value coerces through a FALLBACK that answers to the name of the target type';
}

{
    my class AnswersAnything { method FALLBACK($name, |) { 42 } }
    throws-like { my AnswersAnything(Int) $a = 5 }, X::Coerce::Impossible,
        'coercing into a type that only has a FALLBACK reports an impossible coercion';
}

{
    for ^20 {
        my $type := Metamodel::ClassHOW.new_type(name => "CoercionFallbackWarm$_");
        $type.^add_method('COERCE', my method ($x) { self.new });
        $type.^compose;
        Metamodel::CoercionHOW.new_type($type, Any).^coerce(42);
    }
    my class Target { method COERCE($x) { Target.new } }
    sub take(Target(Any) $t) { $t }
    throws-like { take(Failure.new('lookup failed')) }, Exception, message => 'lookup failed',
        'a Failure coerced after many other coercions throws itself';
}

# vim: expandtab shiftwidth=4
