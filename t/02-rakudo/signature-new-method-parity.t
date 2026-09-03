use Test;

plan 47;

class Foo {
    method bar(Str $a, Int $b) { }
}
my $method-sig := Foo.^lookup('bar').signature;

sub params() {
    Parameter.new(type => Foo, :invocant),
    Parameter.new(name => '$a', type => Str),
    Parameter.new(name => '$b', type => Int),
    Parameter.new(name => '*%_'),
}

# returns
{
    my $sig := Signature.new(params => params);
    ok $sig.returns =:= Mu,
      'unspecified return type is the Mu type object itself';
    is $sig.raku, ':(Foo $:: Str $a, Int $b, *%_)',
      'unspecified return type does not print as --> Mu';
    ok Signature.new(params => params, returns => Mu).returns =:= Mu,
      'explicit Mu return type is the Mu type object itself';
    nok $method-sig.ACCEPTS(Signature.new(params => params, returns => Nil)),
      'a different return type still makes the signatures differ';
}

# arity and count
{
    my $sig := Signature.new(params => params);
    is $sig.arity, 3, 'arity counts invocant and required positionals only';
    is $sig.count, 3, 'count leaves out the slurpy hash';
    is Signature.new(params => (Parameter.new(name => '$a'), Parameter.new(name => '$b?'))).arity, 1,
      'arity leaves out optional positionals';
    is Signature.new(params => (Parameter.new(name => '$a'), Parameter.new(name => '$b?'))).count, 2,
      'count includes optional positionals';
    is Signature.new(params => (Parameter.new(name => '$a'), Parameter.new(name => '*@b'))).count, Inf,
      'count is Inf with a slurpy positional';
    is Signature.new(params => (Parameter.new(name => '**@a'),)).count, Inf,
      'count is Inf with a double slurpy positional';
    is Signature.new(params => (Parameter.new(name => '|c'),)).count, Inf,
      'count is Inf with a capture';
    is Signature.new(params => (Parameter.new(name => ':$x'),)).count, 0,
      'count leaves out named parameters';
    is Signature.new(params => (Parameter.new(name => '$a'),), arity => 2).count, 2,
      'count is never below an explicit arity';
}

# the parameter list
{
    my @array = params;
    my $sig := Signature.new(params => @array);
    ok $method-sig.ACCEPTS($sig),
      'constructed signature matches when the parameters come from an Array';
    @array[0] = Parameter.new(name => '$z');
    is $sig.raku, ':(Foo $:: Str $a, Int $b, *%_)',
      'assigning into the Array afterwards does not change the signature';
    @array.push: Parameter.new(name => '$c');
    is $sig.params.elems, 4,
      'pushing to the Array afterwards does not change the signature';
    is Signature.new(params => (1..2).map({ Parameter.new(name => "\$a$_") }), arity => 2, count => 2e0).params.elems, 2,
      'parameters from a lazy list are all kept when arity and count are given';
    throws-like { Signature.new(params => (1,)) }, X::TypeCheck,
      'a parameter that is not a Parameter is rejected';
    throws-like { Signature.new(params => (Parameter.new(name => '$a'), Nil)) }, X::TypeCheck,
      'a Nil parameter is rejected';
}

# matching a method signature
{
    ok $method-sig.ACCEPTS(Signature.new(params => params)),
      'constructed signature matches the method signature';
    ok $method-sig eqv Signature.new(params => params),
      'constructed signature is eqv to the method signature';
    ok $method-sig.params.tail.type =:= Associative,
      'implicit slurpy hash of a method is typed Associative';
    ok $method-sig.params.tail eqv Parameter.new(name => '*%_'),
      'implicit slurpy hash of a method is eqv to a constructed one';
}

# sigil implied types
{
    ok Parameter.new(name => '@a').type =:= Positional,
      'array sigil implies Positional';
    ok Parameter.new(name => '%h').type =:= Associative,
      'hash sigil implies Associative';
    ok Parameter.new(name => '&c').type =:= Callable,
      'code sigil implies Callable';
    ok Parameter.new(name => '@a', type => Int).type =:= Positional[Int],
      'array sigil with a type gives the parameterized Positional';
    ok Parameter.new(name => '%h', type => Int).type =:= Associative[Int],
      'hash sigil with a type gives the parameterized Associative';
    ok Parameter.new(name => '&c', type => Int).type =:= Callable[Int],
      'code sigil with a type gives the parameterized Callable';
    ok Parameter.new(name => '@a', type => Int) eqv :(Int @a).params[0],
      'typed array parameter is eqv to the compiled one';
    ok Parameter.new(name => ':foo(@x)').type =:= Positional,
      'array sigil behind an alternative name implies Positional';
    ok Parameter.new(name => ':foo(@x)') eqv :(:foo(@x)).params[0],
      'aliased array parameter is eqv to the compiled one';
}

# definite types
{
    ok Parameter.new(name => '$a', type => Int:D).type =:= Int,
      'definite type is stored as its base type';
    is Parameter.new(name => '$a', type => Int:D).raku, 'Int:D $a',
      'definite type is kept as the definedness of the parameter';
    ok Parameter.new(name => '$a', type => Int:D) eqv :(Int:D $a).params[0],
      'defined only parameter is eqv to the compiled one';
    ok Parameter.new(name => '$a', type => Int:U) eqv :(Int:U $a).params[0],
      'undefined only parameter is eqv to the compiled one';
    ok Parameter.new(name => '@a', type => Int:D) eqv :(Int:D @a).params[0],
      'defined only array parameter is eqv to the compiled one';
}

# slurpies
{
    ok Parameter.new(name => '+@a') eqv :(+@a).params[0],
      'single argument slurpy is eqv to the compiled one';
    nok Parameter.new(name => '+@a').raw,
      'single argument slurpy is not raw';
    throws-like { Parameter.new(name => '*@a', type => Int) }, Exception,
      message => 'Slurpy positional parameters with type constraints are not supported',
      'typed slurpy positional is rejected as a positional';
    throws-like { Parameter.new(name => '+@a', type => Int) }, Exception,
      message => 'Slurpy positional parameters with type constraints are not supported',
      'typed single argument slurpy is rejected as a positional';
    throws-like { Parameter.new(name => '*%h', type => Int) }, Exception,
      message => 'Slurpy named parameters with type constraints are not supported',
      'typed slurpy named is rejected as a named';
}

# smartmatching with slurpies
{
    nok :(:$x) ~~ :(*@a),
      'slurpy positional does not accept named arguments';
    nok :(*%h) ~~ :(*@a),
      'slurpy positional does not accept a slurpy hash';
    nok :(|c) ~~ :(*@a),
      'slurpy positional does not accept everything a capture accepts';
    ok :(*@a) ~~ :(|c),
      'capture accepts everything a slurpy positional accepts';
    ok :(**@a) ~~ :(*@a),
      'slurpy positional accepts everything a double slurpy accepts';
}

# vim: expandtab shiftwidth=4
