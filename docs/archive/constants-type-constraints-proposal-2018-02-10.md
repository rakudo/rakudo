# Type Constraints on Constants

*Feb. 10, 2018 proposal by Zoffix*

# TABLE OF CONTENTS
- [Type Constraints on Constants](#type-constraints-on-constants)
    - [General Spec of Behaviour](#general-spec-of-behaviour)
    - [Examples of behaviour](#examples-of-behaviour)
    - [Sigilless/`$`-sigiled constants](#sigilless-sigiled-constants)
    - [Behaviour with `@`-, `%`-, and `&amp;`-sigiled constants](#behaviour-with-----and--sigiled-constants)
        - [`@`-sigiled constants](#-sigiled-constants)
        - [`%`-sigiled constants](#-sigiled-constants-1)
        - [`&amp;`-sigiled constants](#-sigiled-constants-2)
    - [Deviations from Current Behaviour](#deviations-from-current-behaviour)

## General Spec of Behaviour

* Constants bind initializer to them
* Constants with `@` sigil
    * Typecheck initializer for `Positional`
        * When `.=` op is used, method is called on `List` type object
    * If initializer isn't positional, call `.cache` on it
* Constants with `%` sigil
    * Typecheck initializer for `Associative`
        * When `.=` op is used, method is called on `Map` type object
    * If initializer isn't associative, call `.Map` on it
* Constants with `&` sigil
    * Typecheck initializer for `Callable`
        * When `.=` op is used, method is called on `Callable` type object
    * If it isn't callable, throw

* Default type of constants is `Mu`
* Only `is export` trait is supported
* `where` constraints are not supported


## Examples of behaviour

*Note: the `.new()` calls shown in comments simply describe what the resultant
object will be, NOT that any `.new()` calls will actually be made.*

## Sigilless/`$`-sigiled constants

`foo` and `\foo` are equivalent. `$foo` is equivalent in behaviour, except that
it defines a symbol with a `$` sigil.

```perl6
constant foo .= new;  # Mu.new

       constant foo  = 42;      # Int.new(42)
my Int constant foo  = 42;      # Int.new(42)
my Int constant foo  .=new: 42; # Int.new(42)
```

```perl6
my Array[Numeric] constant foo .= new: 1, 2, 3; # Array[Numeric].new(1, 2, 3)
foo.push: 42; # it's an Array, so this works
```

## Behaviour with `@`-, `%`-, and `&`-sigiled constants

Some cases don't have a clear and obvious meaning, as `constant @foo .=new: 1,
2, 3` makes a `List`, yet `my Numeric constant @foo .=new: 1, 2, 3` would have
to make an `Array`, because `List` cannot be parametarized.

So for them, I propose throwing a `X::ParametricConstant` exception, defined
something like:

```perl6
my class X::ParametricConstant is Exception {
    has $.sigil;
    method message() {
        "Parametarization of {$!sigil}-sigiled constants is not supported.\n"
        # ~ "some helpful explanation of the alternative declarations"
    }
}
```

### `@`-sigiled constants

```perl6
constant @foo  =      1, 2, 3;   # List.new(1, 2, 3)
constant @foo .=new:  1, 2, 3;   # List.new(1, 2, 3)
constant @foo  =     [1, 2, 3];  # Array.new(1, 2, 3)
constant @foo .=new: [1, 2, 3];  # List.new(1, 2, 3)

constant @foo = Array[Numeric].new: 1, 2, 3; # Array[Numeric].new(1, 2, 3)
my Numeric constant @foo  =     1, 2, 3;   # throw X::ParametricConstant
my Numeric constant @foo .=new: 1, 2, 3;   # throw X::ParametricConstant

my $foo := class Foo {}.new;
constant @foo = $foo;       # List.new(Foo.new)
constant @foo = $foo,;      # List.new(Foo.new)
constant @foo = $foo, $foo; # List.new(Foo.new, Foo.new)

class Bar does Positional {}.new;
BEGIN my $bar-scalar = Bar.new;
constant @foo = Bar.new;           # Bar.new
constant @foo = $bar-scalar;       # Bar.new
constant @foo = Bar.new,;          # List.new(Bar.new)
constant @foo = Bar.new, Bar.new;  # List.new(Bar.new, Bar.new)
```

### `%`-sigiled constants

```perl6
constant %foo  =     1, 2, 3;     # throw X::Hash::Store::OddNumber
constant %foo  =     1, 2, 3, 4;  # Map.new((1 => 2, 3 => 4))
constant %foo  =    [1, 2, 3, 4]; # Map.new((1 => 2, 3 => 4))
constant %foo .=new: 1, 2, 3, 4;  # Map.new((1 => 2, 3 => 4))

constant %foo   =       :42foo;          # Pair.new(:key<foo>, :value(42))
constant %foo   =       :42foo, :70bar;  # Map.new((:42foo, :70bar))
constant %foo   =     %(:42foo, :70bar); # Hash.new((:42foo, :70bar))
constant %foo  .=new:   :42foo;          # Map.new((:42foo))
constant %foo  .=new:   :42foo, :70bar;  # Map.new((:42foo, :70bar))
constant %foo  .=new: %(:42foo, :70bar); # Map.new((:42foo, :70bar))


constant %foo = Hash[Numeric].new: 1, 2; # Hash[Numeric].new(1, 2)
my Numeric constant %foo  =     1, 2;   # throw X::ParametricConstant
my Numeric constant %foo .=new: 1, 2;   # throw X::ParametricConstant

class Foo {}.new;
constant %foo = Foo.new;          # throw X::Hash::Store::OddNumber
constant %foo = Foo.new,;         # throw X::Hash::Store::OddNumber
constant %foo = Foo.new, Foo.new; # Map.new(Foo.new, Foo.new)

class Bar does Associative {}.new;
BEGIN my $bar-scalar = Bar.new;
constant %foo = $bar;        # Bar.new
constant %foo = $bar-scalar; # Bar.new
constant %foo = $bar,;       # throw X::Hash::Store::OddNumber
constant %foo = $bar, $bar;  # Map.new(Bar.new, Bar.new)
```

### `&`-sigiled constants

Throwing `X::ParametricConstant` on these sigils makes less sense, but I propose
we throw it anyway for consistency.

```perl6
my Numeric constant &foo  = -> --> Numeric {};   # throw X::ParametricConstant
my Numeric constant &foo .=new;                  # throw X::ParametricConstant
constant &foo = &say; # binds &say to &foo
```

## Deviations from Current Behaviour

I'm dubious there are any spec tests actually covering it, but if there are,
then the proposal is to implement the behaviour in 6.d language, instead.

```perl6
# (no spectests for this and it's already changed on master)
constant foo .= new; # OLD: Any.new
                     # NEW: Mu.new
```

```perl6
constant %foo = :42foo, :70bar; # CURRENT:  throws typecheck failure
                                # PROPOSED: Map.new((:42foo, :70bar))
```

That's all the deviation I can see. The rest of it either crashes:

```
    <Zoffix__> m: constant @foo .=new: 1, 2, 3
    <camelia> rakudo-moar f559c6d8b: OUTPUT: «===SORRY!=== Error while compiling
        <tmp>␤An exception occurred while evaluating a constant␤at
        <tmp>:1␤Exception details:␤  Cannot resolve caller new(Int: Int, Int);
        none of these signatures match:␤      (Int $: \value, *%_)␤      (In…»
```

Or silently fails:

```
<Zoffix__> m: my Int constant foo = 'not an Int'; say foo;
<camelia> rakudo-moar f559c6d8b: OUTPUT: «not an Int␤»
```
