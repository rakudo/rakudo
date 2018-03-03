# Polishing Rationals

Mar. 4, 2018 proposal by Zoffix

## Executive Summary

1. `Rat` literals with denominators over 64-bit to be returned as a `FatRat`
2. `RatStr` types with denominators over 64-bit to be returned as a newly-implemented type `FatRatStr`
3. If `Rat.new` is called with denominator that is (after reduction) over 64-bit, construct a `FatRat` instead
4. Remove the optimization that requires the use of .REDUCE-ME method, as
    the optimization has bugs, race conditions, and is detrimental in many
    cases. Preliminary testing (with some new optimizations) showed an 18% improvement in performance, so we're still getting a win here.
5. Always normalize zero-denominator Rationals to `<1/0>`, `<-1/0>`, and `<0/0>`
    - Try mixing in `ZeroDenominatorRational` role into these to get
        performance boost in operators (in dispatch). If improvement is low,
        don't implement this part (the role mixing).

# TABLE OF CONTENTS
- [Problems Being Addressed](#problems-being-addressed)
- [1) Eliminate edge-cases that produce `Rat`s with denominators above 64-bit](#1-eliminate-edge-cases-that-produce-rats-with-denominators-above-64-bit)
    - [<strong>I propose:</strong>](#i-propose)
    - [Reasoning](#reasoning)
    - [Discarded Ideas](#discarded-ideas)
- [2) Make `Rational`s fully-immutable to avoid data-races](#2-make-rationals-fully-immutable-to-avoid-data-races)
    - [1) <strong>Data Race</strong>](#1-data-race)
    - [2) <strong>Inconsistent Object Identity</strong>](#2-inconsistent-object-identity)
    - [3) <strong>Limited Usefulness of the Optimization</strong>](#3-limited-usefulness-of-the-optimization)
    - [<strong>I propose:</strong>](#i-propose-1)
- [3) Fix bugs with operations on zero-denominator `Rational`s](#3-fix-bugs-with-operations-on-zero-denominator-rationals)
    - [<strong>I propose:</strong>](#i-propose-2)

## Problems Being Addressed

1) Eliminate edge-cases that produce `Rat`s with denominators above 64-bit ([RT#132313](https://rt.perl.org/Public/Bug/Display.html?id=132313#ticket-history))
2) Make `Rational`s fully-immutable to avoid data-races ([RT#130774](https://rt.perl.org/Ticket/Display.html?id=130774#ticket-history))
3) Fix bugs with operations on zero-denominator `Rational`s ([R#1354](https://github.com/rakudo/rakudo/issues/1354))

## 1) Eliminate edge-cases that produce `Rat`s with denominators above 64-bit

Under normal conditions, and if `FatRat`s are not involved, if a `Rat`-producing operation were to make a `Rat` with a denominator larger than 64-bit, the result is a `Num` instead:

```perl-6
    say 1/48112959837082048697; # OUTPUT: «2.07844207337515e-20»
```

However, currently it's possible to create a `Rat` with denominators over
64-bit using `val`, quote words, `Rat` literal syntax, or `Rat.new` method call:

```perl-6
    say [.^name, $_, .nude, .denominator.log: 2]
        with (val "1/48112959837082048697").Numeric;
    # [Rat 0.000000000000000000021 (1 48112959837082048697) 65.3830593574438]

    say [.^name, $_, .nude, .denominator.log: 2] with <1/48112959837082048697>;
    # [Rat 0.000000000000000000021 (1 48112959837082048697) 65.3830593574438]

    say [.^name, $_, .nude, .denominator.log: 2]
        with Rat.new: 1, 48112959837082048697;
    # [Rat 0.000000000000000000021 (1 48112959837082048697) 65.3830593574438]

    say [.^name, $_, .nude, .denominator.log: 2] with 1.111111111111111111111
    # [Rat 1.11111111111111111604544 (1111111111111111111111 1000000000000000000000) 69.7604899926346]
```

As can be seen from the last example above, there is loss of precision involved in some routines when working with such `Rats`.

### **I propose:**

1. `Rat` literals with denominators over 64-bit are returned as a `FatRat`
2. `RatStr` types with denominators over 64-bit are returned as a newly-implemented type `FatRatStr`
3. If `Rat.new` is called with denominator that is (after reduction) over 64-bit, construct a `FatRat` instead

### Reasoning

1. The new system makes the `Rat` literals be `Rational` literals, with
    precision handling based on how much precision the user provided.
2. We fill the allomorphs with the only missing type, making all `Numeric`s
    available as an allomoprhic variant.
3. While it may be somewhat unusual for `Rat.new` to create a type that isn't
    a `Rat`, I believe creating a `FatRat` instead of throwing is more user-
    friendly, as it can be hard to discern whether the denominator would fit, especially because the fit-test is done **after** reduction. For example, try guessing which of this would fit into a Rat:

```perl-6
    Rat.new: 48112959837032048697, 48112959837082048697

    Rat.new: 680564733841876926926749214863536422912,
             1361129467683753853853498429727072845824
```

The first one would end up as a `FatRat` with it's 66-bit denominator, while the second one becomes a `Rat` with value `0.5` after reduction.

### Discarded Ideas

These are the alternatives I've considered and found inadequate.

- *Discarded Idea #1:* All of problematic cases to produce a `Num` (or `NumStr`)

    While this avoids creating a new type it creates a problem that the user might get a type that isn't `Rational` by just adding a single digit:
    And that also means that when the user **explicitly gives us more
    precision** in their literals, we discard it and give a type that's less
    precise than even a plain `Rat`:

    ```perl-6
        my Rat $x = 4.9999999999999999999;  # OK
        my Rat $x = 4.99999999999999999999; # Typecheck failure: Got Num

        say 4.999999999999999999999999999999999999999999999999999999999999999999 ~~ ^5;
        # (would produce "False")
    ```

- *Discarded Idea #2:* Make literals produce `FatRat` and val/quotewords produce `RatStr` that can be either `FatRat` or `Rat` in the numeric portion.

    This creates a problem with infectiousness in that, say `Num` + `RatStr`
    produce a `Num`. In a scenario where `RatStr` could contain a `FatRat` as
    a numeric, the operation would be expected to produce a `FatRat` as a
    result. Even if this is made to work, it would be unpredictable behaviour,
    as you can't tell by type alone what result you'd receive.

- *Discarded Idea #3:*  Introduce non-infectious FatRat type

    This would also require an allomorphic counterpart and I'm a bit worried
    about the increase in operators to handle such a type. And if you're making
    this type lose precision with operations with other types, may as well
    not have it have that precision available in the first place.

## 2) Make `Rational`s fully-immutable to avoid data-races

Current implementation of `Rational` contains an optimization used by certain operators, e.g. `infix:<+>`: if we can perform the operation without needing
to change the denominator, then we save time by avoiding reduction and merely produce an **un-reduced Rational** with a tweaked numerator. Thus, for example,
instead of `<1/1>`, `½ + ½` gives `<2/2>`:

    say [.numerator, .denominator] with ½ + ½;
    # [2 2]

A private `.REDUCE-ME` method is then used to cheat around that optimization
and methods that need a reduced rational call it:

    say .nude with ½ + ½;
    # (1 1)

This approach has three problems:

### 1) **Data Race**

The worst issue is a rare data race. Since `.REDUCE-ME` **mutates** the
`Rational` object and some operations read the numerator/denominator without
reduction, it's possible for an operation in one thread (say `infix:<+>`) to
read off the numerator, then for another thread to mutate numerator and
denominator, and then for the first thread to read the denominator that no
longer corresponds to the numerator that was read.

The following code reproduces the race, and once in a while dies with
`Died with the exception: Not right [2] (denominator  2 numerator     2)`:
indicating that mathematical operation `½ + ½ + ½` resulted in answer `1`.
Imagine the look on CFO's face when they find out a $1.5M profit somehow ended
up being just $1M.

```perl-6
    use v6.d.PREVIEW;
    for ^20000 {
        await ^10 .map: { start {
            my $r := ½ + rand.ceiling/2;
            my $at := now + .003;
            await Promise.at($at).then({
                    $r.nude;
                    $r.numerator == $r.denominator == 1
                      or die "Not right [1] ($r.Capture())";
                }),
                Promise.at($at).then({
                    my $r2 := $r + ½;
                    $r2.numerator == 3 and $r2.denominator == 2
                      or die "Not right [2] ($r2.Capture())";
                })
        }}
    }
```

### 2) **Inconsistent Object Identity**

Since `Rational`s are a value type, the following answer makes sense:

```perl-6
    say ½+½ === 1/1;
    # True
```

The two resultant `Rational`s are of the same type and have the same value,
and thus are the same object. Object identity is used by `Set`s and related
types, so we'd expect the two objects above, when placed into a `Set`, to
be counted as one item, however they don't:

```perl-6
    say set(½+½, 1/1).perl;
    # Set.new(1.0,<1/1>)
```

The aforementioned `.REDUCE-ME` must be called by everything that has to
operate on a reduced rational. We already fixed several bugs where methods
did not do so, and the above example is just one more in that bunch. Even the
output of `.perl`—which doesn't need to use `.REDUCE-ME`—is affected
by the presence of this optimization.

### 3) **Limited Usefulness of the Optimization**

The aforementioned optimization that produces unreduced rationals is only
beneficial if those operations are vastly more common than any other operation
that has to use `.REDUCE-ME` as a result. I believe that assumption is too
narrow in scope and in many cases is actually detrimental.

First, if we remove this optimization, reduction would have to be done precisely
once and only when it's needed. With the optimization, however, we have to
go through `.REDUCE-ME` routine multiple times, even if we no reduction needs
to be done. Thus, code like this…

    my $r := ½ + ½;
    my $ = $r.narrow for ^5000_000;

…is 4.65x **SLOWER** when the optimization is in place than when it isn't.

The impact is felt even in routines that don't have to call `.REDUCE-ME`, such
as `.floor`:

    my $r := ½ + ½;
    my $ = $r.floor for ^5000_000;

The above construct becomes 20% faster if we reduce the rational *before* going
into the `for` loop, thanks to the fast-path on `$!denominator == 1` in the
`.floor` routine. While that may seem trivial, `.floor` is actually used
*multiple times* by each `.Str` call, and so this…

    my $r := ½ + ½;
    my $ = $r.Str for ^500_000;

…becomes 30% faster if reduction is performed during addition.

------

As can be seen, even if all of the bugs and race conditions were addressed,
the detrimental impact of this optimization is far-reaching, while the
beneficial aspect is rather narrow. Certainly, an accounting software that
sums up the totals of last month's thousands of purchases can benefit from
`infix:<+>` not performing reduction, but is that that common case? Or would
faster `.Str`, `.narrow`, and dozens of other methods
be of more benefit to a multitude of programs in other domains.

### **I propose:**

I propose we address the issues above by simply removing this optimization
altogether.

I've performed preliminary testing using a bench that calls all Rational methods enough times for each method's bench to run for 1 second. When running all the
benches together (thus, having some GC runs), with **removed** `REDUCE_ME`
optimization and a couple of new optimizations applied, I was able to get
the bench to run **16% faster**. So, I think after this proposal is
fully-implemented, we'll see some performance wins, not losses.

## 3) Fix bugs with operations on zero-denominator `Rational`s

The bugs are largely due to numerators being arbitrary numbers, yet being
computed as if they were normal Rationals. So `<3/0> - <5/0>` (`Inf - Inf`)
ends up being `<-2/0>` (`-Inf`), but it must become a `<0/0>` (`NaN`).

### **I propose:**

My primary proposal is for zero-denominator `Rational`s to be normalized to
`<1/0>`, `<-1/0>`, and `<0/0>`. I think doing that alone will fix all the
bugs in all the routines (tests that'll be written will show that). If it
won't, the still-broken cases will be tweaked in specific routines on a
case-by-case basis.

My secondary proposal is to implement an essentially empty role
`ZeroDenominatorRational` that will be used to tag zero-denominator
`Rationals` and the ops that have special-handling for zero-denominator
`Rationals` would move that handling into a separate multi candidate dispatched
on that role. The hope here is to get a sizeable performance improvement by not
having to do extra checks for zero-denominator Rationals in common operators.
If it'll turn out the performance improvement this idea brings is
insignificant, this proposal will not be implemented.
