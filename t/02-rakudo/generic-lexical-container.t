use Test;
use nqp;

plan 47;

role Greets      { has $.greeting = 'hi' }
role Holds[::U]  { method held { U } }
role First       { method who { 'first' } }
role Second      { method who { 'second' } }
role Labeled     { has $.label is rw = 'none' }
role Entered     { has $.entered = 'yes' }
role Counted     { has int $.count is rw; has str $.unit is rw }
role Box[::U]    { has U $.content is rw; has $.tag = 'boxed' }
role PushOnce[::U] { submethod TWEAK { self.push(1) }; method pushed-type { U } }

multi trait_mod:<is>(Variable:D $v, :$labeled!) {
    $v.var.VAR does Labeled;
    $v.var.VAR.label = $labeled;
}
multi trait_mod:<is>(Variable:D $v, :$counted!) {
    $v.var.VAR does Counted;
    $v.var.VAR.count = $counted;
    $v.var.VAR.unit = 'items';
}

sub key-of(::T, \key)  { my %h{T}; %h{key} = 2; %h }
sub value-of(::T)      { my T %h; %h<a> = 1; %h }
sub element-of(::T)    { my T @a; @a.push(1); @a }
sub key-and-value(::T) { my T %h{T}; %h{1} = 1; %h }
sub keyed-init(::T)    { my %h{T} = 1 => 2; %h }
sub array-init(::T)    { my T @a = 1, 2; @a }
sub explicit(::T)      { my T @a is Array; @a.push(1); @a }
sub hash-default(::T)  { my %h{Str} is default(T); %h }
sub array-default(::T) { my @a is default(T); @a }
sub counter(::T)       { state T @a; @a.push(1); @a.elems }
sub seeded(::T)        { state T @a = 1, 2; @a.push(3); @a.elems }
sub call-assign(::T)   { my %h{T} .= new(1 => 2); %h }
sub set-of(::T)        { my %h is SetHash[T]; %h{1} = True; %h }
sub definite-set(::T)  { my %h is SetHash:D[T]; %h{1} = True; %h }
sub definite-of(::T)   { no worries; my @a is Array[T:D]; @a }
sub definite-type(::T) { no worries; Array[T:D] }
sub constant-set(::T)  { my constant ST = SetHash[T]; my %h is ST; %h }
sub mixed(::T)         { my T @a does Greets does Holds[T]; @a }
sub mixed-untyped(::T) { my @a does Holds[T]; @a }
sub ordered(::T)       { my T @a does First does Second; @a }
sub labeled(::T)       { my T @a is labeled<x>; @a }
sub begun(::T)         { my @a is default(T) will begin { .push(42) }; @a }
sub entered(::T)       { my @a is default(T) will enter { .push(42) }; @a }
sub entered-role(::T)  { my T @a will enter { $_ does Entered }; @a }
sub reset-state(::T)   { state T @a; ENTER @a = (); @a.push(1); @a.elems }
sub skipped(::T)       { my @a is default(T) if False; @a }
sub loop-declared(::T) { my @refs; @refs.push((my T @a)) for ^2; @a.push(1); @refs.map(*.elems).List }
sub restated(::T)      { no worries; state T @a; @a.push(1); state T @a; @a.push(2); @a.elems }
sub state-set(::T, \k) { state %h is SetHash[T]; %h{k} = True; %h.elems }
sub stored(::T)        { state %Stored::h{T}; %Stored::h<a>++; %Stored::h<a> }
sub counted(::T)       { my T @a is counted(9); @a }
sub dynamic-of(::T)    { my T @a is dynamic; @a }
sub boxed(::T)         { my @a does Box[T]; @a }
sub pushed(::T)        { my @a does PushOnce[T]; @a }

# A shaped declaration that fails to compile fails only its own test.
my &shaped = try EVAL q[sub (::T) { my T @a[2]; @a[0] = 1; @a }];

role R[::T] {
    method keyed   { my %h{T}; %h{1} = 2; %h }
    method counted { state T @a; @a.push(T); @a.elems }
}

role Stash[::T] {
    my T @stash;
    method stash(\value) { @stash.push(value); @stash }
}

is-deeply (try EVAL q[my Int @a is Positional; @a.^name]), 'Positional[Int]',
    'an array of a known type with a parametric role as its container type is of that role';
try stored(Int) for ^2;
is-deeply (try stored(Int)), 3,
    'a package qualified state hash keyed by a type capture keeps its values across calls';

todo 'the legacy frontend leaves a generic array, hash or parameterization uninstantiated', 45
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is-deeply (try key-of(Int, 1).keyof), Int,
    'a type capture is the key type of a hash declared with it';
is-deeply (try key-of(Int, 1){1}), 2,
    'a hash keyed by a type capture stores a value under a key of that type';
is-deeply (try key-of(Str, 'a').keyof), Str,
    'a hash keyed by a type capture takes the key type of each call';
is-deeply (try key-of(Int, 1).of), Any,
    'a hash keyed by a type capture has the value type of an untyped hash';
throws-like { my %h := key-of(Int, 1); %h<x> = 1 }, X::TypeCheck::Binding::Parameter,
    expected => Int,
    'a hash keyed by a type capture rejects a key of another type';

is-deeply (try value-of(Int).of), Int,
    'a type capture is the value type of a hash declared with it';
throws-like { value-of(Int)<b> = "x" }, X::TypeCheck::Assignment,
    expected => Int,
    'a hash of a type capture rejects a value of another type';

is-deeply (try element-of(Int)), Array[Int].new(1),
    'a type capture is the element type of an array declared with it';
throws-like { element-of(Int).push("x") }, X::TypeCheck::Assignment,
    expected => Int, symbol => '@a',
    'an array of a type capture rejects an element of another type';

is-deeply (try with key-and-value(Int) { .keyof, .of }), (Int, Int),
    'a type capture is both the key and the value type of a hash';
is-deeply (try keyed-init(Int)), (my %{Int} = 1 => 2),
    'a hash keyed by a type capture takes its initializer';
is-deeply (try array-init(Int)), Array[Int].new(1, 2),
    'an array of a type capture takes its initializer';
is-deeply (try call-assign(Int)), (my %{Int} = 1 => 2),
    'a hash keyed by a type capture keeps the value of a .= initializer';
is-deeply (try with shaped(Int) { .of, .shape }), (Int, (2,)),
    'a shaped array of a type capture keeps its shape';

is-deeply (try explicit(Int).of), Int,
    'an array of a type capture with an explicit container type has the captured element type';
cmp-ok (try set-of(Int).WHAT), &[=:=], SetHash[Int],
    'an explicit container type parameterized by a type capture is instantiated';
cmp-ok (try definite-set(Int).WHAT), &[=:=], SetHash[Int],
    'a definite explicit container type parameterized by a type capture is instantiated';
cmp-ok (try definite-of(Int).WHAT), &[=:=], Array[Int:D],
    'an explicit container type with a definite type capture argument is instantiated';
cmp-ok (try definite-type(Int)), &[=:=], Array[Int:D],
    'a parameterization with a definite type capture argument is done where it is reached';
cmp-ok (try constant-set(Int).WHAT), &[=:=], SetHash[Int],
    'an explicit container type given by a constant parameterized by a type capture is instantiated';

is (try mixed(Int).greeting), 'hi',
    'a role mixed into an array of a type capture by a trait keeps its attribute default';
is-deeply (try mixed(Int).held), Int,
    'a role parameterized by a type capture and mixed in by a trait is instantiated';
is-deeply (try mixed-untyped(Int).held), Int,
    'a role parameterized by a type capture and mixed into an untyped array is instantiated';
is-deeply (try with ordered(Int) { .who, .of }), ('second', Int),
    'roles mixed into an array of a type capture by traits keep their order';
is-deeply (try with labeled(Int) { .label, .of }), ('x', Int),
    'a role attribute a trait set on an array of a type capture keeps its value';
is-deeply (try with counted(Int) { .count, .unit }), (9, 'items'),
    'native role attributes a trait set on an array of a type capture keep their values';
is-deeply (try with boxed(Int) { .content, .tag }), (Int, 'boxed'),
    'a role parameterized by a type capture and mixed in by a trait gets attributes of the captured type with their defaults';
try pushed(Int);
is-deeply (try with pushed(Int) { .List, .pushed-type }), ((1,), Int),
    'a role parameterized by a type capture and mixed in by a trait does not run its TWEAK again';
is-deeply (try entered-role(Int).entered), 'yes',
    'a role a will enter trait mixes into an array of a type capture is kept';
is-deeply (try dynamic-of(Int).VAR.dynamic), True,
    'an array of a type capture keeps the dynamic flag its trait set';

is-deeply (try with begun(Int) { .List, .[1] }), ((42,), Int),
    'an array with a type capture as its default keeps what a will begin trait put in it';
is-deeply (try with entered(Int) { .List, .[1] }), ((42,), Int),
    'an array with a type capture as its default keeps what a will enter trait put in it';
is-deeply (try skipped(Int)[0]), Int,
    'an array with a type capture as its default is instantiated when its declaration is not reached';
is-deeply (try loop-declared(Int)), (1, 1),
    'an array of a type capture declared in a loop is one container';
is-deeply (try hash-default(Int)<a>), Int,
    'a hash with a type capture as its default returns that type for a missing key';
is-deeply (try array-default(Int)[0]), Int,
    'an array with a type capture as its default returns that type for a missing element';

try counter(Int) for ^2;
is-deeply (try counter(Int)), 3,
    'a state array of a type capture keeps its elements across calls';
try seeded(Int) for ^2;
is-deeply (try seeded(Int)), 5,
    'a state array of a type capture runs its initializer on the first call only';
try reset-state(Int) for ^2;
is-deeply (try reset-state(Int)), 1,
    'a state array of a type capture is what an ENTER phaser of its block works on';
try restated(Int);
is-deeply (try restated(Int)), 4,
    'a redeclared state array of a type capture keeps its elements across calls';
try state-set(Int, $_) for 1, 2;
is-deeply (try state-set(Int, 3)), 3,
    'a state hash of an explicit container type parameterized by a type capture keeps its keys across calls';

is-deeply (try R[Int].new.keyed.keyof), Int,
    'a role type parameter is the key type of a hash declared in a method';
is-deeply (try Stash[Int].new.stash(1)), Array[Int].new(1),
    'a role type parameter is the element type of an array declared in the role body';

try R[Int].new.counted for ^2;
is-deeply (try R[Str].new.counted), 1,
    'a state array of a role type parameter belongs to its concretization';
is-deeply (try R[Int].new.counted), 3,
    'a state array of a role type parameter keeps its elements across calls';
