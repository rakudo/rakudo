unit module ThunkedRoutines;

constant X is export = ((my sub foo { 42 }; foo() + 1) xx 2).List;
constant W is export = * + (my sub bar { 1 }; bar())[1];
constant K is export = try (my sub baz { 7 }; baz())[1];

my $d is default(((my sub qux { 5 }; qux()) xx 2).List);
our sub trait-default() { $d.map(*[1]).List }

our sub left-of-xx($i) { (my sub foo { $i }) xx 2; foo() }
our sub nested-statements($i) { ((my sub foo { $i * 10 }; $i) xx 1); foo() }
our sub under-try($i) { try ((my sub foo { $i * 10 }; $i) xx 1); foo() }
our sub under-once($i) { once (my sub foo { $i * 10 }); foo() }
our sub topic-under-try() { 42 andthen try $_ + 1 }
our sub first-nested($y) { my $x = $y; FIRST (FIRST (my sub foo { $x }; 1); 1); foo() }
our sub begin-routine($y) { my $x = $y; BEGIN my sub foo { $x }; foo() }

role P[&c] { method pm { c() } }
class TraitRoutine does P[sub g { 42 }] { }
our sub trait-routine() { TraitRoutine.pm }
multi trait_mod:<will>(Routine:D $r, $block, :$prefix!) { $r.wrap(-> |c { $block() ~ callsame }) }
class WillMethod { method m will prefix { "p-" } { "m" } }
our sub will-method() { WillMethod.m }
our sub where-routine($v) { my Int $y where (sub ok($n) { $n > 0 })($_) = $v; $y }
our sub multi-xx($p) { (multi g(Int) { "i$p" }) xx 0; g(1) }
subset AStr of Str where /^a/;
our sub astr-begin() { BEGIN "ab" ~~ AStr }
