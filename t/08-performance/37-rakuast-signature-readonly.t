use Test;
use nqp;
plan 23;

# The Signature meta-object carries a bit mask of its read only
# positionals. The invocation dispatcher reads the mask to pass such
# arguments outside their Scalar containers. These tests pin the mask
# across parameter shapes and the argument passing behavior the mask
# must not change.

sub mask(Mu $routine) {
    nqp::getattr_i(nqp::decont($routine.signature), Signature, '$!readonly')
}

sub two-plain($a, $b) { }
is mask(&two-plain), 3, 'two plain positionals set both bits';

sub first-rw($a is rw, $b) { }
is mask(&first-rw), 2, 'an rw positional does not set its bit';

sub first-raw(\a, $b) { }
is mask(&first-raw), 2, 'a raw positional does not set its bit';

sub first-copy($a is copy, $b) { }
is mask(&first-copy), 3, 'an is copy positional keeps its bit';

sub with-slurpy($a, *@rest) { }
is mask(&with-slurpy), 1, 'a slurpy positional sets no bit of its own';

sub with-named($a, :$n) { }
is mask(&with-named), 1, 'a named parameter sets no bit of its own';

sub with-capture(|c) { }
is mask(&with-capture), 0, 'a capture parameter sets no bit';

sub named-slurpy-only(*%h) { }
is mask(&named-slurpy-only), 0, 'a named slurpy alone leaves the mask empty';

sub optional-positional($a?) { }
is mask(&optional-positional), 1, 'an optional positional keeps its bit';

my class WithMethod {
    method m($a) { }
}
is mask(WithMethod.^lookup('m')), 3,
    'a method sets bits for the invocant and its positional';

multi sub multi-case($a, $b) { }
is mask(&multi-case.candidates[0]), 3,
    'a multi candidate signature carries the mask';

is mask(&infix:<+>.candidates.first({
    .signature.params == 2 && .signature.params[0].type =:= Int
})), 3, 'a setting operator candidate carries the mask';

is mask(/ y /), 1, 'an anonymous regex sets the invocant bit';

is mask({;$_}), 0, 'a bare block leaves its topic parameter unmasked';

is mask(-> $a { }), 1, 'a pointy block positional sets its bit';

my &wide = EVAL 'sub ('
    ~ (^66).map({ $_ == 3 ?? "\$p$_ is rw" !! "\$p$_" }).join(', ')
    ~ ') { }';
is mask(&wide), -9,
    'positionals beyond index 63 set no bits and rw still clears within range';

# The mask must not change what a callee observes.
sub sees-var($a) { $a.VAR.^name }
my $contained = 5;
is sees-var($contained), 'Scalar',
    'a read only parameter still sees the caller container through VAR';

multi sub multi-sees-var($a) { $a.VAR.^name }
is multi-sees-var($contained), 'Scalar',
    'a read only multi parameter still sees the caller container through VAR';

multi sub writes-back($a is rw) { $a = 42 }
my $target = 1;
writes-back($target);
is $target, 42, 'an rw multi parameter still writes back to the caller';

multi sub raw-sees-var(\a) { a.VAR.^name }
is raw-sees-var($contained), 'Scalar',
    'a raw multi parameter still receives the caller container';

multi sub copy-isolated($a is copy) { $a = 99 }
my $kept = 5;
copy-isolated($kept);
is $kept, 5, 'an is copy multi parameter does not write back to the caller';

multi sub resumed-write(Int $a is rw) { $a = 10; callsame }
multi sub resumed-write($a is rw) { $a = $a + 1 }
my $resumed = 1;
resumed-write($resumed);
is $resumed, 11, 'an rw parameter still writes back through a callsame resumption';

sub sees-proxy($a) { $a.VAR.^name }
my $proxy := Proxy.new(FETCH => -> $ { 5 }, STORE => -> $, $v { });
is sees-proxy($proxy), 'Scalar',
    'a Proxy argument still reaches the callee as a container';

# vim: expandtab shiftwidth=4
