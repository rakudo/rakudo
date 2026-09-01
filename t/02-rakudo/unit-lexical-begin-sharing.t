use lib <t/02-rakudo/test-packages>;
use Test;

plan 16;

# A closure created by code run at BEGIN time resolves a unit lexical to
# the serialized container of the compiling unit. The unit frame runs
# once, so it declares its containers static instead of cloning them on
# first read, and the mainline and such closures share one container no
# matter which side touches it first.

my constant observed = class {};
my @seen;
multi sub trait_mod:<does>(Variable:D $v, observed) {
    $v.block.add_phaser: 'LEAVE', $v.willdo: -> \var { @seen.push(var) };
}
sub traited() { my $fh does observed = 42 }

my @bound := @seen;
traited();
is-deeply @seen, [42],
    'a phaser added by a variable trait reaches the unit array that a binding read first';
traited();
is-deeply @seen, [42, 42],
    'later phaser runs keep accumulating into the shared container';

# Each program compiles and runs on its own, so the read it names is the
# first read of the array in that unit.
sub seen-after(Str:D $read) {
    use MONKEY-SEE-NO-EVAL;
    EVAL Q:to/PROGRAM/.subst('READ', $read);
    my constant observed = class {};
    my @seen;
    multi sub trait_mod:<does>(Variable:D $v, observed) {
        $v.block.add_phaser: 'LEAVE', $v.willdo: -> \var { @seen.push(var) };
    }
    sub traited() { my $fh does observed = 42 }
    sub reader() { @seen.elems }
    READ
    traited();
    @seen
    PROGRAM
}
is-deeply seen-after('my @x := @seen;'), [42],
    'a phaser added by a variable trait reaches the array of a unit whose first read is a binding';
is-deeply seen-after('my $s = "@seen[]";'), [42],
    'a phaser added by a variable trait reaches the array of a unit whose first read is an interpolation';
is-deeply seen-after('reader();'), [42],
    'a phaser added by a variable trait reaches the array of a unit whose first read is in a nested sub';
is-deeply seen-after(''), [42],
    'a phaser added by a variable trait reaches the array of a unit that never read it first';

my $scalar = 5;
my &read-scalar = BEGIN { -> { $scalar } };
is read-scalar(), 5,
    'a closure created at BEGIN time reads the value the mainline assigned';

my @array;
my &push-array = BEGIN { -> { @array.push('begin') } };
@array.push('mainline');
push-array();
is-deeply @array, ['mainline', 'begin'],
    'a closure created at BEGIN time pushes into the array the mainline uses';

our @package;
my &push-package = BEGIN { -> { @package.push('begin') } };
@package.push('mainline');
push-package();
is-deeply @package, ['mainline', 'begin'],
    'a closure created at BEGIN time pushes into the array an our declaration names';
ok OUR::<@package> =:= @package,
    'the lexical of an our declaration is the package slot container';

my $assigned = 1;
BEGIN { $assigned = 2 }
is $assigned, 1,
    'a mainline assignment still overwrites a BEGIN time assignment';

my $unassigned;
BEGIN { $unassigned = 3 }
is $unassigned, 3,
    'a BEGIN time assignment still shows in a variable the mainline never assigns';

"aXa" ~~ /(X)/;
sub own-match() { $/ }
nok own-match().defined,
    'a routine match variable stays fresh after the mainline matches';
is ~$0, 'X',
    'the mainline match variable keeps the mainline match';

use UnitLexicalSharing;
is-deeply unit-seen(), [42],
    'a phaser added by a variable trait reaches an array in a unit scoped package body that a binding read first';
is unit-scalar(), 5,
    'a closure created at BEGIN time reads the value a unit scoped package body assigned';
