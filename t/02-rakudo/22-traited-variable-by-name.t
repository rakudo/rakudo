use Test;

plan 3;

# A variable trait applies through a Variable meta-object whose phaser
# helper (Variable.willdo) walks calling frames by name at run time, and
# a custom trait_mod can hold on to the variable's name and do the same.
# A traited declaration therefore must stay reachable as a by-name
# lexical rather than be lowered to a frame-local.

my constant loop-observed = class {};
my constant sub-observed = class {};
my @loop-seen;
my @sub-seen;
multi sub trait_mod:<does>(Variable:D $v, loop-observed) {
    $v.block.add_phaser: 'LEAVE', $v.willdo: -> \var { @loop-seen.push(var) };
}
multi sub trait_mod:<does>(Variable:D $v, sub-observed) {
    $v.block.add_phaser: 'LEAVE', $v.willdo: -> \var { @sub-seen.push(var) };
}

sub loop-traited() {
    for 1..3 {
        my $fh does loop-observed = $_ * 10;
    }
}
loop-traited();
is-deeply @loop-seen, [10, 20, 30],
    'the LEAVE phaser of each loop iteration receives that iteration value';

sub sub-traited() {
    my $fh does sub-observed = 42;
}
sub-traited();
is-deeply @sub-seen, [42],
    'a LEAVE phaser added by a custom does trait receives the traited variable';

my @will-seen;
sub will-traited() {
    my $x will leave { @will-seen.push($_) } = 66;
}
will-traited();
is-deeply @will-seen, [66],
    'a will leave phaser receives its variable';
