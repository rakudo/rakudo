unit module UnitLexicalSharing;

# The trait, the phaser it adds, and the lexicals they touch all live in
# the body of a unit scoped package, with a binding as the first read.

my constant observed = class {};
my @seen;
multi sub trait_mod:<does>(Variable:D $v, observed) {
    $v.block.add_phaser: 'LEAVE', $v.willdo: -> \var { @seen.push(var) };
}
sub traited() { my $fh does observed = 42 }

my $scalar = 5;
my &read-scalar = BEGIN { -> { $scalar } };

my @bound := @seen;

sub unit-seen() is export { traited(); @seen }
sub unit-scalar() is export { read-scalar() }
