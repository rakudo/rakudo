unit module MFlag::Fixture;

sub fixture-loaded() is export(:DEFAULT, :loaded) { 'MFlag::Fixture loaded' }

sub fixture-tagged() is export(:tagged) { 'MFlag::Fixture tagged' }

sub infix:<mflag>($a, $b) is export { "$a mflag $b" }
