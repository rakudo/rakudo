# Provide full precision for all rational calculations by making all
# Rat FatRats.

my constant Rat is export = FatRat;
my sub infix:</>(Int:D \nu,Int:D \de) is export { FatRat.new(nu,de) }
